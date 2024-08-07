/*
 * Copyright 2019 cats-tagless maintainers
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.tagless.macros

import scala.annotation.experimental
import scala.quoted.*

private object DeriveMacros:
  // Unfortunately there is no flag for default parameters.
  private val defaultRegex = ".*\\$default\\$\\d+".r

@experimental
private class DeriveMacros[Q <: Quotes](using val q: Q):
  import quotes.reflect.*

  type Transform = PartialFunction[(Symbol, TypeRepr, Term), Term]
  type Combine = PartialFunction[(Symbol, TypeRepr, Seq[Term]), Term]

  final class ReplaceTerm(from: Term, to: Term) extends TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term =
      if tree == from then to else super.transformTerm(tree)(owner)

  private val nonOverridableOwners =
    TypeRepr.of[(Object, Any, AnyRef, AnyVal)].typeArgs.map(_.typeSymbol).toSet

  private val nonOverridableFlags =
    List(Flags.Final, Flags.Artifact, Flags.Synthetic, Flags.Mutable, Flags.Param)

  extension (xf: Transform)
    def transformRepeated(method: Symbol, tpe: TypeRepr, arg: Term): Tree =
      val x = Symbol.freshName("x")
      val resultType = xf(method, tpe, Select.unique(arg, "head")).tpe
      val lambdaType = MethodType(x :: Nil)(_ => tpe :: Nil, _ => resultType)
      val lambda = Lambda(method, lambdaType, (_, xs) => xf(method, tpe, xs.head.asExpr.asTerm))
      val result = Select.overloaded(arg, "map", resultType :: Nil, lambda :: Nil)
      val repeatedType = defn.RepeatedParamClass.typeRef.appliedTo(resultType)
      Typed(result, TypeTree.of(using repeatedType.asType))

    def transformArg(method: Symbol, paramAndArg: (Definition, Tree)): Tree =
      paramAndArg match
        case (param: ValDef, arg: Term) =>
          val paramType = param.tpt.tpe.widenParam
          if !xf.isDefinedAt(method, paramType, arg) then arg
          else if !param.tpt.tpe.isRepeated then xf(method, paramType, arg)
          else xf.transformRepeated(method, paramType, arg)
        case (_, arg) => arg

  extension (term: Term)
    def appliedToAll(argss: List[List[Tree]]): Term =
      argss.foldLeft(term): (term, args) =>
        val typeArgs = for case arg: TypeTree <- args yield arg
        val termArgs = for case arg: Term <- args yield arg
        if typeArgs.isEmpty then term.appliedToArgs(termArgs)
        else term.appliedToTypeTrees(typeArgs)

    def call(method: Symbol)(argss: List[List[Tree]]): Term = argss match
      case args1 :: args2 :: argss if !args1.exists(_.isExpr) && args2.forall(_.isExpr) =>
        val typeArgs = for case arg: TypeTree <- args1 yield arg.tpe
        val termArgs = for case arg: Term <- args2 yield arg
        Select.overloaded(term, method.name, typeArgs, termArgs).appliedToAll(argss)
      case args :: argss if !args.exists(_.isExpr) =>
        val typeArgs = for case arg: TypeTree <- args yield arg.tpe
        Select.overloaded(term, method.name, typeArgs, Nil).appliedToAll(argss)
      case args :: argss if args.forall(_.isExpr) =>
        val termArgs = for case arg: Term <- args yield arg
        Select.overloaded(term, method.name, Nil, termArgs).appliedToAll(argss)
      case argss =>
        term.select(method).appliedToAll(argss)

    def replace(from: Expr[?], to: Expr[?]): Term =
      ReplaceTerm(from.asTerm, to.asTerm).transformTerm(term)(Symbol.spliceOwner)

  extension (expr: Expr[?])
    def transformTo[A: Type](
        args: Transform = PartialFunction.empty,
        body: Transform = PartialFunction.empty
    ): Expr[A] =
      val term = expr.asTerm

      def transformDef(method: DefDef)(argss: List[List[Tree]]): Option[Term] =
        val sym = method.symbol
        val delegate = term.call(sym):
          for (params, xs) <- method.paramss.zip(argss)
          yield for paramAndArg <- params.params.zip(xs)
          yield args.transformArg(sym, paramAndArg)
        Some(body.applyOrElse((sym, method.returnTpt.tpe, delegate), _ => delegate))

      def transformVal(value: ValDef): Option[Term] =
        val sym = value.symbol
        val delegate = term.select(sym)
        Some(body.applyOrElse((sym, value.tpt.tpe, delegate), _ => delegate))

      Symbol.newClassOf[A](transformDef, transformVal)

  extension (exprs: Seq[Expr[?]])
    def combineTo[A: Type](
        args: Seq[Transform] = exprs.map(_ => PartialFunction.empty),
        body: Combine = PartialFunction.empty
    ): Expr[A] =
      val terms = exprs.map(_.asTerm)

      def combineDef(method: DefDef)(argss: List[List[Tree]]): Option[Term] =
        val sym = method.symbol
        val delegates = terms
          .lazyZip(args)
          .map: (term, xf) =>
            term.call(sym):
              for (params, args) <- method.paramss.zip(argss)
              yield for paramAndArg <- params.params.zip(args)
              yield xf.transformArg(sym, paramAndArg)
        Some(body.applyOrElse((sym, method.returnTpt.tpe, delegates), _ => delegates.head))

      def combineVal(value: ValDef): Option[Term] =
        val sym = value.symbol
        val delegates = terms.map(_.select(sym))
        Some(body.applyOrElse((sym, value.tpt.tpe, delegates), _ => delegates.head))

      Symbol.newClassOf[A](combineDef, combineVal)

  extension (sym: Symbol)
    def privateIn: Symbol =
      sym.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol)

    def overrideKeeping(flags: Flags*): Flags =
      flags.iterator.filter(sym.flags.is).foldLeft(Flags.Override)(_ | _)

    // TODO: Include type members.
    // TODO: Handle accessibility.
    def overridableMembers: List[Symbol] = for
      cls <- This(sym).tpe :: Nil
      member <- Iterator.concat(sym.methodMembers, sym.fieldMembers, sym.typeMembers)
      if !member.isNoSymbol
      if !member.isClassConstructor
      if !nonOverridableFlags.exists(member.flags.is)
      if !nonOverridableOwners.contains(member.owner)
      if !DeriveMacros.defaultRegex.matches(member.name)
    yield
      if member.isDefDef then
        val flags = member.overrideKeeping(Flags.ExtensionMethod, Flags.Infix)
        Symbol.newMethod(sym, member.name, cls.memberType(member), flags, sym.privateIn)
      else if member.isValDef then
        val flags = member.overrideKeeping(Flags.Lazy)
        Symbol.newVal(sym, member.name, cls.memberType(member), flags, sym.privateIn)
      else member

  extension (tpe: TypeRepr)
    def contains(that: TypeRepr): Boolean =
      tpe != tpe.substituteTypes(that.typeSymbol :: Nil, TypeRepr.of[Any] :: Nil)

    def containsAll(types: TypeRepr*): Boolean =
      types.forall(tpe.contains)

    def isRepeated: Boolean =
      tpe.typeSymbol == defn.RepeatedParamClass

    def isByName: Boolean = tpe match
      case ByNameType(_) => true
      case _ => false

    def bounds: TypeBounds = tpe match
      case bounds: TypeBounds => bounds
      case tpe => TypeBounds(tpe, tpe)

    def widenParam: TypeRepr =
      if tpe.isRepeated then tpe.typeArgs.head else tpe.widenByName

    def widenParamSeq: TypeRepr =
      if tpe.isRepeated then TypeRepr.of[Seq].appliedTo(tpe.typeArgs.head) else tpe.widenByName

    def summon: Term = Implicits.search(tpe) match
      case success: ImplicitSearchSuccess => success.tree
      case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)
      case _ => report.errorAndAbort(s"Not found: given ${tpe.show}")

    def lambda(args: List[Symbol]): TypeLambda =
      val n = args.length
      val names = args.map(_.name)
      def bounds(tl: TypeLambda) =
        val params = List.tabulate(n)(tl.param)
        args.map(_.info.substituteTypes(args, params).bounds)
      def result(tl: TypeLambda) =
        val params = List.tabulate(n)(tl.param)
        tpe.substituteTypes(args, params)
      TypeLambda(names, bounds, result)

    def summonLambda[T <: AnyKind: Type](arg: TypeRepr, args: TypeRepr*): Term =
      TypeRepr.of[T].appliedTo(tpe.lambda((arg :: args.toList).map(_.typeSymbol))).summon

    def summonOpt[T <: AnyKind: Type]: Option[Term] =
      Implicits.search(TypeRepr.of[T].appliedTo(tpe)) match
        case success: ImplicitSearchSuccess => Some(success.tree)
        case _ => None

  extension (symbol: SymbolModule)
    def newClassOf[T: Type](
        transformDef: DefDef => List[List[Tree]] => Option[Term],
        transformVal: ValDef => Option[Term]
    ): Expr[T] =
      val name = symbol.freshName("$anon")
      val parents = List(TypeTree.of[Object], TypeTree.of[T])
      val cls = symbol.newClass(symbol.spliceOwner, name, parents.map(_.tpe), _.overridableMembers, None)

      val members = cls.declarations
        .filterNot(_.isClassConstructor)
        .map: member =>
          member.tree match
            case method: DefDef => DefDef(member, transformDef(method))
            case value: ValDef => ValDef(member, transformVal(value))
            case _ => report.errorAndAbort(s"Not supported: $member in ${member.owner}")

      val newCls = New(TypeIdent(cls)).select(cls.primaryConstructor).appliedToNone
      Block(ClassDef(cls, parents, members) :: Nil, newCls).asExprOf[T]

end DeriveMacros
