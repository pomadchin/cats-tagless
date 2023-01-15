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

package cats.tagless

import cats.tagless.syntax.all.*
import cats.tagless.macros.*

import cats.Id
import cats.arrow.FunctionK
import cats.data.Tuple2K
import scala.util.Try
import cats.~>

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.compiletime.testing.*

class ApplyKSpec extends AnyFunSpec with Matchers with Fixtures:
  describe("ApplyK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val applyK = Derive.applyK[SimpleService]
      applyK `shouldBe` a[ApplyK[SimpleService]]
    }

    it("ApplyK should be a valid instance for a simple algebra") {
      val functorK         = Derive.functorK[SimpleService]
      val applyK           = Derive.applyK[SimpleService]
      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Option(id)))

      val fk: Tuple2K[Id, Option, *] ~> Try = FunctionK.lift([X] => (tup: Tuple2K[Id, Option, X]) => Try(tup.second.map(_ => tup.first).get))
      val tryInstance                       = applyK.map2K[Id, Option, Try](instance, optionalInstance)(fk)

      tryInstance.id() `shouldBe` Try(instance.id())
      tryInstance.list(0) `shouldBe` Try(instance.list(0))
      tryInstance.paranthesless `shouldBe` Try(instance.paranthesless)
      tryInstance.tuple `shouldBe` Try(instance.tuple)
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.applyK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }

    it("ApplyK derives syntax") {
      val optionalInstance = instance.mapK(FunctionK.lift([X] => (id: Id[X]) => Option(id)))

      val fk: Tuple2K[Id, Option, *] ~> Try = FunctionK.lift([X] => (tup: Tuple2K[Id, Option, X]) => Try(tup.second.map(_ => tup.first).get))
      val tryInstance                       = instance.map2K(optionalInstance)(fk)

      tryInstance.id() `shouldBe` Try(instance.id())
      tryInstance.list(0) `shouldBe` Try(instance.list(0))
      tryInstance.paranthesless `shouldBe` Try(instance.paranthesless)
      tryInstance.tuple `shouldBe` Try(instance.tuple)
    }
  }
