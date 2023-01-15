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

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.compiletime.testing.*

class FunctorKSpec extends AnyFunSpec with Matchers with Fixtures:

  describe("FunctorK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val functorK = Derive.functorK[SimpleService]
      functorK `shouldBe` a[FunctorK[SimpleService]]
    }

    it("FunctorK should be a valid instance for a simple algebra") {
      val functorK = Derive.functorK[SimpleService]

      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Some(id)))

      optionalInstance.id() `shouldBe` Some(instance.id())
      optionalInstance.list(0) `shouldBe` Some(instance.list(0))
      optionalInstance.lists(0, 1) `shouldBe` Some(instance.lists(0, 1))
      optionalInstance.paranthesless `shouldBe` Some(instance.paranthesless)
      optionalInstance.tuple `shouldBe` Some(instance.tuple)
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.functorK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }

    it("FunctorK derives syntax") {
      val optionalInstance = instance.mapK(FunctionK.lift([X] => (id: Id[X]) => Some(id)))

      optionalInstance.id() `shouldBe` Some(instance.id())
      optionalInstance.list(0) `shouldBe` Some(instance.list(0))
      optionalInstance.lists(0, 1) `shouldBe` Some(instance.lists(0, 1))
      optionalInstance.paranthesless `shouldBe` Some(instance.paranthesless)
      optionalInstance.tuple `shouldBe` Some(instance.tuple)
    }
  }
