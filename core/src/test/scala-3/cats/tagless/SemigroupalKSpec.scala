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

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.compiletime.testing.*

class SemigroupalKSpec extends AnyFunSpec with Matchers with Fixtures:
  describe("SemigorupalK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val semigroupalK = Derive.semigroupalK[SimpleService]
      semigroupalK `shouldBe` a[SemigroupalK[SimpleService]]
    }

    it("SemigorupalK should be a valid instance for a simple algebra") {
      val functorK         = Derive.functorK[SimpleService]
      val semigroupalK     = Derive.semigroupalK[SimpleService]
      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Some(id)))
      val combinedInstance = semigroupalK.productK(instance, optionalInstance)

      combinedInstance.id() `shouldBe` Tuple2K(instance.id(), optionalInstance.id())
      combinedInstance.list(0) `shouldBe` Tuple2K(instance.list(0), optionalInstance.list(0))
      combinedInstance.lists(0, 1) `shouldBe` Tuple2K(instance.lists(0, 1), optionalInstance.lists(0, 1))
      combinedInstance.paranthesless `shouldBe` Tuple2K(instance.paranthesless, optionalInstance.paranthesless)
      combinedInstance.tuple `shouldBe` Tuple2K(instance.tuple, optionalInstance.tuple)
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.semigroupalK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }

    it("SemigroupalK derives syntax") {
      val optionalInstance = instance.mapK(FunctionK.lift([X] => (id: Id[X]) => Some(id)))
      val combinedInstance = instance.productK(optionalInstance)

      combinedInstance.id() `shouldBe` Tuple2K(instance.id(), optionalInstance.id())
      combinedInstance.list(0) `shouldBe` Tuple2K(instance.list(0), optionalInstance.list(0))
      combinedInstance.lists(0, 1) `shouldBe` Tuple2K(instance.lists(0, 1), optionalInstance.lists(0, 1))
      combinedInstance.paranthesless `shouldBe` Tuple2K(instance.paranthesless, optionalInstance.paranthesless)
      combinedInstance.tuple `shouldBe` Tuple2K(instance.tuple, optionalInstance.tuple)
    }
  }
