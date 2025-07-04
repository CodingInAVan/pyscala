package transpiler

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.{should, shouldBe}

class IfElseTest extends AnyFunSuite with Matchers with TranspilerTestHelpers:
  test("Simple if statement") {
    val python =
      """
x = 5
if x > 3:
    y = 1
        """
    val scalaCode = transpile(toPythonCode(python))



    scalaCode shouldBe expected
  }

  test("If-else statement") {
    val python =
      """
a = 2
if a > 1:
    b = 10
else:
    b = 20
print(b)
        """
    val scalaCode = transpile(toPythonCode(python)).trim
    println(s"scalaCode: ${scalaCode}")

    val expected =
      """val a = 2
        |val b = if (a > 1) {
        |  10
        |} else {
        |  20
        |}""".stripMargin.trim

    scalaCode shouldBe expected
  }

  test("If-elif-else statement") {
    val python =
      """
n = 0
if n > 0:
    sign = 1
elif n == 0:
    sign = 0
else:
    sign = -1
        """
    val statements = transpile(toPythonCode(python)).split("\n")

    statements should contain("val n = 0")
    statements.exists(_.startsWith("if (n > 0) {")) shouldBe true
    statements.exists(_.contains("else if (n == 0) {")) shouldBe true
    statements.exists(_.contains("else {")) shouldBe true
    statements.exists(_.contains("val sign = 1")) shouldBe true
    statements.exists(_.contains("val sign = 0")) shouldBe true
    statements.exists(_.contains("val sign = -1")) shouldBe true
  }

  test("Nested if statement") {
    val python =
      """
score = 85
if score >= 60:
    if score >= 90:
        grade = "A"
    else:
        grade = "B"
else:
    grade = "F"
        """
    val statements = transpile(toPythonCode(python)).split("\n")

    statements should contain("val score = 85")
    statements.exists(_.contains("""if (score >= 60) {""")) shouldBe true
    statements.exists(_.contains("""if (score >= 90) {""")) shouldBe true
    statements.exists(_.contains("""else {""")) shouldBe true
    statements.exists(_.contains("val grade = \"A\"")) shouldBe true
    statements.exists(_.contains("val grade = \"B\"")) shouldBe true
    statements.exists(_.contains("val grade = \"F\"")) shouldBe true
  }

  test("If with boolean expression") {
    val python =
      """
x = True
if not x:
    y = False
        """
    val statements = transpile(toPythonCode(python)).split("\n")

    statements should contain("val x = true")
    statements.exists(_.startsWith("if (!x) {")) shouldBe true
    statements.exists(_.contains("val y = false")) shouldBe true
  }
