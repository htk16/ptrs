import org.specs2.mutable._
import org.htk.trs._

class ElementsSpec extends Specification {

  "Value(\"hoge\")" should {
    "equals Value(\"hoge\")" in {
      Value("hoge") equals Value("hoge") must beTrue
    }

    "doesn't equal Value(\"fuga\")" in {
      Value("hoge") equals Value("fuga") must beFalse
    }

    "doesn't equal Variable(\"hoge\")" in {
      Value("hoge") equals Variable("hoge") must beFalse
    }
  }

}

class NodeSpec extends Specification {

  implicit def stringToElement(s: String) =
    if (s.length > 0 && s.charAt(0).isUpper) Variable(s) else Value(s)
  implicit def stringToNode(s: String) = new Node(s)

  "foo" should {
    val foo = new Node("foo")
    val foo_alt = new Node("foo")
    val bar = new Node("bar")

    "equals foo" in {
      foo equals foo_alt must beTrue
    }

    "doesn't equal bar" in {
      foo equals bar must beFalse
    }

    "doesn't equal foo(bar, baz)" in {
      val foo_bar_baz = new Node("foo", List("bar", "baz"))
      foo equals foo_bar_baz must beFalse
    }

    "matches pattern foo" in {
      foo matchPattern foo_alt must beSome(Map())
    }

    "doesn't match pattern bar" in {
      foo matchPattern bar must beNone
    }

    "matches pattern X" in {
      foo matchPattern "X" must beSome(Map("X" -> foo))
    }
  }

  "foo(bar, baz)" should {
    val foo_bar_baz = new Node("foo", List("bar", "baz"))
    val foo_bar_baz_alt = new Node("foo", List("bar", "baz"))

    "equals foo(bar, baz)" in {
      foo_bar_baz equals foo_bar_baz_alt must beTrue
    }

    "matches pattern foo(bar, baz)" in {
      foo_bar_baz matchPattern foo_bar_baz_alt must beSome(Map())
    }

    "doesn't match pattern foo(bar)" in {
      foo_bar_baz matchPattern (new Node("foo", List("bar"))) must beNone
    }

    "matches pattern X" in {
      foo_bar_baz matchPattern "X" must beSome(Map("X" -> foo_bar_baz))
    }

    "matches pattern foo(A, B)" in {
      val foo_A_B = new Node("foo", List("A", "B"))
      foo_bar_baz matchPattern foo_A_B must beSome(
        Map("A" -> new Node("bar"),
          "B" -> new Node("baz")))
    }
  }
}



