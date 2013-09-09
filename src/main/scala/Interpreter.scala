package org.htk.trs
import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader, CharSequenceReader}

object Interpreter {
  def main(args: Array[String]) {
    // @todo 入力の読み込み処理を見直す
    val inputs = Source.fromFile("test.trs").toArray
    val reader = new CharSequenceReader(inputs)
    val rewriter = new Rewriter(Nil)
    interpret(reader, rewriter)
  }

  def interpret(reader: Reader[Char], rewriter: Rewriter) {
    if (reader.atEnd) return

    val (command, rest) = Parser.parseCommand(reader)
    println("command: %s".format(command))
    command match {
      case Exit() => return
      case Add(rule) => interpret(rest, rewriter ++ rule)
      case Reduce(target) => {
        println(rewriter.reduce(target))
        interpret(rest, rewriter)
      }
    }
  }
}

/*

 項表現の文法

 Node ::= Label { Children }
 Lable ::= value | variable
 Children ::= '(' Node {',' Node} ')'
 Rule :: Node '->' Node

 Command の文法

 Command ::= Exit | Add | Reduce
 Exit ::= ":exit"
 Add ::= ":add" + Rule
 Reduce ::= [":reduce"] + Node
 
*/
object Parser extends JavaTokenParsers {
  def parseTerm(input: String): Node = parseAll(node, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  type Result = (Command, Reader[Char])
  def parseCommand(input: Reader[Char]): Result = parse(command, input) match {
    case Success(result, rest) => (result, rest)
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  // 項表現
  def node: Parser[Node] =
    element ~ opt(children) ^^ { r => new Node(r._1, r._2 getOrElse List()) }
  
  def element: Parser[Element] = 
    """[a-z0-9]+""".r ^^ { Value(_) } |
    """[A-Z][a-zA-Z-9]*""".r ^^ { Variable(_) }

  def children: Parser[List[Node]] = "(" ~> repsep(node, ",") <~ ")"

  def rule: Parser[Rule] = (node <~ "->") ~ node ^^ { r => new Rule(r._1, r._2) }

  // コマンド
  def command: Parser[Command] = exit | add | reduce
  def exit: Parser[Command] = ":exit" ^^ { _ => Exit() }
  def add: Parser[Command] = ":add" ~> rule ^^ { Add(_) }
  def reduce: Parser[Command] = opt(":reduce") ~> node ^^ { Reduce(_) }
}

// インタプリタコマンド
trait Command
case class Exit() extends Command
case class Add(val rule: Rule) extends Command
case class Reduce(val term: Node) extends Command

// @todo children を List[Node] でなく Vector[Node] にする
class Node(val value: Element, val children: List[Node]) {
  def length: Int = { children.length }
  def isLeaf(): Boolean = { length == 0 }
  def isNode(): Boolean = { !isLeaf }

  type Substitution = Map[String, Node]

  def this(value: Element) { this(value, Nil) }

  override def toString(): String =
    if (children.length == 0)
      value.toString
    else
      "%s(%s)".format(value.toString, children.mkString(", "))


  override def equals(that: Any): Boolean = that match {
    case n: Node => {
      if (value != n.value || length != n.length)
        false
      else
        (children zip n.children) forall (t => t._1 == t._2)
    }
    case _ => false
  }

  // @todo 同名の変数が複数存在する場合の対応を入れる
  def matchPattern(pattern: Node): Option[Substitution] = {
    def matchChildren(
      nodes: Traversable[(Node, Node)],
      subs: Option[Substitution]): Option[Substitution] = nodes match {
      case (t, p)#::tail =>
        for (s <- subs;
          sh <- t.matchPattern(p);
          st <- matchChildren(tail, Some(s ++ sh))) yield st
      case _ => subs
    }

    pattern.value match {
      case v: Value =>
        if (value == v && length == pattern.length)
          matchChildren(children.toStream zip pattern.children.toStream, Some(Map()))
        else
          None
      case Variable(n) => Some(Map(n -> this))
      case _ => None
    }
  }

  def matchRule(rule: Rule): Option[Substitution] = matchPattern(rule.lhs)

  def construct(subs: Substitution): Option[Node] = value match {
    case _: Value => {
      val cs = children.toStream.map(_.construct(subs))
      if (cs.exists(_.isEmpty))
        None
      else
        // @todo get を使わないように書きなおす
        Some(new Node(value, cs.map(_.get).toList))
    }
    case Variable(n) => subs.get(n)
    case _ => None
  }
}

// @todo 適切なクラス名を与える
abstract class Element
case class Value(name: String) extends Element {
  override def toString() = name
}
case class Variable(name: String) extends Element {
  // @todo Value, Variable で toString の処理を共有する
  override def toString() = name
}

class Rule(val lhs: Node, val rhs: Node) {
  override def toString = "%s -> %s".format(lhs.toString, rhs.toString)

  // @todo メソッド名を見直す
  def matchTarget(target: Node): Option[Node#Substitution] = {
    target.matchPattern(lhs)
  }

  def rewrite(target: Node): Option[Node] = {
    val subs = target matchRule this
    subs.flatMap(s => rhs.construct(s))
  }
}

class Rewriter(rules: List[Rule]) {
  def ++(rule: Rule): Rewriter = new Rewriter(rule :: rules)

  def rewrite(target: Node): Option[Node] = 
    rules.toStream.map(_.rewrite(target)).find(_.isDefined) getOrElse None

  def reduce(target: Node): Node = reduceOneStep(target) match {
    case Some(n) => reduce(n)
    case None => target
  }

  // @todo マッチングの結果や書き換え結果をメモ化する
  def reduceOneStep(target: Node): Option[Node] = {
    val rewrited_children = target.children.map(reduceOneStep _)
    if ((target.children.length > 0) && (rewrited_children.exists(c => c.isDefined)))
      Some(new Node(target.value,
        rewrited_children.zip(target.children).map(t => t._1 getOrElse t._2)))
    else
      // target の子は書き換えられなかったので，自身の書き換えを試みる
      rewrite(target)
  }
}
