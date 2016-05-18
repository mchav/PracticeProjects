import java.io.IOException
import scala.annotation.tailrec
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafxml.core.{NoDependencyResolver, FXMLView}
import scalafx.event.ActionEvent
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.GridPane
import scalafxml.core.macros.sfxml

object UI extends JFXApp {
  val resource = getClass.getResource("scalculator.fxml")
  if (resource == null) {
    throw new IOException("Cannot load resource: scalculator.fxml")
  }

  val root = FXMLView(resource, NoDependencyResolver)

  stage = new PrimaryStage() {
    title = "Scalculator"
    scene = new Scene(root)
  }
}

object Main extends App {
  val operators = "+-*/^√"
  sealed trait Token { override def toString: String }
  case object TokEnd extends Token {
    override def toString() = "EOE" // End of Expression
  }
  case class TokOp(op: Operator) extends Token {
    override def toString() = op.toString
  }
  case class TokIdent(ident: String) extends Token {
    override def toString() = ident
  }
  case class TokNum(num: Int) extends Token {
    override def toString() = num.toString
  }
  case object TokSpace extends Token {
    override def toString() = " " 
  } 
  case object TokAssign extends Token {
    override def toString() = "="
  }
  case object TokLParen extends Token {
    override def toString() = "(" 
  } 
  case object TokRParen extends Token {
    override def toString() = ")" 
  }

  sealed trait Expression

  sealed trait Operator
  case object Plus extends Operator
  case object Minus extends Operator
  case object Times extends Operator
  case object Div extends Operator
  case object Exp extends Operator
  case object Sqrt extends Operator

  def operator(c: Char): Operator = c match {
    case '+' => Plus
    case '-' => Minus
    case '*' => Times
    case '/' => Div
    case '^' => Exp
    case '√' => Sqrt
    case _     => throw new IllegalArgumentException(c + " is not a valid operator.")
  }

  def opToChar(o: Operator): Char = o match {
    case Plus  => '+'
    case Minus => '-'
    case Times => '*'
    case Div   => '/'
    case Exp   => '^'
    case Sqrt  => '√'
  }

  def deSpace(tokens: Seq[Token]): Seq[Token] = tokens.filter(_ != TokSpace)

  def tokenise(s: String): Seq[Token] = {
    def identifier(c: Char, s: String) = {
      def alnums(str: String): (String, String) = {
        (str.takeWhile(_.isLetterOrDigit), str.dropWhile(_.isLetterOrDigit))
      }
      val (str, cs) = alnums(s)
      TokIdent(c +: str) +: tokenise(cs)
    }

    def number(c: Char, s: String) = {
      def digits(str: String): (String, String) = {
        (str.takeWhile(_.isDigit), str.dropWhile(_.isDigit))
      }
      val (str, cs) = digits(s)
      TokNum((c +: str).toInt) +: tokenise(cs)
    }

    s match {
      case "" => Seq()
      case _  => s.head match {
        case '='                       => TokAssign +: tokenise(s.tail)
        case '('                       => TokLParen +: tokenise(s.tail)
        case ')'                       => TokRParen +: tokenise(s.tail)
        case c if operators contains c => TokOp(operator(c)) +: tokenise(s.tail)
        case c if c.isDigit            => number(c, s.tail)
        case c if c.isLetter           => identifier(c, s.tail)
        case c if c.isWhitespace       => TokSpace +: tokenise(s.tail)
        case c                         => throw new IllegalArgumentException("Cannot tokenise " + c)
      }
    }
  }

  sealed trait Tree
  case class SumNode(op: Operator, left: Tree, right: Tree) extends Tree
  case class ProdNode(op: Operator, left: Tree, right: Tree) extends Tree
  case class AssignNode(left: String, right: Tree) extends Tree
  case class UnaryNode(op: Operator, tree: Tree) extends Tree
  case class NumNode(num: Double) extends Tree
  case class VarNode(variable: String) extends Tree

  def expression(tokens: Seq[Token]): (Tree, Seq[Token]) = {
    val (termTree, toks) = term(tokens)
    val next = lookAhead(toks)
    next match {
      case TokOp(op) if List(Plus, Minus) contains op => {
        val (exTree, otherToks) = expression(accept(toks))
        (SumNode(op, termTree, exTree), otherToks)
      }
      case TokAssign => termTree match {
        case VarNode(str) => {
          val (exTree, otherToks) = expression(accept(toks))
          (AssignNode(str, exTree), otherToks)
        }
        case _            => throw new IllegalArgumentException("Only variables can be assigned to.")
      }
      case _        => (termTree, toks)
    }
  }

  def factor(tokens: Seq[Token]): (Tree, Seq[Token]) = {
    val next = lookAhead(tokens)
    next match {
      case TokNum(x)     => (NumNode(x), accept(tokens))
      case TokIdent(str) => (VarNode(str), accept(tokens))
      case TokOp(op) if List(Plus, Minus, Sqrt) contains op => {
        val (factTree, toks) = expression(accept(tokens))
        (UnaryNode(op, factTree), toks)
      }
      case TokLParen     => {
        val (expTree, toks) = expression(accept(tokens))
        val look = lookAhead(toks)
        if (look != TokRParen) throw new IllegalArgumentException("Missing right parentheses.") 
        else (expTree, accept(toks))
      }
      case _             => throw new IllegalArgumentException("Parse error on token " + next)
    }
  }
  def term(tokens: Seq[Token]): (Tree, Seq[Token]) = {
    val (factTree, toks) = factor(tokens)
    val next = lookAhead(toks)
    next match {
      case TokOp(op) if List(Times, Div, Exp) contains op => {
        val (termTree, otherToks) = term(accept(toks))
        (ProdNode(op, factTree, termTree), otherToks)
      }
      case _ => (factTree, toks)
    }
  }

  def lookAhead(tokens: Seq[Token]): Token = tokens match {
    case Nil => TokEnd
    case _     => tokens.head
  }

  def accept(tokens: Seq[Token]): Seq[Token] = tokens match {
    case Nil => throw new IllegalArgumentException("Nothing to accept")
    case _     => tokens.tail
  }

  def parse(tokens: Seq[Token]): Tree = {
    val (tree, toks) = expression(tokens)
    /*toks match {
      case Nil => tree
      case _     => throw new IllegalArgumentException("Left over tokens " + toks.toString)
    }*/
    tree
  }
  def evaluate(parsed: Tree): Double = {
    parsed match {
      case SumNode(op, left, right) => {
        val lft = evaluate(left)
        val rgt = evaluate(right)
        op match {
          case Plus  => lft + rgt
          case Minus => lft - rgt
          case _     => ???
        }
      }
      case ProdNode(op, left, right) => {
        val lft = evaluate(left)
        val rgt = evaluate(right)
        op match {
          case Times  => lft * rgt
          case Div    => lft / rgt
          case Exp    => Math.pow(lft, rgt)
          case _      => ???
        }
      }
      case UnaryNode(op, tree) => {
        val x = evaluate(tree)
        op match {
          case Plus  => x
          case Minus => -1 * x
          case Sqrt  => Math.pow(x, 0.5)
          case _     => ???
        }
      }
      case NumNode(x) => x
      case _          => throw new IllegalArgumentException("Not yet implemented")
    }
  }
  println(evaluate(parse(deSpace(tokenise("√4")))))
}