#!/bin/sh
exec scala "$0" "$@"
!#

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


sealed trait LineType

case class DocLine(line: String) extends LineType

case class CodeLine(line: String) extends LineType

sealed trait ControlDirective extends LineType

object DocBegin extends ControlDirective
object DocEnd extends ControlDirective

case class Include(id: String) extends ControlDirective

case class DefBegin(id: String, lines: Seq[LineType]) extends ControlDirective {
  def +(line: LineType) = copy(lines = lines :+ line)
}

object DefEnd extends ControlDirective

object LineType{
  def apply(line: String) = {
    if (line.startsWith("// doc begin")) DocBegin
    else if (line.startsWith("// doc end")) DocEnd
    else if (line.startsWith("// def begin ")) DefBegin(line.drop(13), Seq())
    else if (line.startsWith("// include ")) Include(line.drop(11))
    else if (line.startsWith("// def end")) DefEnd
    else if (line.startsWith("// ")) DocLine(line.drop(3))
    else CodeLine(line)
  }
}

class CodeBlock(line: String) {
  private val lines = ArrayBuffer[String]()
  add(line)

  def add(line: String) = lines += line

  def end() {
    val pad = lines.filter(!_.isEmpty).map(_.prefixLength(_.isWhitespace)).min
    println("")
    println("```scala")
    lines.foreach(x => println(x.drop(pad)))
    println("```")
    println("")
  }
}

var block: CodeBlock = null

val defs = mutable.HashMap[String, DefBegin]()

def process(fileName: String) {
  val file = new File(fileName)
  val source = Source.fromFile(file)
  val lines = source.getLines()
  process(lines.toSeq.map(LineType.apply))
}

def process(lines: Traversable[LineType]) {
  lines.toSeq.:+(DocEnd).foldLeft[LineType](DocEnd)({
    // end
    case (DocEnd, DocBegin) => DocBegin
    case (DocEnd, CodeLine(_)) => DocEnd
    case (DocEnd, DocLine(_)) => DocEnd
    case (DocEnd, DocEnd) => DocEnd
    case (DocEnd, defBegin: DefBegin) => defBegin

    // begin
    case (DocBegin, doc@DocLine(line)) => {
      println(line)
      doc
    }
    case (DocBegin, code@CodeLine(line)) => {
      block = new CodeBlock(line)
      code
    }
    case (DocBegin, DocEnd) => DocEnd
    case (DocBegin, DocBegin) => DocBegin

    // doc
    case (doc@DocLine(_), DocBegin) => doc
    case (DocLine(_), DocEnd) => DocEnd
    case (DocLine(_), doc@DocLine(line)) => {
      println(line)
      doc
    }
    case (DocLine(_), code@CodeLine(line)) => {
      block = new CodeBlock(line)
      code
    }

    // code
    case (code@CodeLine(_), DocBegin) => code
    case (CodeLine(_), DocEnd) => {
      block.end()
      DocEnd
    }
    case (CodeLine(_), doc@DocLine(line)) => {
      block.end()
      println(line)
      doc
    }
    case (CodeLine(_), code@CodeLine(line)) => {
      block.add(line)
      code
    }

    case (defBegin: DefBegin, DocBegin) => defBegin + DocBegin
    case (defBegin: DefBegin, DocEnd) => defBegin + DocEnd
    case (defBegin: DefBegin, x: CodeLine) => defBegin + x
    case (defBegin: DefBegin, x: DocLine) => defBegin + x
    case (defBegin: DefBegin, DefEnd) => {
      defs.put(defBegin.id, defBegin)
      DocEnd
    }

    case (DocBegin, Include(id)) => defs.get(id) match {
      case Some(DefBegin(_, lines)) => {
        process(DocBegin +: lines :+ DocEnd)
        DocBegin
      }
      case None => throw new Exception(s"can't find $id to include")
    }
    case (docLine: DocLine, Include(id)) => defs.get(id) match {
      case Some(DefBegin(_, lines)) => {
        process(DocBegin +: lines :+ DocEnd)
        docLine
      }
      case None => throw new Exception(s"can't find $id to include")
    }
    case x => throw new MatchError(x)
  })
}

process("src/xtract/params.scala")
process("src/xtract/adapters.scala")
process("src/xtract/converters.scala")
process("src/xtract/fieldnamingconventions.scala")
process("src/xtract/instantiators.scala")
process("src/xtract/exceptions.scala")

process("test/xtract/docs/embedded/concrete/EmbeddedConcreteClassesSection.scala")
//process("docs/10_BriefIntroSection.scala")
//process("docs/20_ConfigurationOverviewSection.scala")
//process("docs/30_AdaptersSection.scala")
//process("docs/40_ConvertersSection.scala")
//process("docs/50_FieldNamingConventionsSection.scala")
//process(test, "InstantiatorsSection.scala")
//process(test, "ExceptionsSection.scala")