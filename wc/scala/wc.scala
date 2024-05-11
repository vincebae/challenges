//> using dep info.picocli:picocli:4.7.5

import java.io.InputStream
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Files
import java.util.Optional
import java.util.concurrent.Callable
import picocli.CommandLine
import picocli.CommandLine.{Option as CommandLineOption, *}
import scala.collection.JavaConverters._
import scala.jdk.OptionConverters._

enum CountType:
  case Bytes, Chars, Words, Lines

val DefaultCountTypes = List(CountType.Chars, CountType.Words, CountType.Lines)

def bytes(text: String) = text.getBytes(StandardCharsets.UTF_8).length

def chars(text: String) = text.length

def words(text: String) = text.split("\\s+").length

def lines(text: String) = text.count(_ == '\n')

def count(text: String, countTypes: Seq[CountType]): Seq[Int] =
  for countType <- countTypes yield countType match
    case CountType.Bytes => bytes(text)
    case CountType.Chars => chars(text)
    case CountType.Words => words(text)
    case CountType.Lines => lines(text)

@Command(
  name = "word count",
  version = Array("0.0.1"),
  mixinStandardHelpOptions = true,
  description = Array("word count")
)
class WordCountApp(
    val defaultIn: InputStream = System.in,
    val out: PrintWriter = PrintWriter(System.out),
    val fileSystem: FileSystem = FileSystems.getDefault()
) extends Callable[Int] {

  @CommandLineOption(
    names = Array("-n", "--name"),
    description = Array("name")
  )
  private var name: Optional[String] = Optional.empty

  @CommandLineOption(
    names = Array("-l", "--line"),
    description = Array("show lines")
  )
  private var showLines: Boolean = true

  @Parameters(
    paramLabel = "filenames",
    description = Array("one or more files to archive")
  )
  private var filenames: Array[String] = Array()

  def call(): Int = {
    val bytes = if filenames.isEmpty
      then defaultIn.readAllBytes()
      else Files.readAllBytes(fileSystem.getPath(filenames(0)))
    val text = String(bytes, StandardCharsets.UTF_8);
    val counts = count(text, DefaultCountTypes)
    val result = counts.mkString(" ")
    out.println(result)
    0 // exit code
  }

}

@main def main(args: String*): Unit =
  System.exit(CommandLine(WordCountApp()).execute(args*))
