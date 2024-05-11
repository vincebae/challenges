//> using test.dep com.google.jimfs:jimfs:1.3.0
//> using test.dep org.scalatest::scalatest::3.2.18
//> using dep info.picocli:picocli:4.7.5

import CountType._
import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.io.PrintWriter
import java.io.StringWriter
import java.nio.charset.StandardCharsets
import java.nio.file.FileSystem
import java.nio.file.Files
import java.nio.file.Path
import org.scalatest._
import org.scalatest.featurespec._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import picocli.CommandLine
import scala.compiletime.uninitialized

val TestInput = """This is a test input for
    |word count.
    |Hello, world!
    |""".stripMargin

val TestInput2 = """This is a test input with unicode
    |안녕하세요. 반갑습니다!
    |또 만나요!
    |""".stripMargin

/*
 * Unit tests for {@link WordCount}.
 */
class WordCountTest extends AnyFlatSpec {

  "bytes(TestInput)" should "be 51" in {
    bytes(TestInput) shouldBe 51
  }

  "chars(TestInput)" should "be 51" in {
    chars(TestInput) shouldBe 51
  }

  "words(TestInput)" should "be 10" in {
    words(TestInput) shouldBe 10
  }

  "lines(TestInput)" should "be 3" in {
    lines(TestInput) shouldBe 3
  }

  "bytes(TestInput2)" should "be 83" in {
    bytes(TestInput2) shouldBe 83
  }

  "chars(TestInput2)" should "be 55" in {
    chars(TestInput2) shouldBe 55
  }

  "words(TestInput2)" should "be 11" in {
    words(TestInput2) shouldBe 11
  }

  "lines(TestInput2)" should "be 3" in {
    lines(TestInput2) shouldBe 3
  }

  "count for CountType.Bytes" should "contain bytes only " in {
    val actual = count(TestInput, Seq(Bytes))
    actual shouldBe Seq(bytes(TestInput))
  }

  "count for CountType.Chars" should "contain chars only " in {
    val actual = count(TestInput, Seq(Chars))
    actual shouldBe Seq(chars(TestInput))
  }

  "count for CountType.Words" should "contain words only " in {
    val actual = count(TestInput, Seq(Words))
    actual shouldBe Seq(words(TestInput))
  }

  "count for CountType.Lines" should "contain lines only " in {
    val actual = count(TestInput, Seq(Lines))
    actual shouldBe Seq(lines(TestInput))
  }

  "count for all count types" should "contain bytes, chars, words and lines in order" in {
    val allCountTypes = Seq(Bytes, Chars, Words, Lines)
    val actual = count(TestInput, allCountTypes)
    actual shouldBe Seq(51, 51, 10, 3)
  }

}

/*
 * End-to-end test for CLI application
 */
class WordCountAppTest
    extends AnyFeatureSpec
    with GivenWhenThen
    with BeforeAndAfterEach {

  val HomeDir = "/home/user"

  // Variables to be initialized in beforeEach()
  var defaultIn: InputStream = uninitialized
  var stringWriter: StringWriter = uninitialized
  var fileSystem: FileSystem = uninitialized
  var commandLine: CommandLine = uninitialized

  override def beforeEach(): Unit = {
    defaultIn = ByteArrayInputStream(TestInput.getBytes(StandardCharsets.UTF_8))
    stringWriter = StringWriter()
    fileSystem = Jimfs.newFileSystem(
      Configuration.unix().toBuilder().setWorkingDirectory(HomeDir).build()
    )
    commandLine = CommandLine(
      WordCountApp(defaultIn, PrintWriter(stringWriter), fileSystem)
    )
  }

  info("As a application user")
  info("I want to execute the command line")

  feature("Execute command line") {
    scenario("User execute without any command-line argument") {
      Given("no prerequisite")

      When("the application is executed without arguments")
      val exitCode = commandLine.execute();

      Then(
        """exit code should be 0 and 
        counts for chars, words and lines from the default input should be written to output"""
      )
      exitCode should be(0)
      stringWriter.toString should be("51 10 3\n")
    }

    scenario("User execute with filename") {
      Given("the file with the filename exists in the filesystem")
      val filename = "input.txt"
      val path = fileSystem.getPath(HomeDir).resolve(filename)
      Files.writeString(path, TestInput2)

      When("the application is executed with the filename")
      val exitCode = commandLine.execute(filename)

      Then(
        """exit code should be 0 and
        counts for chars, words and lines of the file should be written to output"""
      )
      exitCode should be(0)
      stringWriter.toString should be("55 11 3\n")
    }

  }

}
