package ex1

import ex1.*
import ex1.Parsers.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFlatSpec with Matchers:

  private def parser: Parser[Char] =
    BasicParser(Set('a', 'b', 'c'))

  private def parserNE: Parser[Char] =
    NonEmptyParser(Set('0', '1'))

  private def parserNTC: Parser[Char] =
    NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))

  // Linearisation:
  // NonEmpty -> NotTwoConsecutive -> BasicParser -> Parser
  private def parserNTCNE: Parser[Char] =
    new BasicParser(Set('X', 'Y', 'Z'))
      with NotTwoConsecutive[Char]
      with NonEmpty[Char]

  private def sparser: Parser[Char] =
    "abc".charParser()

  "BasicParser" should "accept only configured characters, including empty input" in:
    parser.parseAll("aabc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false
    parser.parseAll("".toList) shouldBe true

  "NonEmptyParser" should "reject empty input" in:
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) shouldBe false
    parserNE.parseAll(List.empty) shouldBe false

  "NotTwoConsecutiveParser" should "reject equal consecutive characters" in:
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) shouldBe false
    parserNTC.parseAll("".toList) shouldBe true

  "Stacked parser (NotTwoConsecutive + NonEmpty)" should "combine both constraints" in:
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
    parserNTCNE.parseAll("".toList) shouldBe false

  "String extension parser" should "recognise characters from the string" in:
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) shouldBe false
    sparser.parseAll("".toList) shouldBe true