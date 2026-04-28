package ex1

import ex1.*
import ex1.Parsers.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFunSuite with Matchers:

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

  test("BasicParser accepts only configured characters, including empty input"):
    parser.parseAll("aabc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false
    parser.parseAll("".toList) shouldBe true

  test("NonEmptyParser rejects empty input"):
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) shouldBe false
    parserNE.parseAll(List.empty) shouldBe false

  test("NotTwoConsecutiveParser rejects equal consecutive characters"):
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) shouldBe false
    parserNTC.parseAll("".toList) shouldBe true

  test("NotTwoConsecutive and NonEmpty can be stacked together"):
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
    parserNTCNE.parseAll("".toList) shouldBe false

  test("String extension creates a parser for the string character set"):
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) shouldBe false
    sparser.parseAll("".toList) shouldBe true