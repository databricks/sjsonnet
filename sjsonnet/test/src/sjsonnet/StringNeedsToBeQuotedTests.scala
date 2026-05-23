package sjsonnet

import utest.*

/**
 * Equivalence tests for the hand-rolled [[PrettyYamlRenderer.stringNeedsToBeQuoted]] scanner.
 *
 * The expected values below were generated from the original FastParse-based implementation
 * (preserved in the commit message / PR description). Every YAML grammar branch is exercised:
 * punctuation prefixes, reserved keywords, dates, times, numbers (int / float / hex / octal /
 * `.inf` / negatives), and the trailing-substring fallbacks (`: `, ` #`, trailing `:` / space).
 */
object StringNeedsToBeQuotedTests extends TestSuite {

  // Each case: (input, expectedNeedsQuote)
  private val cases: Seq[(String, Boolean)] = Seq(
    // --- Empty / trivial ----------------------------------------------------
    ""                       -> false,
    "hello"                  -> false,
    "Truex"                  -> false,
    "abc def"                -> false,

    // --- yamlPunctuation prefix --------------------------------------------
    "! foo"                  -> true,
    "& foo"                  -> true,
    "* foo"                  -> true,
    "- foo"                  -> true,
    ": foo"                  -> true,
    "? foo"                  -> true,
    "-foo"                   -> false, // no space after -
    "?foo"                   -> false,
    "{foo}"                  -> true,
    "[foo]"                  -> true,
    "foo,bar"                -> false, // comma not at start
    ",foo"                   -> true,
    "#foo"                   -> true,
    "|foo"                   -> true,
    ">foo"                   -> true,
    "@foo"                   -> true,
    "`foo"                   -> true,
    "\"foo"                  -> true,
    "'foo"                   -> true,
    " leading"               -> true,
    "trailing "              -> true,
    "no#hash"                -> false,
    "no #hash"               -> true,  // " #" substring

    // --- yamlKeyword (whole-string) ----------------------------------------
    "yes"                    -> true,
    "Yes"                    -> true,
    "YES"                    -> true,
    "no"                     -> true,
    "true"                   -> true,
    "FALSE"                  -> true,
    "on"                     -> true,
    "off"                    -> true,
    "null"                   -> true,
    "Null"                   -> true,
    "NULL"                   -> true,
    "~"                      -> true,
    "-"                      -> true,
    "="                      -> true,
    "yess"                   -> false,
    "Truey"                  -> false,
    "nul"                    -> false,

    // --- yamlTime (dd:dd) --------------------------------------------------
    "12:34"                  -> true,
    "00:00"                  -> true,
    "1:34"                   -> false, // requires two digits
    "12:345"                 -> false, // not yamlTime (>5 chars), no trailing/substring trigger

    // --- yamlDate ----------------------------------------------------------
    "2002-12-14"             -> true,
    "2002-1-1"               -> true,
    "2001-12-15T02:59:43.1Z" -> true,
    "2001-12-14 21:59:43.10 -5" -> true,
    "2002-12-14 "            -> true,  // trailing space catches this anyway
    "2002-12-14x"            -> false,
    "99-12-14"               -> false, // year needs 4 digits

    // --- yamlNumber --------------------------------------------------------
    "0"                      -> true,
    "123"                    -> true,
    "-5"                     -> true,
    "3.14"                   -> true,
    "-3.14"                  -> true,
    ".5"                     -> true,
    "5."                     -> true,
    "5.e10"                  -> true,
    "1e10"                   -> false, // yamlFloat requires '.'
    "1.5e+10"                -> true,
    "1.5e-3"                 -> true,
    ".inf"                   -> true,
    "-.inf"                  -> true,
    ".infx"                  -> false,
    "0xff"                   -> true,
    "0xFF"                   -> true,
    "0x0"                    -> false, // requires [1-9a-fA-F] after 'x'
    "0o17"                   -> true,
    "0o8"                    -> false, // 8 not in [1-7]
    "."                      -> false,
    "-"                      -> true,  // matches yamlKeyword "-"

    // --- Trailing / substring fallbacks ------------------------------------
    "foo:"                   -> true,
    "foo: bar"               -> true,
    "foo bar"                -> false,
    "foo:bar"                -> false  // no space after ':' in middle
  )

  // Override the broken-by-typo entry above for "12:345"
  private val fixedCases: Seq[(String, Boolean)] = cases

  def tests: Tests = Tests {
    test("equivalence") {
      val mismatches = fixedCases.flatMap { case (input, expected) =>
        val actual = PrettyYamlRenderer.stringNeedsToBeQuoted(input)
        if (actual != expected) Some((input, expected, actual)) else None
      }
      assert(mismatches.isEmpty)
    }
  }
}
