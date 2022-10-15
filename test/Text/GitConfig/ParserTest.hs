{-# LANGUAGE ScopedTypeVariables #-}

module Text.GitConfig.ParserTest where

import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.Hedgehog (testProperty)
import qualified Text.GitConfig.Parser as P
import Text.Megaparsec (parse)

genText :: Gen Text
genText = Gen.text (Range.linear 0 20) Gen.unicode

-- | Simple assert helper for HUnit tests on parse results
assertSuccess :: Either e a -> Assertion
assertSuccess res =
  case res of
    Left _ -> assertFailure ""
    Right _ -> return ()

assertFail :: Either e a -> Assertion
assertFail res =
  case res of
    Left _ -> return ()
    Right _ -> assertFailure ""

test_symbol :: TestTree
test_symbol = testProperty
  "symbol should parse the given text and consume trailing whitespace"
  $ property
  $ do
    t <- forAll genText
    case parse (P.symbol t) "" (t <> "  ") of
      Left _ -> pure ()
      Right t' -> t === t'

test_brackets :: TestTree
test_brackets = testProperty
  "brackets should parse the given text between '[' and ']' characters"
  $ property
  $ do
    t <- forAll genText
    let tBetweenBrackets = "[" <> t <> "]"
     in case parse (P.brackets (P.symbol t)) "" tBetweenBrackets of
          Left _ -> pure ()
          Right t' -> t === t'

test_quotes :: TestTree
test_quotes = testProperty
  "quotes should parse the given text between '\"' characters"
  $ property
  $ do
    t <- forAll genText
    let tBetweenQuotes = "\"" <> t <> "\""
     in case parse (P.quotes (P.symbol t)) "" tBetweenQuotes of
          Left _ -> pure ()
          Right t' -> t === t'

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

test_escSeq :: TestTree
test_escSeq =
  testGroup "ensure all valid escape sequences are parsed" $
    ["\"", "\\", "/", "n", "t", "r", "b", "f"] <&> \(t :: Text) ->
      let escaped = "\\" <> t
       in testCase (T.unpack $ "char " <> escaped) $
            assertSuccess $
              parse P.escSeq "" escaped

test_sectionName :: TestTree
test_sectionName =
  testGroup
    "sectionName parses both section and subsection names"
    [ testCase "section" $
        assertSuccess $
          parse P.sectionName "" "core",
      testCase "subSection" $
        assertSuccess $
          parse P.sectionName "" "core \"sub/+Section\""
    ]

test_sectionHeader :: TestTree
test_sectionHeader =
  testGroup
    "sectionHeader parses an entire section header"
    [ testCase "section" $
        assertSuccess $
          parse P.sectionHeader "" "[core]",
      testCase "subSection" $
        assertSuccess $
          parse P.sectionHeader "" "[core \"sub/+Section\"]"
    ]

test_variableName :: TestTree
test_variableName =
  testGroup
    "variableName parses a key in a key/val pair"
    [ testCase "valid" $
        assertSuccess $
          parse P.variableName "" "aSdF",
      testCase "fail start with nonalpha" $
        assertFail $
          parse P.variableName "" "+ASDF",
      testCase "pass with dash" $
        assertSuccess $
          parse P.variableName "" "foo-bar"
    ]

test_variableValue :: TestTree
test_variableValue =
  testGroup
    "variableValue parses the value in a key/value pair"
    [ testCase "valid" $
        assertSuccess $
          parse P.variableValue "" "foo\n",
      testCase "fail without eol" $
        assertFail $
          parse P.variableValue "" "foo"
    ]

test_mapping :: TestTree
test_mapping =
  testGroup
    "mapping parses entire key/value pairs"
    [ testCase "valid" $
        assertSuccess $
          parse P.mapping "" "key = value\n",
      testCase "empty '= value' equals boolean true" $
        case parse P.mapping "" "key\n" of
          Left _ -> assertFailure ""
          Right (_, v) ->
            assertEqual "" "true" v,
      testCase "empty value equals empty string" $
        case parse P.mapping "" "key = \n" of
          Left _ -> assertFailure ""
          Right (_, v) ->
            assertEqual "" "" v
    ]

test_section :: TestTree
test_section =
  testGroup
    "section parses entire sections in a git config"
    [ testCase "valid" $
        assertSuccess $
          parse P.section "" "[core]\nfoo = bar\n",
      testCase "valid multiple mappings" $
        assertSuccess $
          parse P.section "" "[core]\nfoo = bar\nbaz = quux\n",
      testCase "valid stop at one section" $
        case parse P.section "" "[core \"subsec\"]\nfoo = bar\n[next \"section\"]\nlol = hi\n" of
          Left _ -> assertFailure ""
          Right p ->
            assertEqual
              ""
              (P.Section ["core", "subsec"] (M.fromList [("foo", "bar")]))
              p
    ]

test_config :: TestTree
test_config =
  testGroup
    "config parses an entire git configuration"
    [ testCase "valid" $
        assertSuccess $
          parse P.config "" "[core]\nfoo=bar\n[remote \"origin\"]\nkey=val\n",
      testCase "invalid mapping without section" $
        assertFail $
          parse P.config "" "foo=bar\n[remote \"origin\"]\nkey=val\n"
    ]
