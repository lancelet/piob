{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Piob.Grammar.GrammarTest
  ( spec_parse_grammar,
    spec_rules,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified
import Path (Abs, File, Path, toFilePath)
import Path.IO qualified
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    pending,
    runIO,
    shouldBe, example,
  )
import Text.ABNF (Rule (Rule), canonicalizeRules, parseDocument)
import Text.ABNF.ABNF.Parser (parseABNF)
import Text.Megaparsec (errorBundlePretty)

grammarPath :: FilePath
grammarPath = "data/grammar/grammar.abnf"

absoluteGrammarPath :: IO (Path Abs File)
absoluteGrammarPath = Path.IO.resolveFile' grammarPath

adaptNewlines :: Text -> Text
adaptNewlines = Text.intercalate "\r\n" . Text.lines

loadGrammar :: IO (Path Abs File, Text)
loadGrammar = do
  grammar_abs_file <- absoluteGrammarPath
  grammar <- Data.Text.IO.readFile . toFilePath $ grammar_abs_file
  pure (grammar_abs_file, adaptNewlines grammar <> "\r\n")

spec_parse_grammar :: Spec
spec_parse_grammar =
  describe "grammar.abnf" $ do
    it "can be parsed" $ do
      (grammar_abs_file, grammar) <- loadGrammar
      let maybeRules = parseABNF (toFilePath grammar_abs_file) grammar
      case maybeRules of
        Left err -> expectationFailure . errorBundlePretty $ err
        Right _ -> pure ()

ruleName :: Rule -> Text
ruleName (Rule name _ _) = name

ruleNameAndRule :: Rule -> (Text, Rule)
ruleNameAndRule rule = (ruleName rule, rule)

parseCheck :: Rule -> Text -> Maybe Text
parseCheck rule example =
  case parseDocument rule example of
    Left err -> Just . Text.pack $ err
    Right _ -> Nothing

parseCheckExamples ::
  [Rule] ->
  Text ->
  [Text] ->
  Spec
parseCheckExamples rules rule_name examples =
  it (Text.unpack rule_name) $ do
    case canonicalizeRules rule_name rules of
      Nothing ->
        expectationFailure $
          "Could not canonicalize rule: "
            <> Text.unpack rule_name
      Just rule -> mapM (parseCheck rule) examples `shouldBe` Nothing

spec_rules :: Spec
spec_rules = do
  describe "grammar.abnf rules" $ do
    (grammar_abs_file, grammar) <- runIO loadGrammar
    rule_list <- case parseABNF (toFilePath grammar_abs_file) grammar of
      Left err -> error . errorBundlePretty $ err
      Right p -> pure p
    let rules :: HashMap Text Rule
        rules = HashMap.fromList $ ruleNameAndRule <$> rule_list

        chk :: Text -> [Text] -> Spec
        chk = parseCheckExamples rule_list

    chk "note" examplesNote
    chk "embellishment" examplesEmbellishment
    chk "int-start" examplesIntStart
    chk "int-digit" examplesIntDigit
    chk "int" examplesIntValue
    chk "duration" examplesDuration
    chk "note-duration" examplesNoteDuration
    chk "dot-cut-shorthand" examplesDotCutShorthand

examplesDotCutShorthand :: [Text]
examplesDotCutShorthand =
  [
    "e4>f",
    "g8<e"
  ]

examplesNoteDuration :: [Text]
examplesNoteDuration =
  [
    "e3/2",
    "f/2",
    "A4"
  ]

examplesDuration :: [Text]
examplesDuration =
  [
    "1",
    "4",
    "/2",
    "3/2",
    "1/16",
    "/16"
  ]

examplesIntValue :: [Text]
examplesIntValue =
  [ "1",
    "10",
    "16"
  ]

examplesIntDigit :: [Text]
examplesIntDigit = "0" : examplesIntStart

examplesIntStart :: [Text]
examplesIntStart =
  [ "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9"
  ]

examplesNote :: [Text]
examplesNote =
  [ "G",
    "A",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "a"
  ]

examplesEmbellishment :: [Text]
examplesEmbellishment =
  [ "{g}",
    "{gdc}"
  ]