{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Piob.Grammar.GrammarTest
  ( spec_parse_grammar,
    spec_rules,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text qualified as AT
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified
import Debug.Trace (trace)
import Path (Abs, File, Path, toFilePath)
import Path.IO qualified
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    example,
    expectationFailure,
    it,
    pending,
    runIO,
    shouldBe,
  )
import Text.ABNF (Rule (Rule), canonicalizeRules, generateParser, parseDocument)
import Text.ABNF.ABNF.Parser (parseABNF)
import Text.ABNF.Document (Document)
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

parseComplete :: AT.Parser a -> Text -> AT.Result a
parseComplete parser input =
  case AT.parse parser input of
    AT.Partial cont -> cont Text.empty
    other -> other

parseCheck :: Rule -> Text -> Maybe Text
parseCheck rule example =
  case parseComplete (generateParser rule) example of
    fail@(AT.Fail {}) -> case AT.eitherResult fail of
      Right _ -> error "Unexpected"
      Left err -> Just . Text.pack $ err
    partial@(AT.Partial {}) -> error "Unexpected"
    AT.Done rem result ->
      if Text.null rem
        then Nothing
        else
          Just $
            "Failed ABNF rule \""
              <> ruleName rule
              <> "\":\n"
              <> "  Input     : \""
              <> example
              <> "\"\n"
              <> "  Problem   : Did not consume all input.\n"
              <> "  Remainder : \""
              <> rem
              <> "\""

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
      Just rule ->
        let parse_errs :: Maybe [Text]
            parse_errs = case mapMaybe (parseCheck rule) examples of
              [] -> Nothing
              errs -> Just errs
         in case parse_errs of
              Nothing -> pure ()
              Just errs ->
                let msg :: Text
                    msg =
                      Text.intercalate "\n" $
                        [ "The following errors were encountered:"
                        ]
                          <> errs
                 in expectationFailure (Text.unpack msg)

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
    chk "int-one-up" examplesIntOneUp
    chk "int-zero-up" examplesIntZeroUp
    chk "int" examplesIntValue
    chk "duration" examplesDuration
    chk "note-duration" examplesNoteDuration
    chk "dot-cut-shorthand" examplesDotCutShorthand
    chk "bar" examplesBar
    chk "tuplet-timing" examplesTupletTiming
    chk "beam" examplesBeam

examplesBeam :: [Text]
examplesBeam =
  [ "ABc",
    "d2e",
    "c/2d3/2",
    "g",
    "(3abc",
    "(3:2:2 G4c2",   -- only the spaces after a tuplet are allowed in a beam
    "(3:2:4 G2A2Bc",
    "d2d(3abc"
  ]

examplesTupletTiming :: [Text]
examplesTupletTiming =
  [ "(3:2:4  ", -- whitespace allowed following a tuplet
    "(2",
    "(3",
    "(4",
    "(5",
    "(6",
    "(7",
    "(8",
    "(9",
    "(3::",
    "(3:2",
    "(3:2:3",
    "(3::2",
    "(3:2:2",
    "(3:2:4"
  ]

examplesBar :: [Text]
examplesBar =
  [ "|",
    "|]",
    "||",
    "[|",
    "|:",
    ":|",
    "::",
    "|  " -- whitespace is permitted after a bar
  ]

examplesDotCutShorthand :: [Text]
examplesDotCutShorthand =
  [ "e4>f",
    "g8<e"
  ]

examplesNoteDuration :: [Text]
examplesNoteDuration =
  [ "e3/2",
    "f/2",
    "A4"
  ]

examplesDuration :: [Text]
examplesDuration =
  [ "1",
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

examplesIntZeroUp :: [Text]
examplesIntZeroUp = "0" : examplesIntOneUp

examplesIntOneUp :: [Text]
examplesIntOneUp =
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
