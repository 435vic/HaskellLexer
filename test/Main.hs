module Main (main) where

import Lexer
import System.Exit qualified as Exit
import Test.HUnit
import Tokenizer

tokenizerTestCases :: [(Tokenizer, [(String, Bool)])]
tokenizerTestCases = [(integerTokenizer, integerTestCases), (commentTokenizer, commentTestCases)]
  where
    integerTestCases =
        [ ("+245", True)
        , ("681", True)
        , ("4", True)
        , ("12-2", False)
        , ("abc", False)
        , ("123.45", False)
        , ("0xFF", False)
        , ("0", True)
        ]
    commentTestCases =
        [ ("//*8asfhg", True)
        , ("safajhg25", False)
        , ("/ /sfa", False)
        , ("//", True)
        , ("/ ajsfjgjja", False)
        , ("// comment", True)
        , ("// // // //", True)
        ]

tknrDFATests :: (Tokenizer, [(String, Bool)]) -> Test
tknrDFATests (Tokenizer{tknrID = tkid, tknrDFA = dfa}, cases) =
    "DFA test for " ++ tkid ~: assertions
  where
    assertions =
        TestList [test $ dfaAccept dfa str == expected @? testID str expected | (str, expected) <- cases]
    testID str expected = if expected then "Accept " else "Reject " ++ str

dfaTests :: Test
dfaTests = "Tokenizer DFA tests" ~: TestList $ map tknrDFATests tokenizerTestCases

main :: IO ()
main = do
    result <- runTestTT dfaTests
    if errors result > 0 || failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess
