module Main (main) where

import Lexer
import System.Exit qualified as Exit
import Test.HUnit
import Tokenizer

tokenizerTestCases :: [(Tokenizer, [(String, Bool)])]
tokenizerTestCases =
    [ (integerTokenizer, integerTestCases)
    , (commentTokenizer, commentTestCases)
    , (variableTokenizer, variableTestCases)
    , (realTokenizer, realTestCases)
    ]
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
    variableTestCases =
        [ ("_var", False)
        , ("Var", True)
        , ("var", True)
        , ("u78a7", True)
        , ("7u8a7", False)
        , (" ", False)
        , ("VARIABLE", True)
        ]
    realTestCases =
        [ ("+245.0", True)
        , ("681.0", True)
        , ("4.0", True)
        , ("12-2.0", False)
        , ("abc", False)
        , ("123.45", True)
        , ("0xFF", False)
        , ("0.0", True)
        , (".24", True)
        , ("24.", True)
        , ("24.0e-2", True)
        , ("24.0e+2", True)
        , ("24.0e2", True)
        , ("24.0e", False)
        , ("-24.0e+", False)
        , ("-2414", False)
        , ("9", False)
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
