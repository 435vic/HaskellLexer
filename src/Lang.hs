{- |
Module      :  Lang
Description :  Language definitions for arithmetic programming language
Copyright   :  (c) Victor Quintana A01643020
License     :  MIT

Maintainer  :  A01643020@tec.mx

Language definitions for the arithmetic programming language described in
Activity 3.4 (TC2037.602)
-}
module Lang (module Lang) where

import Data.Set qualified as Set
import Parser
import Tokenizer
import Data.Char

lexerLowercase :: Set.Set Char
lexerLowercase = Set.fromList ['a' .. 'z']

lexerUppercase :: Set.Set Char
lexerUppercase = Set.fromList ['A' .. 'Z']

lexerNumbers :: Set.Set Char
lexerNumbers = Set.fromList ['0' .. '9']

lexerSymbols :: Set.Set Char
lexerSymbols = Set.fromList ['=', '+', '-', '*', '/', '^', ',', ';']

lexerWhitespace :: Set.Set Char
lexerWhitespace = Set.fromList [' ', '\t']

lexerLetters :: Set.Set Char
lexerLetters = lexerLowercase `Set.union` lexerUppercase

lexerAlphanumeric :: Set.Set Char
lexerAlphanumeric = lexerLetters `Set.union` lexerNumbers

lexerFullAlphabet :: Set.Set Char
lexerFullAlphabet = lexerAlphanumeric `Set.union` lexerSymbols `Set.union` lexerWhitespace

commentTransitions :: [((Int, Char), Int)]
commentTransitions = ((0, '/'), 1) : ((1, '/'), 2) : [((2, x), 2) | x <- Set.toList lexerFullAlphabet]

whitespaceTransitions :: [((Int, Char), Int)]
whitespaceTransitions = [((0, ' '), 1), ((0, '\t'), 1), ((1, ' '), 1), ((1, '\t'), 1)]

integerTransitions :: [((Int, Char), Int)]
integerTransitions =
    [((fro, x), to) | x <- Set.toList lexerNumbers, (fro, to) <- [(0, 1), (1, 1)]]

variableTransitions :: [((Int, Char), Int)]
variableTransitions =
    ((1, '_'), 1)
        : [((0, x), 1) | x <- Set.toList lexerLetters]
        ++ [((1, x), 1) | x <- Set.toList lexerAlphanumeric]

realTransitions :: [((Int, Char), Int)]
realTransitions =
    ((4, 'E'), 6)
        : ((4, 'e'), 6)
        : ((2, 'e'), 6)
        : [((fro, '.'), to) | (fro, to) <- [(0, 1), (2, 5)]]
        ++ [((6, x), 7) | x <- ['+', '-']]
        ++ [ ((fro, x), to)
           | x <- Set.toList lexerNumbers
           , (fro, to) <- numberTransitions
           ]
  where
    numberTransitions =
        [ (0, 2)
        , (2, 2)
        , (1, 4)
        , (5, 4)
        , (4, 4)
        , (6, 8)
        , (7, 8)
        , (8, 8)
        ]

plusTokenizer :: Tokenizer
plusTokenizer = tknrSingleChar '+' "PLUS" False

minusTokenizer :: Tokenizer
minusTokenizer = tknrSingleChar '-' "MINUS" False

divTokenizer :: Tokenizer
divTokenizer = tknrSingleChar '/' "DIV" False

multTokenizer :: Tokenizer
multTokenizer = tknrSingleChar '*' "MULT" False

expTokenizer :: Tokenizer
expTokenizer = tknrSingleChar '^' "EXP" False

assignTokenizer :: Tokenizer
assignTokenizer = tknrSingleChar '=' "ASSIGN" False

parenOpenTokenizer :: Tokenizer
parenOpenTokenizer = tknrSingleChar '(' "LPAREN" False

parenCloseTokenizer :: Tokenizer
parenCloseTokenizer = tknrSingleChar ')' "RPAREN" False

braceOpenTokenizer :: Tokenizer
braceOpenTokenizer = tknrSingleChar '{' "LBRACE" False

braceCloseTokenizer :: Tokenizer
braceCloseTokenizer = tknrSingleChar '}' "RBRACE" False

semicolonTokenizer  :: Tokenizer
semicolonTokenizer = tknrSingleChar ';' "SEMICOLON" False

whitespaceTokenizer :: Tokenizer
whitespaceTokenizer = tknrNew lexerWhitespace 0 Nothing [1] whitespaceTransitions "WHITESPACE" True

commentTokenizer :: Tokenizer
commentTokenizer = tknrNew lexerFullAlphabet 0 Nothing [2] commentTransitions "COMMENT" False

integerTokenizer :: Tokenizer
integerTokenizer =
    let integerAlphabet = lexerNumbers
     in tknrNew integerAlphabet 0 Nothing [1] integerTransitions "INTEGER" False

variableTokenizer :: Tokenizer
variableTokenizer =
    let variableAlphabet = lexerAlphanumeric `Set.union` Set.singleton '_'
     in tknrNew variableAlphabet 0 Nothing [1] variableTransitions "VARIABLE" False

realTokenizer :: Tokenizer
realTokenizer =
    let realAlphabet = lexerNumbers `Set.union` Set.fromList ['.', 'e', 'E', '+', '-']
     in tknrNew realAlphabet 0 Nothing [4, 5, 8] realTransitions "REAL" False

tokenizers :: [Tokenizer]
tokenizers =
    [ commentTokenizer
    , plusTokenizer
    , minusTokenizer
    , divTokenizer
    , multTokenizer
    , expTokenizer
    , assignTokenizer
    , semicolonTokenizer
    , parenOpenTokenizer
    , parenCloseTokenizer
    , braceOpenTokenizer
    , braceCloseTokenizer
    , variableTokenizer
    , realTokenizer
    , integerTokenizer
    , whitespaceTokenizer
    ]

-- While the existing tokenizer worked well to demonstrate the concept of a DFA-based tokenizer,
-- there were still some bugs with the implementation, and recognizing keywords was more
-- complicated than it should be. This simple implementation is enough to work with the parser.
newTokenize :: String -> Either TokenizeError [Token]
newTokenize str = newTokenize' str []

newTokenize' :: String -> [Token] -> Either TokenizeError [Token]
newTokenize' [] tokens = (Right . reverse . filter (\t -> tokenID t /= "WHITESPACE")) tokens
newTokenize' (c:cs) tokens
    | isSpace c = newTokenize' cs (newToken [c] "WHITESPACE" tokens : tokens)
    | c == '(' = newTokenize' cs (newToken [c] "LPAREN" tokens : tokens)
    | c == ')' = newTokenize' cs (newToken [c] "RPAREN" tokens : tokens)
    | c == '{' = newTokenize' cs (newToken [c] "LBRACE" tokens  : tokens)
    | c == '}' = newTokenize' cs (newToken [c] "RBRACE" tokens : tokens)
    | c == '+' = newTokenize' cs (newToken [c] "PLUS" tokens : tokens)
    | c == '-' = newTokenize' cs (newToken [c] "MINUS" tokens : tokens)
    | c == '*' = newTokenize' cs (newToken [c] "MULT" tokens : tokens)
    | c == '/' = newTokenize' cs (newToken [c] "DIV" tokens : tokens)
    | c == '^' = newTokenize' cs (newToken [c] "EXP" tokens : tokens)
    | c == '=' = newTokenize' cs (newToken [c] "ASSIGN" tokens : tokens)
    | c == ';' = newTokenize' cs (newToken [c] "SEMICOLON" tokens : tokens)
    | c == '/' && head cs == '/' = newTokenize' [] (newToken (c:cs) "COMMENT" tokens : tokens)
    | isAlpha c =
        let (word, rest) = span isAlphaNum cs
         in if c:word `elem` ["Entero", "Real"]
            then newTokenize' rest (newToken (c:word) "TYPE" tokens : tokens)
            else if c:word == "Programa"
                then newTokenize' rest (newToken (c:word) "PROGRAM" tokens : tokens)
                else if c:word == "principal" 
                    then newTokenize' rest (newToken (c:word) "MAIN" tokens : tokens)
                    else newTokenize' rest (newToken (c:word) "VARIABLE" tokens : tokens)
    | isDigit c || c == '.' =
        let (num, rest) = span (\x -> isDigit x || x `elem` ".eE") cs
         in if '.' `elem` num || 'e' `elem` num || 'E' `elem` num
            then newTokenize' rest (newToken (c:num) "REAL" tokens : tokens)
            else newTokenize' rest (newToken (c:num) "INTEGER" tokens : tokens)
    | otherwise = Right []
  where
    newToken :: String -> String -> [Token] -> Token
    newToken content tid [] = Token {
        tokenID = tid,
        tokenStart = 0,
        tokenEnd = length content,
        tokenContent = Nothing,
        tokenLine = Nothing
    }
    newToken content tid tkns = Token {
        tokenID = tid,
        tokenStart = tokenEnd (head tkns),
        tokenEnd = tokenEnd (head tkns) + length content,
        tokenContent = Nothing,
        tokenLine = Nothing
    }

grammar :: Grammar
grammar =
    Grammar
        { grammarStart = "program"
        , grammarRules =
            [ ("program", [T "PROGRAM", T "LBRACE", NT "method", T "RBRACE"])
            , ("method", [T "MAIN", T "LPAREN", T "RPAREN", T "LBRACE", NT "stmt", T "RBRACE"])
            , ("stmt", [T "TYPE", T "VARIABLE", T "ASSIGN", NT "expr", T "SEMICOLON", NT "stmt"])
            , ("stmt", [])
            , ("expr", [NT "term", NT "expr'"])
            , ("expr'", [T "PLUS", NT "term", NT "expr'"])
            , ("expr'", [T "MINUS", NT "term", NT "expr'"])
            , ("expr'", [])
            , ("term", [NT "fac", NT "term'"])
            , ("term'", [T "MULT", NT "fac", NT "term'"])
            , ("term'", [T "DIV", NT "fac", NT "term'"])
            , ("term'", [])
            , ("fac", [T "LPAREN", NT "expr", T "RPAREN"])
            , ("fac", [T "VARIABLE"])
            , ("fac", [T "INTEGER"])
            , ("fac", [T "PLUS", T "INTEGER"])
            , ("fac", [T "MINUS", T "INTEGER"])
            , ("fac", [T "REAL"])
            , ("fac", [T "PLUS", T "REAL"])
            , ("fac", [T "MINUS", T "REAL"])
            ]
        }
