module Lexer (module Lexer) where

import Data.Set qualified as Set
import Tokenizer

lexerLowercase :: Set.Set Char
lexerLowercase = Set.fromList ['a' .. 'z']

lexerUppercase :: Set.Set Char
lexerUppercase = Set.fromList ['A' .. 'Z']

lexerNumbers :: Set.Set Char
lexerNumbers = Set.fromList ['0' .. '9']

lexerSymbols :: Set.Set Char
lexerSymbols = Set.fromList ['=', '+', '-', '*', '/', '^']

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
    ((0, '+'), 1)
        : ((0, '-'), 1)
        : [((fro, x), to) | x <- Set.toList lexerNumbers, (fro, to) <- [(1, 2), (0, 2), (2, 2)]]

variableTransitions :: [((Int, Char), Int)]
variableTransitions =
    ((1, '_'), 1)
        : [((0, x), 1) | x <- Set.toList lexerLetters]
        ++ [((1, x), 1) | x <- Set.toList lexerAlphanumeric]

realTransitions :: [((Int, Char), Int)]
realTransitions =
    ((4, 'E'), 6)
        : ((4, 'e'), 6)
        : [((fro, '.'), to) | (fro, to) <- [(0, 1), (2, 5)]]
        ++ [((fro, x), to) | x <- ['+', '-'], (fro, to) <- [(0, 3), (6, 7)]]
        ++ [ ((fro, x), to)
           | x <- Set.toList lexerNumbers
           , (fro, to) <- numberTransitions
           ]
  where
    numberTransitions =
        [ (0, 2)
        , (2, 2)
        , (3, 2)
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
divTokenizer = tknrSingleChar '/' "DIVISION" False

multTokenizer :: Tokenizer
multTokenizer = tknrSingleChar '*' "MULTIPLICATION" False

expTokenizer :: Tokenizer
expTokenizer = tknrSingleChar '^' "EXPONENTIATION" False

assignTokenizer :: Tokenizer
assignTokenizer = tknrSingleChar '=' "ASSIGNMENT" False

parenOpenTokenizer :: Tokenizer
parenOpenTokenizer = tknrSingleChar '(' "PAREN_OPEN" False

parenCloseTokenizer :: Tokenizer
parenCloseTokenizer = tknrSingleChar ')' "PAREN_CLOSE" False

whitespaceTokenizer :: Tokenizer
whitespaceTokenizer = tknrNew lexerWhitespace 0 Nothing [1] whitespaceTransitions "WHITESPACE" True

commentTokenizer :: Tokenizer
commentTokenizer = tknrNew lexerFullAlphabet 0 Nothing [2] commentTransitions "COMMENT" False

integerTokenizer :: Tokenizer
integerTokenizer =
    let
        integerAlphabet = lexerNumbers `Set.union` Set.fromList ['+', '-']
     in
        tknrNew integerAlphabet 0 Nothing [2] integerTransitions "INTEGER" False

variableTokenizer :: Tokenizer
variableTokenizer =
    let
        variableAlphabet = lexerAlphanumeric `Set.union` Set.singleton '_'
     in
        tknrNew variableAlphabet 0 Nothing [1] variableTransitions "VARIABLE" False

realTokenizer :: Tokenizer
realTokenizer =
    let
        realAlphabet = lexerNumbers `Set.union` Set.fromList ['.', 'e', 'E', '+', '-']
     in
        tknrNew realAlphabet 0 Nothing [4, 5, 8] realTransitions "REAL" False

arithmeticTokenizers :: [Tokenizer]
arithmeticTokenizers =
    [ commentTokenizer
    , plusTokenizer
    , minusTokenizer
    , divTokenizer
    , multTokenizer
    , expTokenizer
    , assignTokenizer
    , parenOpenTokenizer
    , parenCloseTokenizer
    , variableTokenizer
    , realTokenizer
    , integerTokenizer
    , whitespaceTokenizer
    ]
