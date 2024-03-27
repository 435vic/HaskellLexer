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

plusTokenizer :: Tokenizer
plusTokenizer = tknrSingleChar '+' "PLUS" False

minusTokenizer :: Tokenizer
minusTokenizer = tknrSingleChar '-' "MINUS" False

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
