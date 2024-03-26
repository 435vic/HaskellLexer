{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Lexer

import Data.Set qualified as Set

lexerLowercase = Set.fromList ['a' .. 'z']
lexerUppercase = Set.fromList ['A' .. 'Z']
lexerNumbers = Set.fromList ['0' .. '9']
lexerSymbols = Set.fromList ['=', '+', '-', '*', '/', '^']
lexerWhitespace = Set.fromList [' ', '\t']
lexerLetters = lexerLowercase `Set.union` lexerUppercase
lexerAlphanumeric = lexerLetters `Set.union` lexerNumbers
lexerFullAlphabet = lexerAlphanumeric `Set.union` lexerSymbols `Set.union` lexerWhitespace

commentTransitions = ((0, '/'), 1) : ((1, '/'), 2) : [((2, x), 2) | x <- Set.toList lexerFullAlphabet]

-- This would match any and all sequences of whitespace, as we don't really care how long is it
whitespaceTransitions = [((0, ' '), 1), ((0, '\t'), 1), ((1, ' '), 1), ((1, '\t'), 1)]

plusTokenizer = tknrSingleChar '+' "PLUS" False
minusTokenizer = tknrSingleChar '-' "MINUS" False
whitespaceTokenizer = tknrNew lexerWhitespace 0 Nothing [1] whitespaceTransitions "WHITESPACE" True
commentTokenizer = tknrNew lexerFullAlphabet 0 Nothing [2] commentTransitions "COMMENT" False

main :: IO ()
main = do
    -- print (dfaAccept dfaDivBy4 "10100")
    let result = tokenize [commentTokenizer, plusTokenizer, minusTokenizer, whitespaceTokenizer] "++--/ /+-"
    result `seq` print result