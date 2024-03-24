-- Activity 2.3 Arithmetic Lexer
-- Author: Victor Quintana - A01643020
-- DISCLAIMER: ChatGPT and Github Copilot were consulted along with other online sources
-- *The actual algorithm is my own design*, following programming patterns I found in online code
-- and as written by the LLMs consulted.
-- Most of my interactions with ChatGPT can be found in the following conversation link:
-- https://chat.openai.com/share/974e55de-ff5f-4de2-aae7-4b664a34edca

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace, traceStack)

-- The formal definition of a DFA (source: Wikipedia) is a 5-tuple (Q, Σ, δ, q0, F) consisting of:
-- 1. A finite set of states Q, stored as an array of generic type q
-- 2. A finite set of input symbols called the alphabet Σ, stored as an array of characters
-- 3. A transition function δ: Q × Σ → Q, stored as a hashmap
-- 4. An initial state q0 ∈ Q
-- 5. A set of states F that are designated as accepting states
data DFA = DFA {
    dfaAlphabet:: Set.Set Char,
    dfaTransitions:: Map.Map (Int, Char) Int,
    dfaStartState:: Int,
    dfaAcceptStates:: Set.Set Int
}

-- | This transition function, given a DFA, a state, and an input, looks up the DFA's transition
-- table for the next state.
dfaTransition:: DFA -> Int -> Char -> Maybe Int
dfaTransition dfa state x = Map.lookup (state, x) (dfaTransitions dfa)

-- | This function consumes the entire input string and determines if it is accepted.
dfaAccept:: DFA -> [Char] -> Bool
dfaAccept dfa = recurse (dfaStartState dfa)
    where
        recurse state [] = state `elem` dfaAcceptStates dfa
        recurse state (x:xs) = recurse (fromJust $ dfaTransition dfa state x) xs

-- | This utility function creates a DFA that accepts a single character.
dfaSingleChar:: Char -> DFA
dfaSingleChar c = DFA {
    dfaAlphabet = Set.fromList [c],
    dfaTransitions = Map.fromList [((0, c), 1), ((1, c), 2), ((2, c), 2)],
    dfaStartState = 0,
    dfaAcceptStates = Set.fromList [1]
}

-- | Tokenizer is an individual tokenizer. It detects a single type of token.
data Tokenizer = Tokenizer {
    tknrDFA:: DFA,
    tknrID:: String,
    tknrIgnore:: Bool,
    tknrState:: Maybe Int
}

instance Show Tokenizer where
    show tokenizer = "Tokenizer " ++ tknrID tokenizer ++ " state " ++ show (tknrState tokenizer)

-- | Token represents a parsed token from a string. It contains the ID, and the start (inclusive)
-- and end (exclusive) indices.
data Token = Token {
    tokenID:: String,
    tokenStart:: Int,
    tokenEnd:: Int
}

instance Show Token where
    show token = "Token " ++ tokenID token ++ " at (" ++
                 show (tokenStart token)++ ", " ++
                 show (tokenEnd token) ++ ")"

-- | Determine if a char is part of a tokenizer's defined alphabet.
tknrValidChar:: Tokenizer -> Char -> Bool
tknrValidChar tokenizer char = char `Set.member` dfaAlphabet (tknrDFA tokenizer)

-- | Determine if the tokenizer is currently in an accepted state.
tknrValidState:: Tokenizer -> Bool
tknrValidState tokenizer = let
    acceptStates = (dfaAcceptStates . tknrDFA) tokenizer
    state = tknrState tokenizer
    in fromMaybe (-1) state `Set.member` acceptStates

-- capture syntax and pattern matching! haskell is kinda cool, reminds me of rust
-- found this out in https://chat.openai.com/share/f115190a-2da0-475e-baf0-7cc44887bd6c
-- (the actual relevant information is at the end of the conversation)
-- | Calcualate the next state of a tokenizer given a char.
tknrStep :: Tokenizer -> Char -> Tokenizer
tknrStep tokenizer@Tokenizer { tknrState = Nothing, tknrDFA = dfa } char = tokenizer {
    tknrState = dfaTransition dfa (dfaStartState dfa) char
}
tknrStep tokenizer@Tokenizer { tknrState = Just state, tknrDFA = dfa } char = tokenizer {
    tknrState = dfaTransition dfa state char
}

-- Tokenize is a function that given a string and a list of DFAs will split it into tokens.
-- 1. It needs to store the current string
-- 2. It needs to store a list of parsed tokens
-- 3. For each DFA, we need to track the start index of each match (maybe not necessary if we consume the string)
-- 4. Once each DFA encounters a character that's not in the alphabet, we record whether the
--    string is accepted or not. At this point, if no other DFAs are active we split the string
--    at the current position and add the first half as a token to the list.
-- 5. IF multiple DFAs are active, we ignore the DFA that finished matching (maximal munch rule).
-- 6. If multiple DFAs finish matching at the same position, we choose the highest one on the
--    provided DFA list.

-- DFA match length means the current index of the string + 1

-- finished matching means tknrState is ACCEPTED and current char is not part of tokenizer alphabet
-- If top element (sort DFA by NON-ZERO match length) is finished matching 

-- inputString tokenizers tokens currentIndex
-- thing1242
--      ^
-- 012345678

-- +234
--  ^

_debugTokenize:: [Tokenizer] -> String -> [Token] -> Int -> Int -> String
_debugTokenize tokenizers str tokens start idx =
    "\n--------------\ntokenizer call:\n" ++
    "tokenizers: " ++ show tokenizers ++ "\n" ++
    "string: " ++ show str ++ "\n" ++
    "tokens: " ++ show tokens ++ "\n" ++
    "start: " ++ show start ++ "\n" ++
    "index: " ++ show idx ++ "\n"

_tokenizeInternal:: [Tokenizer] -> String -> [Token] -> Int -> Int -> [Token]
-- _tokenizeInternal a b c d e | trace (_debugTokenize a b c d e) False = undefined

-- base case: The string is fully consumed so we know everything was tokenized
_tokenizeInternal _ "" tokens _ _ = tokens
-- Recursive case: we update the tokenizers each level, checking for matches and consuming
-- substrings if a token is detected
_tokenizeInternal tokenizers inputString tokens currentStart currentIndex
    -- NO tokenizers are active: character is not valid (or we just started)
    | currentIndex /= 0 && null matches =
        error $ "Unexpected char '" ++ [prevChar] ++ "' at position " ++ show prevIndex
    -- We have at least one match (we choose the first)
    -- 'matches' contains all matching tokenizers, even if they aren't finished yet
    -- If the most important one isn't finished yet, we can discard the rest
    | not (null matches) && tknrFinishedMatching matchedTokenizer = let
        -- After a substring matches we can discard all the states of the tokenizers,
        -- they are not needed anymore
        newTokenizers = map (\tknr -> tknr { tknrState = Nothing }) tokenizers
        newToken = Token {
            tokenID = tknrID matchedTokenizer,
            tokenStart = currentStart,
            tokenEnd = currentStart+currentIndex
        }
        -- tokenizers have an ignore property because some tokens should be parsed but not
        -- necessarily displayed or considered (e.g whitespace)
        newTokens = if tknrIgnore matchedTokenizer then tokens else newToken : tokens
        in _tokenizeInternal newTokenizers suffix newTokens (currentStart+currentIndex) 0
    -- No tokenizers have finished a match
    -- We advance the currentIndex by one, calculating the next state for all tokenizers
    -- We should never reach this if endOfString is True
    | otherwise = _tokenizeInternal nextTokenizers inputString tokens currentStart (currentIndex + 1)
    where
        endOfString = currentIndex >= length inputString
        currentChar = inputString !! currentIndex
        prevIndex = currentIndex-1
        prevChar = inputString !! prevIndex
        -- Used for creating tokens
        (prefix, suffix) = splitAt currentIndex inputString
        -- Next iteration of tokenizers
        nextTokenizers = map (`tknrStep` currentChar) tokenizers
        -- This function determines if the current character is part of the specified
        -- tokenizer's alphabet. Of course, past the end of the string anything would be invalid.
        tknrValidNow tknr
            | endOfString = False
            | otherwise = let
                nextState = tknrState $ tknr `tknrStep` currentChar
                dfa = tknrDFA tknr
                in maybe False (`Set.member` dfaAcceptStates dfa) nextState
        -- A tokenizer has a complete match if it reaches an *accepted* state at the end of a
        -- *valid* string. Which means the valid string ends as soon as an invalid character
        -- appears in the input string.
        tknrFinishedMatching tknr = not (tknrValidNow tknr) && tknrValidState tknr
        tknrPartiallyMatching tknr = isJust $ tknrState tknr
        matches = filter tknrPartiallyMatching tokenizers
        matchedTokenizer = head matches

tokenize:: [Tokenizer] -> String -> [Token]
tokenize tokenizers inputString = _tokenizeInternal tokenizers inputString [] 0 0

--- Examples ---

-- dfaDivBy4:: DFA
-- dfaDivBy4 = DFA {
--     dfaAlphabet = ['0', '1'],
--     dfaTransitions = Map.fromList [
--         ((0, '0'), 1),
--         ((0, '1'), 0),
--         ((1, '0'), 2),
--         ((1, '1'), 0),
--         ((2, '0'), 2),
--         ((2, '1'), 0)
--     ],
--     dfaStartState = 0,
--     dfaAcceptStates = [2]
-- }

-------- Lexer Constants --------

lexerLowercase = Set.fromList ['a'..'z']
lexerUppercase = Set.fromList ['A'..'Z']
lexerNumbers = Set.fromList ['0'..'9']
lexerSymbols = Set.fromList ['=', '+', '-', '*', '/', '^']
lexerLetters = lexerLowercase `Set.union` lexerUppercase
lexerAlphanumeric = lexerLetters `Set.union` lexerNumbers
lexerFullAlphabet = lexerAlphanumeric `Set.union` lexerSymbols

----------  Comments  -----------

-- When state == 0, only the / charater transitions to


commentTransitions = map (\x -> ((2, x), 2)) (Set.toList lexerAlphanumeric) ++
    ((1, '/'), 2) : map (\x -> ((1, x), 2)) (Set.toList lexerAllExceptSlash) ++
    ((0, '/'), 1) : map (\x -> ((0, x), 1)) (Set.toList lexerAllExceptSlash)
    where
        lexerAllExceptSlash = lexerAlphanumeric `Set.difference` Set.singleton '/'


plusTokenizer = Tokenizer {
    tknrDFA = dfaSingleChar '+',
    tknrID = "PLUS",
    tknrIgnore = False,
    tknrState = Nothing
}

minusTokenizer = Tokenizer {
    tknrDFA = dfaSingleChar '-',
    tknrID = "MINUS",
    tknrIgnore = False,
    tknrState = Nothing
}

main:: IO ()
main = do
    -- print (dfaAccept dfaDivBy4 "10100")
    let result = tokenize [plusTokenizer, minusTokenizer] "-+--+-"
    result `seq` print result
