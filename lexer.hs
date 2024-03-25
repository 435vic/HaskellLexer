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
import Data.List (nub)
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

-- | Tokenizer is an individual tokenizer. It detects a single type of token.
-- It's built from a DFA object and a string identifier. The `tknrIgnore` option
-- provides a way to add tokenizers that while needed, should not necessarily be
-- displayed along with other tokens.
data Tokenizer = Tokenizer {
    tknrDFA:: DFA,
    tknrID:: String,
    tknrIgnore:: Bool,
    tknrDeadStates:: Set.Set Int,
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

-- | Create a new tokenizer with a DFA, String ID and whether to ignore its tokens when tokenizing.
tknrNew:: Set.Set Char -> Int -> [Int] -> [Int] -> [((Int, Char), Int)] -> String -> Bool -> Tokenizer
tknrNew alphabet startState acceptStates deadStates transitionList tkid ignore = Tokenizer {
    tknrDFA = verifiedDFA,
    tknrID = tkid,
    tknrIgnore = ignore,
    tknrDeadStates = Set.fromList deadStates,
    tknrState = Nothing
} where
    verifiedDFA
        | invalidChars =
            error ("Invalid transition characters " ++ show invalidCharList ++ " for tokenizer " ++ tkid)
        | invalidTransitions =
            error ("Invalid transitions " ++
                   show invalidStateTransitions ++
                   " to non-specified state for tokenizer " ++ tkid)
        | invalidStates = error ("Not all transitions defined for states for tokenizer " ++ tkid)
        | otherwise = DFA {
            dfaAlphabet = alphabet,
            dfaAcceptStates = Set.fromList acceptStates,
            dfaTransitions = transitions,
            dfaStartState = startState
        }

    transitions = Map.fromList transitionList
    -- Valid chars: All transitions must specify chars that are part of the alphabet
    invalidChars = not $ null invalidCharList
    invalidCharList = [x | ((_, x), _) <- transitionList, x `Set.notMember` alphabet]
    -- Valid transitions: All states transitioned to must exist (i.e) have at least one entry
    -- where the state exists as a source state
    invalidTransitions = not $ null invalidStateTransitions
    fromStates = [q | ((q, _), _) <- transitionList]
    invalidStateTransitions = [((qf, x), qt) | ((qf, x), qt) <- transitionList, qt `notElem` fromStates]
    -- Valid states: for each state there must be one and only one transition for ALL elements
    -- of the alphabet
    invalidStates = not $ null invalidStatesList
    invalidStatesList = [q | q <- nub fromStates, c <- Set.toList alphabet, (q, c) `Map.notMember` transitions]


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
tknrStep tokenizer@Tokenizer { tknrState = maybeState, tknrDFA = dfa, tknrDeadStates = deadStates } char = tokenizer {
    tknrState = newState >>= checkDeadState
} where
    state = fromMaybe (dfaStartState dfa) maybeState
    newState = dfaTransition dfa state char
    checkDeadState s = if s `elem` deadStates then Nothing else Just s

-- | This utility function creates a tokenizer that accepts a single character.
tknrSingleChar:: Char -> String -> Bool -> Tokenizer
tknrSingleChar c = tknrNew (Set.singleton c) 0 [1] [2] transitions
    where
        transitions = [((0, c), 1), ((1, c), 2), ((2, c), 2)]

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

-- Tokenizers should have dead states. It's worth keeping in mind that every token detected must be accepted
-- by the DFA by the official definition.

-- . 0
--   ^
-- 

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
_tokenizeInternal a b c d e | trace (_debugTokenize a b c d e) False = undefined

-- base case: The string is fully consumed so we know everything was tokenized
_tokenizeInternal _ "" tokens _ _ = tokens
-- Recursive case: we update the tokenizers each level, checking for matches and consuming
-- substrings if a token is detected
_tokenizeInternal tokenizers inputString tokens currentStart currentIndex
    -- NO tokenizers are active: character is not valid (or we just started)
    | currentIndex /= 0 && not (any (isJust . tknrState) tokenizers) =
        error $ "Unexpected char '" ++ [prevChar] ++ "' at position " ++ show prevPosition
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
        prevPosition = currentStart + currentIndex - 1
        prevChar = inputString !! (currentIndex - 1)
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
        tknrFinishedMatching tknr = not (tknrValidNow tknr)
        -- For more complicated matces the solution would be to process them at a tree level,
        -- not token level. For example, the minus (-) sign is both a negative symbol and the
        -- subtraction operator. The distinction would be made once the tokens are processed.
        matches = filter tknrValidState tokenizers
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
lexerWhitespace = Set.fromList [' ', '\t']
lexerLetters = lexerLowercase `Set.union` lexerUppercase
lexerAlphanumeric = lexerLetters `Set.union` lexerNumbers
lexerFullAlphabet = lexerAlphanumeric `Set.union` lexerSymbols `Set.union` lexerWhitespace

----------  Comments  -----------

-- When state == 0, only the / charater transitions to 1, otherwise 3
-- When state == 1, only the / character transitions to 2, otherwise 3
-- When state == 2, all characters transition to 2
-- state 3 is a dead state

commentTransitions = map (\x -> ((2, x), 2)) (Set.toList lexerFullAlphabet) ++
    map (\x -> ((3, x), 3)) (Set.toList lexerFullAlphabet) ++
    ((1, '/'), 2) : map (\x -> ((1, x), 3)) (Set.toList lexerAllExceptSlash) ++
    ((0, '/'), 1) : map (\x -> ((0, x), 3)) (Set.toList lexerAllExceptSlash)
    where
        lexerAllExceptSlash = lexerFullAlphabet `Set.difference` Set.singleton '/'

whitespaceTransitions = [((0, ' '), 1), ((0, '\t'), 1), ((1, ' '), 1), ((1, '\t'), 1)]

plusTokenizer = tknrSingleChar '+' "PLUS" False
minusTokenizer = tknrSingleChar '-' "MINUS" False
whitespaceTokenizer = tknrNew lexerWhitespace 0 [1] [] whitespaceTransitions "WHITESPACE" True
commentTokenizer = tknrNew lexerFullAlphabet 0 [2] [3] commentTransitions "COMMENT" False

main:: IO ()
main = do
    -- print (dfaAccept dfaDivBy4 "10100")
    let result = tokenize [commentTokenizer, plusTokenizer, minusTokenizer, whitespaceTokenizer] "-+-/ /+-"
    result `seq` print result
