{- |
Module      :  Tokenizer
Description :  Tokenizer implemented using DFAs
Copyright   :  (c) Victor Quintana A01643020
License     :  MIT

Maintainer  :  A01643020@tec.mx

Tokenizer is a tokenizer implemented using DFAs.
DISCLAIMER: ChatGPT and Github Copilot were consulted along with other online sources
*The actual algorithm is my own design*, following programming patterns I found in online code
and as written by the LLMs consulted.
Most of my interactions with ChatGPT can be found in the following conversation link:
https://chat.openai.com/share/974e55de-ff5f-4de2-aae7-4b664a34edca
-}
module Tokenizer (module Tokenizer) where

import Data.List (nub, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace (trace)

-- The formal definition of a DFA (source: Wikipedia) is a 5-tuple (Q, Σ, δ, q0, F) consisting of:
-- 1. A finite set of states Q, stored as an array of generic type q
-- 2. A finite set of input symbols called the alphabet Σ, stored as an array of characters
-- 3. A transition function δ: Q × Σ → Q, stored as a hashmap
-- 4. An initial state q0 ∈ Q
-- 5. A set of states F that are designated as accepting states
data DFA = DFA
    { dfaAlphabet :: Set.Set Char
    , dfaTransitions :: Map.Map (Int, Char) Int
    , dfaStartState :: Int
    , dfaAcceptStates :: Set.Set Int
    }

{- | This transition function, given a DFA, a state, and an input, looks up the DFA's transition
table for the next state.
-}
dfaTransition :: DFA -> Int -> Char -> Maybe Int
dfaTransition dfa state x = Map.lookup (state, x) (dfaTransitions dfa)

-- | This function consumes the entire input string and determines if it is accepted.
dfaAccept :: DFA -> String -> Bool
dfaAccept dfa = recurse (Just $ dfaStartState dfa)
  where
    recurse :: Maybe Int -> [Char] -> Bool
    recurse (Just state) [] = state `elem` dfaAcceptStates dfa
    recurse (Just state) (x : xs) = recurse (dfaTransition dfa state x) xs
    recurse Nothing _ = False

{- | Tokenizer is an individual tokenizer. It detects a single type of token.
It's built from a DFA object and a string identifier. The `tknrIgnore` option
provides a way to add tokenizers that while needed, should not necessarily be
displayed along with other tokens.
-}
data Tokenizer = Tokenizer
    { tknrDFA :: DFA
    , tknrID :: String
    , tknrIgnore :: Bool
    , tknrDeadState :: Maybe Int
    , tknrState :: Maybe Int
    }

instance Show Tokenizer where
    show tokenizer = "Tokenizer " ++ tknrID tokenizer ++ " state " ++ show (tknrState tokenizer)

{- | Token represents a parsed token from a string. It contains the ID, and the start (inclusive)
and end (exclusive) indices.
-}
data Token = Token
    { tokenID :: String
    , tokenStart :: Int
    , tokenEnd :: Int
    }

instance Show Token where
    show token =
        "Token "
            ++ tokenID token
            ++ " at ("
            ++ show (tokenStart token)
            ++ ", "
            ++ show (tokenEnd token)
            ++ ")"

data TokenizeError = TokenizeError
    { tknrErrMsg :: String
    , tknrErrPos :: Int
    }

instance Show TokenizeError where
    show err =
        tknrErrMsg err ++ " at " ++ show (tknrErrPos err)

-- | Create a new tokenizer with a DFA, String ID and whether to ignore its tokens when tokenizing.
tknrNew :: Set.Set Char -> Int -> Maybe Int -> [Int] -> [((Int, Char), Int)] -> String -> Bool -> Tokenizer
tknrNew alphabet startState deadState acceptStates transitionList tkid ignore =
    Tokenizer
        { tknrDFA = verifiedDFA
        , tknrID = tkid
        , tknrIgnore = ignore
        , tknrDeadState = Just $ fromMaybe newDeadState deadState
        , tknrState = Nothing
        }
  where
    verifiedDFA
        | invalidChars =
            error ("Invalid transition characters " ++ show invalidCharList ++ " for tokenizer " ++ tkid)
        | invalidTransitions =
            error
                ( "Invalid transitions "
                    ++ show invalidStateTransitions
                    ++ " to non-specified state for tokenizer "
                    ++ tkid
                )
        | invalidStates =
            error
                ( "Not all transitions defined for states"
                    ++ show invalidStatesList
                    ++ "for tokenizer "
                    ++ tkid
                )
        | otherwise =
            DFA
                { dfaAlphabet = alphabet
                , dfaAcceptStates = Set.fromList acceptStates
                , dfaTransitions = transitions
                , dfaStartState = startState
                }
    alphabetList = Set.toList alphabet
    transitionMap = Map.fromList transitionList
    transitions = expandedTransitions `Map.union` transitionMap
    -- In some DFA notation when a transition is not specified it is assumed that it immediately
    -- rejects the string. This can be done by creating a additional 'dead state' to which all
    -- missing transitions point to. This state will transition to itself with all symbols.
    expandedTransitions =
        let
            -- (q, c) is every state/character pair
            -- we add the condition that it's NOT part of the transitions
            -- Then, for each of these new transitions we make it go to our new dead state
            newTransitions =
                [ ((q, c), newDeadState)
                | q <- fromStates
                , c <- alphabetList
                , (q, c) `Map.notMember` transitionMap
                ]
            -- Here we specify that at the dead state every character transitions to itself
            deadTransitions = [((newDeadState, c), newDeadState) | c <- alphabetList]
         in
            Map.fromList $ newTransitions ++ deadTransitions
    -- This will calculate the first state number that isn't already in the states list.
    -- Because haskell is haskell, it's an infinite list, of which we only get the first value.
    newDeadState = head [q | q <- [0 ..], q `notElem` fromStates]
    -- Valid chars: All transitions must specify chars that are part of the alphabet
    invalidChars = not $ null invalidCharList
    invalidCharList = [x | ((_, x), _) <- transitionList, x `Set.notMember` alphabet]
    -- Valid transitions: All states transitioned to must exist (i.e) have at least one entry
    -- where the state exists as a source state
    invalidTransitions = not $ null invalidStateTransitions
    fromStates = nub [q | ((q, _), _) <- transitionList]
    invalidStateTransitions = nub $ filter (\((_, _), qt) -> qt `notElem` fromStates) transitionList
    -- Valid states: for each state there must be one and only one transition for ALL elements
    -- of the alphabet. Before checking this we expand the transition list.
    invalidStates = not $ null invalidStatesList
    invalidStatesList = nub [q | q <- fromStates, c <- alphabetList, (q, c) `Map.notMember` transitions]

-- | Determine if a char is part of a tokenizer's defined alphabet.
tknrValidChar :: Tokenizer -> Char -> Bool
tknrValidChar tokenizer char = char `Set.member` dfaAlphabet (tknrDFA tokenizer)

-- | Determine if the tokenizer is currently in an accepted state.
tknrValidState :: Tokenizer -> Bool
tknrValidState tokenizer =
    let
        acceptStates = (dfaAcceptStates . tknrDFA) tokenizer
        state = tknrState tokenizer
     in
        fromMaybe (-1) state `Set.member` acceptStates

-- capture syntax and pattern matching! haskell is kinda cool, reminds me of rust
-- found this out in https://chat.openai.com/share/f115190a-2da0-475e-baf0-7cc44887bd6c
-- (the actual relevant information is at the end of the conversation)

-- | Calcualate the next state of a tokenizer given a char.
tknrStep :: Tokenizer -> Char -> Tokenizer
tknrStep tokenizer@Tokenizer{tknrState = maybeState, tknrDFA = dfa, tknrDeadState = deadState} char =
    tokenizer
        { tknrState = dfaTransition dfa (fromMaybe (dfaStartState dfa) maybeState) char >>= checkDeadState
        }
  where
    checkDeadState s
        | Just s == deadState = Nothing
        | otherwise = Just s

-- | This utility function creates a tokenizer that accepts a single character.
tknrSingleChar :: Char -> String -> Bool -> Tokenizer
tknrSingleChar c = tknrNew (Set.singleton c) 0 Nothing [1] transitions
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

_debugTokenize :: [Tokenizer] -> [Tokenizer] -> String -> [Token] -> Int -> Int -> String
_debugTokenize _ currentTokenizers str tokens start idx =
    "\n--------------\ntokenizer call:\n"
        ++ "tokenizers: "
        ++ show currentTokenizers
        ++ "\n"
        ++ "string: "
        ++ show str
        ++ "\n"
        ++ "tokens: "
        ++ show tokens
        ++ "\n"
        ++ "start: "
        ++ show start
        ++ "\n"
        ++ "index: "
        ++ show idx
        ++ "\n"

tokenize :: [Tokenizer] -> String -> Either TokenizeError [Token]
tokenize tokenizers inputString = tokenize' tokenizers [] inputString [] 0 0

tokenize' :: [Tokenizer] -> [Tokenizer] -> String -> [Token] -> Int -> Int -> Either TokenizeError [Token]
-- _tokenizeInternal a b c d e f | trace (_debugTokenize a b c d e f) False = undefined
-- base case: The string is fully consumed so we know everything was tokenized
tokenize' _ _ "" tokens _ _ = Right tokens
-- Recursive case: we update the tokenizers each level, checking for matches and consuming
-- substrings if a token is detected
tokenize' tokenizers currentTokenizers inputString tokens currentStart currentIndex
    -- NO tokenizers are active: character is not valid
    | null activeTokenizers =
        Left
            TokenizeError
                { tknrErrMsg = "Unexpected character " ++ [head inputString]
                , tknrErrPos = currentStart
                }
    -- error $ "Unexpected char '" ++ [head inputString] ++ "' at position " ++ show currentStart
    -- 'matches' contains all tokenizers that are in valid states
    -- The ones have finished matching (i.e the current character would be invalid) are sorted
    -- last in the list. If the top of the list has one that's finished it means that no longer
    -- tokens can be created. In this case we take the most important one of those.
    | not (null matches) && tknrFinishedMatching matchedTokenizer =
        let
            -- After a substring matches we can discard all the states of the tokenizers,
            -- they are not needed anymore
            newTokenizers = map (\tknr -> tknr{tknrState = Nothing}) tokenizers
            newToken =
                Token
                    { tokenID = tknrID matchedTokenizer
                    , tokenStart = currentStart
                    , tokenEnd = currentStart + currentIndex
                    }
            -- tokenizers have an ignore property because some tokens should be parsed but not
            -- necessarily displayed or considered (e.g whitespace)
            newTokens = if tknrIgnore matchedTokenizer then tokens else newToken : tokens
         in
            tokenize' newTokenizers [] suffix newTokens (currentStart + currentIndex) 0
    -- No tokenizers have finished a match
    -- We advance the currentIndex by one, calculating the next state for all tokenizers
    -- We should never reach this if endOfString is True
    | otherwise = tokenize' tokenizers nextTokenizers inputString tokens currentStart (currentIndex + 1)
  where
    endOfString = currentIndex >= length inputString
    currentChar = inputString !! currentIndex
    -- Used for creating tokens
    (_, suffix) = splitAt currentIndex inputString
    -- Next iteration of tokenizers, we only update the ones that are currently active
    nextTokenizers = map (`tknrStep` currentChar) activeTokenizers
    -- A tokenizer is considered active when it is in a non-dead state.
    -- Only tokenizers that activate on the first character of the substring will be considered.
    activeTokenizers
        | currentIndex == 0 = tokenizers
        | endOfString = filter tknrValidState currentTokenizers
        | otherwise = filter (isJust . tknrState) currentTokenizers

    -- This function determines if the current character is part of the specified
    -- tokenizer's alphabet. Of course, past the end of the string anything would be invalid.
    tknrStillMatching tknr
        | endOfString = False
        | otherwise = isJust (tknrState $ tknr `tknrStep` currentChar)

    -- A tokenizer has a complete match if it reaches an *accepted* state at the end of a
    -- \*valid* string. Which means the valid string ends as soon as an invalid character
    -- appears in the input string.
    tknrFinishedMatching tknr = tknrValidState tknr && not (tknrStillMatching tknr)

    -- The tokenizers that are still matching take priority in this case
    matches =
        let
            (left, right) = partition tknrStillMatching activeTokenizers
        in left ++ filter tknrFinishedMatching right
    -- matches = trace (show _matches) _matches
    matchedTokenizer = head matches
