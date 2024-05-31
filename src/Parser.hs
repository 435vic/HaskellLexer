{- |
Module      :  Parser
Description :  Recursive descent parser for LL(k) context free grammars
Copyright   :  (c) Victor Quintana A01643020
License     :  MIT

Maintainer  :  A01643020@tec.mx

An implementation of a recursive descent parser.
-}
module Parser (
    Grammar (Grammar),
    Symbol (T, NT),
    AST,
    grammarStart,
    grammarRules,
    productionsFor,
    parse,
) where

import Tokenizer
import Debug.Trace
import Data.Bool (bool)
import Data.Maybe

type Terminal = String

type NonTerminal = String

data Symbol = T Terminal | NT NonTerminal deriving (Show, Eq)

type ProductionRule = (NonTerminal, [Symbol])

data Grammar = Grammar
    { grammarStart :: NonTerminal
    , grammarRules :: [ProductionRule]
    }
    deriving (Show, Eq)

productionsFor :: Grammar -> NonTerminal -> [ProductionRule]
productionsFor grammar nt = filter (\(lhs, _) -> lhs == nt) (grammarRules grammar)

data AST = Node NonTerminal [AST] | Leaf Terminal Token

instance Show AST where
    show = drawTree

drawTree :: AST -> String
drawTree = unlines . draw
  where
    draw :: AST -> [String]
    draw (Node nt trees) = nt : drawChildren trees
    draw (Leaf _ token) = [displayToken token]

    drawChildren :: [AST] -> [String]
    drawChildren [] = []
    drawChildren [t] = shift "└─ " "   " (draw t)
    drawChildren (t : ts) = shift "├─ " "│  " (draw t) ++ drawChildren ts

    shift first other = zipWith (++) (first : repeat other)

data ParseErrorType
    = UnexpectedToken Token Terminal
    | UnexpectedEndOfInput
    deriving Show

data ParseError = ParseError
    { parseErrType :: ParseErrorType
    , parseErrStack :: [Symbol]
    }

instance Show ParseError where
    show (ParseError err stack) = "Syntax error:\n" ++ case err of
        UnexpectedToken token expected ->
            "Unexpected token on line " ++ show (fromMaybe 0 (tokenLine token)) ++ ": " ++ show (fromMaybe "[unknown]" (tokenContent token)) ++ ", expected " ++ expected
        UnexpectedEndOfInput -> "Unexpected end of input at " ++ show stack

parseErrDepth :: ParseError -> Int
parseErrDepth = length . parseErrStack

parse :: Grammar -> [Token] -> Either ParseError AST
parse grammar tokens = case parse' grammar (grammarStart grammar) tokens [NT (grammarStart grammar)] Nothing of
    Right (ast, [], _) -> Right ast
    Right (_, _, _) -> Left $ ParseError UnexpectedEndOfInput []
    Left err -> Left err

-- ParseError is passed as a parameter and as a return value to keep track of the deepest error
-- This gives us slightly better error messages
parse' :: Grammar -> NonTerminal -> [Token] -> [Symbol] -> Maybe ParseError -> Either ParseError (AST, [Token], Maybe ParseError)
-- parse' _ nt _ stack | trace ("\nparse' " ++ nt ++ " " ++ show stack) False = undefined
parse' grammar nt tokens st = useRules (productionsFor grammar nt) st
  where
    useRules :: [ProductionRule] -> [Symbol] -> Maybe ParseError -> Either ParseError (AST, [Token], Maybe ParseError)
    -- useRules a b c | trace ("useRules " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
    useRules [] _ (Just lastErr) = Left lastErr
    useRules [] stack Nothing = Left $ ParseError UnexpectedEndOfInput stack
    useRules ((_, symbols) : rest) stack lastErr =
        case parseRule symbols tokens stack lastErr of
            Right (subtrees, remainingTokens) ->
                let
                    nonEmpty = filter (not . isEmpty) subtrees
                    fwdErr = bool Nothing lastErr (null symbols)
                 in Right (Node nt nonEmpty, remainingTokens, fwdErr)
            Left err -> useRules rest stack (Just err)

    parseRule :: [Symbol] -> [Token] -> [Symbol] -> Maybe ParseError -> Either ParseError ([AST], [Token])
    -- parseRule symbols tkns stack err | trace ("parseRule " ++ show tkns ++ " " ++ show symbols ++ " " ++ show stack) False = undefined
    -- parseRule symbols tkns stack err | trace ("parseRule " ++ show tkns ++ " " ++ show err ++ " " ++ show stack) False = undefined
    parseRule [] tkns _ _ = Right ([], tkns)
    parseRule (T terminal : rest) (token : tkns) stack err
        | terminal == tokenID token = do
            (subtrees, remainingTokens) <- parseRule rest tkns stack err
            Right (Leaf terminal token : subtrees, remainingTokens)
        -- | otherwise = Left $ ParseError (UnexpectedToken token terminal) stack
        | otherwise = maybe (Left $ ParseError (UnexpectedToken token terminal) stack) Left err
    parseRule (NT nonTerminal : rest) tkns stack err = do
        (subtree, remainingTokens, lastErr) <- parse' grammar nonTerminal tkns (NT nonTerminal : stack) Nothing
        --                       ^ here should be the possible error
        (subtrees, finalTokens) <- parseRule rest remainingTokens stack (deepestErr err lastErr)
        return (subtree : subtrees, finalTokens)
    parseRule _ [] stack _ = Left $ ParseError UnexpectedEndOfInput stack

    isEmpty :: AST -> Bool
    isEmpty (Node _ []) = True
    isEmpty _ = False

    deepestErr :: Maybe ParseError -> Maybe ParseError -> Maybe ParseError
    deepestErr Nothing Nothing = Nothing
    deepestErr Nothing (Just err) = Just err
    deepestErr (Just err) Nothing = Just err
    deepestErr (Just err1) (Just err2) = Just $ bool err1 err2 (parseErrDepth err1 > parseErrDepth err2)
