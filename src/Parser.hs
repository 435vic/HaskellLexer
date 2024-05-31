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
    = UnexpectedToken Token 
    | UnexpectedEndOfInput
    deriving Show

data ParseError = ParseError
    { parseErrType :: ParseErrorType
    , parseErrStack :: [Symbol]
    } deriving Show

parseErrDepth :: ParseError -> Int
parseErrDepth = length . parseErrStack

parse :: Grammar -> [Token] -> Either ParseError AST
parse grammar tokens = case parse' grammar (grammarStart grammar) tokens [NT (grammarStart grammar)] Nothing of
    Right (ast, []) -> Right ast
    Right (_, _) -> Left $ ParseError UnexpectedEndOfInput []
    Left err -> Left err

parse' :: Grammar -> NonTerminal -> [Token] -> [Symbol] -> Maybe ParseError -> Either ParseError (AST, [Token])
parse' _ nt _ stack | trace ("\nparse' " ++ nt ++ " " ++ show stack) False = undefined
parse' grammar nt tokens st = useRules (productionsFor grammar nt) st
  where
    useRules :: [ProductionRule] -> [Symbol] -> Maybe ParseError -> Either ParseError (AST, [Token])
    useRules [] _ (Just lastErr) = Left lastErr
    useRules [] stack Nothing = Left $ ParseError (UnexpectedToken (head tokens)) stack
    useRules ((_, symbols) : rest) stack _ =
        case parseRule symbols tokens stack of
            Right (subtrees, remainingTokens) ->
                let nonEmpty = filter (not . isEmpty) subtrees
                 in Right (Node nt nonEmpty, remainingTokens)
            Left err -> useRules rest stack (Just err)

    parseRule :: [Symbol] -> [Token] -> [Symbol] -> Either ParseError ([AST], [Token])
    parseRule symbols tokens stack | trace ("parseRule " ++ show tokens ++ " " ++ show symbols ++ " " ++ show stack) False = undefined
    parseRule [] tkns _ = Right ([], tkns)
    parseRule (T terminal : rest) (token : tkns) stack
        | terminal == tokenID token = do
            (subtrees, remainingTokens) <- parseRule rest tkns stack
            Right (Leaf terminal token : subtrees, remainingTokens)
        | otherwise = Left $ ParseError (UnexpectedToken token) (trace "unexpected token" stack)
    parseRule (NT nonTerminal : rest) tkns stack = do
        (subtree, remainingTokens) <- parse' grammar nonTerminal tkns (NT nonTerminal : stack) Nothing
        (subtrees, finalTokens) <- parseRule rest remainingTokens (NT nonTerminal : stack)
        return (subtree : subtrees, finalTokens)
    parseRule _ [] stack = Left $ ParseError UnexpectedEndOfInput stack

    isEmpty :: AST -> Bool
    isEmpty (Node _ []) = True
    isEmpty _ = False
