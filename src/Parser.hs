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
    drawChildren [t] =
        "│" : shift "└─ " "   " (draw t)
    drawChildren (t : ts) =
        "│" : shift "├─ " "│  " (draw t) ++ drawChildren ts

    shift first other = zipWith (++) (first : repeat other)

data ParseError = ParseError
    { parseErrorMsg :: String
    , parseErrorRemainingTokens :: [Token]
    }

instance Show ParseError where
    show ParseError{parseErrorRemainingTokens = []} = "Syntax error at line 0 col 0"
    show ParseError{parseErrorRemainingTokens = (tkn@Token{tokenLine = Just ln, tokenStart = col} : _)} =
        "Syntax error at line " ++ show ln ++ " col " ++ show col ++ show tkn
    show _ = "Syntax error."

parse :: Grammar -> [Token] -> Either ParseError AST
parse grammar tokens = case parse' grammar (grammarStart grammar) tokens of
    Nothing ->
        Left
            ParseError
                { parseErrorMsg = "A parse tree could not be built from the provided tokens."
                , parseErrorRemainingTokens = tokens
                }
    Just (tree, []) -> Right tree
    Just (_, tkns) ->
        Left
            ParseError
                { parseErrorMsg = "There was an error while parsing the tokens."
                , parseErrorRemainingTokens = tkns
                }

parse' :: Grammar -> NonTerminal -> [Token] -> Maybe (AST, [Token])
parse' grammar nt tokens = useRules $ productionsFor grammar nt
  where
    useRules :: [ProductionRule] -> Maybe (AST, [Token])
    useRules [] = Nothing
    useRules ((_, symbols) : rest) =
        case parseRule symbols tokens of
            Just (subtrees, remainingTokens) ->
                let nonEmpty = filter (not . isEmpty) subtrees
                 in Just (Node nt nonEmpty, remainingTokens)
            Nothing -> useRules rest

    parseRule :: [Symbol] -> [Token] -> Maybe ([AST], [Token])
    parseRule [] tkns = Just ([], tkns)
    parseRule (T terminal : rest) (token : tkns)
        | terminal == tokenID token = do
            (subtrees, remainingTokens) <- parseRule rest tkns
            return (Leaf terminal token : subtrees, remainingTokens)
    parseRule (NT nonTerminal : rest) tkns = do
        (subtree, remainingTokens) <- parse' grammar nonTerminal tkns
        (subtrees, finalTokens) <- parseRule rest remainingTokens
        return (subtree : subtrees, finalTokens)
    parseRule _ _ = Nothing

    isEmpty :: AST -> Bool
    isEmpty (Node _ []) = True
    isEmpty _ = False
