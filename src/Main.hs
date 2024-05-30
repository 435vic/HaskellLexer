import Lang qualified
import System.Environment (getArgs)
import System.Exit qualified as Exit
import Tokenizer
import Parser

usageMessage :: String
usageMessage = "Usage: ./lexer <input_string> OR ./lexer -f <input_file>\n"

parseArgs :: [String] -> IO ()
parseArgs ["-f", filename] = readFile filename >>= parseLines . lines
parseArgs [] = putStrLn usageMessage >> Exit.exitFailure
parseArgs l = parseLines l

substr :: String -> Int -> Int -> String
substr s start end = take (end - start) (drop start s)

parseLines :: [String] -> IO ()
parseLines l = do
    tokens <- populateTokens l <$> tokenizeLines l 0
    case parse Lang.grammar (concat tokens) of
        Left err -> print err
        Right tree -> print tree
    putStrLn "done :)"
  where
    tokenizeLines :: [String] -> Int -> IO [[Token]]
    tokenizeLines [] _ = return []
    tokenizeLines (line:rest) lineno = case Lang.newTokenize line of
        Left err -> do
            putStrLn $ "Token error on line " ++ show lineno ++ ": " ++ show err
            fail "Syntax error, halting."
        Right tokens -> do
            restTokens <- tokenizeLines rest (lineno + 1)
            return (tokens : restTokens)

    populateTokens :: [String] -> [[Token]] -> [[Token]]
    populateTokens ls = zipWith (map . getTokenContent) (zip [0..] ls)

    getTokenContent :: (Int, String) -> Token -> Token
    getTokenContent (line, string) token = token {
        tokenContent = Just $ substr string (tokenStart token) (tokenEnd token),
        tokenLine = Just line
    }

main :: IO ()
main = getArgs >>= parseArgs
