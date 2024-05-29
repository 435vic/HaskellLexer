import Lang qualified
import System.Environment (getArgs)
import System.Exit qualified as Exit
import Text.Printf (printf)
import Tokenizer

usageMessage :: String
usageMessage = "Usage: ./lexer <input_string> OR ./lexer -f <input_file>\n"

parseArgs :: [String] -> IO ()
parseArgs ["-f", filename] = readFile filename >>= parseLines . lines
parseArgs [] = putStrLn usageMessage >> Exit.exitFailure
parseArgs l = parseLines l

parseLines :: [String] -> IO ()
parseLines l = mapM_ processLine (zip [0 ..] l)
  where
    processLine :: (Int, String) -> IO ()
    processLine (i, line) = do
        printf msg i line
        displayResults line (tokenize Lang.tokenizers line)
    msg = "--------------- Line %d: \"%s\" ---------------\n"

displayResults :: String -> Either TokenizeError [Token] -> IO ()
displayResults _ (Left err) = putStr "Error: " >> print err
displayResults str (Right tokens) = mapM_ formatted (reverse tokens)
  where
    formatted Token{tokenID = tkid, tokenStart = start, tokenEnd = end} =
        printf ("%-" ++ show padding ++ "s%s\n") (substr str start end) tkid
    padding = maximum (map substrLength tokens) + 4
    substrLength Token{tokenStart = start, tokenEnd = end} = end - start
    substr :: String -> Int -> Int -> String
    substr s start end = take (end - start) (drop start s)

main :: IO ()
main = getArgs >>= parseArgs