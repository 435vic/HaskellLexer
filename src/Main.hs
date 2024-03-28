import Lexer
import Tokenizer

main :: IO ()
main = do
    -- print (dfaAccept dfaDivBy4 "10100")
    let result = tokenize arithmeticTokenizers "a = 32.4 *(-8.6 - b)/       6.1E-8"
    result `seq` print result