import Lexer
import Tokenizer

main :: IO ()
main = do
    -- print (dfaAccept dfaDivBy4 "10100")
    let result = tokenize [commentTokenizer, plusTokenizer, minusTokenizer, whitespaceTokenizer] "++--/ /+-"
    result `seq` print result