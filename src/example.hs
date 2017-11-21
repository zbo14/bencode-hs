module Main where

import Bencode (decode,encode)
import Data.Map (empty)
import Value (consInt,consString,insertList,insertString,wrap)

main :: IO ()
main = do 
    let l = consInt 123 $ consString "abc" []
        d = insertString "string" "v a p o r vv a v e" $ insertList "list" l empty
        s = encode $ wrap d
    putStrLn s
    -- d4:listli123e3:abce6:string18:v a p o r vv a v ee
    let d' = decode s
    putStrLn $ show d'
    -- Just (D (fromList [("list",L [I 123,S "abc"]),("string",S "v a p o r vv a v e")]),49,"")