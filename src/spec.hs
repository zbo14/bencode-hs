module Spec where

import Bencode (encode,decode)
import Data.Map (fromList)
import Value (Value(I,S,L,D),wrap)
import Test.Hspec

string = "bencode"
wrappedString = wrap string
encodedString = encode wrappedString
decodedString= decode encodedString

int = 1001 :: Int
wrappedInt = wrap int
encodedInt = encode wrappedInt
decodedInt = decode encodedInt

list = [wrappedString,wrappedInt]
wrappedList = wrap list
encodedList = encode wrappedList
decodedList = decode encodedList

dict = fromList [("string",wrappedString),("list", wrappedList),("int",wrappedInt)] -- unsorted
wrappedDict = wrap dict
encodedDict = encode wrappedDict
decodedDict = decode encodedDict

main :: IO ()
main = hspec $ do 
    describe "Bencode" $ do 
        it "checks encoded string" $
            encodedString `shouldBe` "7:bencode"
        it "checks decoded string" $
            decodedString `shouldBe` Just (S string,9,[])
        it "checks encoded int" $ 
            encodedInt `shouldBe` "i1001e"
        it "checks decoded int" $
            decodedInt `shouldBe` Just (I int,6,[])
        it "checks encoded list" $
            encodedList `shouldBe` "l7:bencodei1001ee"
        it "checks decoded list" $
            decodedList `shouldBe` Just (L list,17,[])
        it "checks encoded dict" $
            encodedDict `shouldBe` "d3:inti1001e4:listl7:bencodei1001ee6:string7:bencodee" -- sorted
        it "checks decoded dict" $
            decodedDict `shouldBe` Just (D dict,53,[])
        it "tries to decode unterminated" $ 
            decode "l7:bencodei1001e" `shouldBe` Nothing
        it "tries to decode string that's too short" $ 
            decode "7:bencod" `shouldBe` Nothing
        it "tries to decode string with unexpected first char" $ 
            decode "z:whatisz?" `shouldBe` Nothing
        it "tries to decode without colon" $ 
            decode "4spam" `shouldBe` Nothing
