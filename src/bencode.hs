module Bencode (encode,decode) where

import Data.List (sortBy)
import Data.Map (empty,insert,toList)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import Value (Dict,List,Value(I,S,D,L))

encode :: Value -> String
encode (I i) = encodeInt i
encode (S s) = encodeString s
encode (D d) = encodeDict d
encode (L l) = encodeList l

decode :: String -> Maybe (Value,Int,String)
decode ('i':tl) = decodeInt tl >>= \(res,i,s) -> pure (I res,i,s)
decode ('l':tl) = decodeList tl >>= \(res,i,s) -> pure (L res,i,s)
decode ('d':tl) = decodeDict tl >>= \(res,i,s) -> pure (D res,i,s)
decode s = decodeString s >>= \(res,i,s') -> pure (S res,i,s')

segment :: Char -> String -> Maybe (String,String)
segment = segment' []

segment' :: String -> Char -> String -> Maybe (String,String)
segment' res c (hd:tl) = if c == hd 
    then pure (reverse res, tl)
    else segment' (hd : res) c tl
segment' _ _ _ = Nothing

decodeInt :: String -> Maybe (Int,Int,String)
decodeInt b = segment 'e' b >>= decodeInt'

decodeInt' :: (String,String) -> Maybe (Int,Int,String)
decodeInt' (s1,s2) = pure (x,n,s2) 
    where x = fromIntegral $ read s1
          n = length s1 + 2

decodeString :: String -> Maybe (String,Int,String)
decodeString b = segment ':' b >>= decodeString'

decodeString' :: (String,String) -> Maybe (String,Int,String)
decodeString' (s1,_) | isNothing $ (readMaybe s1 :: Maybe Int) = Nothing
decodeString' (s1,s2) = 
    if length s' /= r 
        then Nothing 
        else pure (s',r',s'')
    where r = fromIntegral $ read s1
          r' = length s1 + 1 + r
          (s',s'') = splitAt r s2

decodeList :: String -> Maybe (List,Int,String)
decodeList = decodeList' [] 2

decodeList' :: List -> Int -> String -> Maybe (List,Int,String)
decodeList' _ _ [] = Nothing
decodeList' l r ('e':tl) = pure (reverse l,r,tl)
decodeList' l r s = decode s >>= \(v,r',s') -> decodeList' (v:l) (r+r') s'

decodeDict :: String -> Maybe(Dict,Int,String)
decodeDict = decodeDict' empty 2

decodeDict' :: Dict -> Int -> String -> Maybe(Dict,Int,String)
decodeDict' _ _ [] = Nothing
decodeDict' d r ('e':tl) = pure (d,r,tl)
decodeDict' d r s = do 
    (k,r',s') <- decodeString s
    (v,r'',s'') <- decode s'
    decodeDict' (insert k v d) (r+r'+r'') s''

encodeInt :: Int -> String
encodeInt i = 'i' : show i ++ "e"

encodeString :: String -> String
encodeString s = (show $ length s) ++ ':' : s

encodeList :: [Value] -> String
encodeList l = 
    'l' : (concat $ map encode l) ++ "e"

encodeDict :: Dict -> String
encodeDict d = 
    'd' : (concat . map (\(k,v) -> encodeString k ++ encode v) . sortBy (\(k1,_) (k2,_) -> k1 `compare` k2) $ toList d) ++ "e"