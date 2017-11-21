{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Value 
(
    Dict,
    List,
    Value(I,S,D,L),
    Wrapper,
    consDict,
    consInt,
    consList,
    consString,
    insertDict,
    insertInt,
    insertList,
    insertString,
    wrap,
    unwrap
) where

import Data.Map (Map,insert)

type Dict = Map String Value

type List = [Value]

data Value = I Int | S String | D Dict | L List deriving (Show,Eq,Ord)

class Wrapper a where
    wrap :: a -> Value
    unwrap :: Value -> Maybe a
    unwrap _ = Nothing 

instance Wrapper Int where 
    wrap = I
    unwrap (I i) = pure i

instance Wrapper String where 
    wrap = S
    unwrap (S s) = pure s

instance Wrapper Dict where 
    wrap = D
    unwrap (D d) = pure d

instance Wrapper List where 
    wrap = L
    unwrap (L l)= pure l

cons :: Wrapper a => a -> List -> List 
cons v l = (wrap v) : l

consDict :: Dict -> List -> List 
consDict v = Value.cons v

consInt :: Int -> List -> List 
consInt v = Value.cons v

consList :: List -> List -> List 
consList v = Value.cons v

consString :: String -> List -> List 
consString v = Value.cons v

insert :: Wrapper a => String -> a -> Dict -> Dict
insert k = Data.Map.insert k . wrap

insertDict :: String -> Dict -> Dict -> Dict
insertDict k = Value.insert k

insertInt :: String -> Int -> Dict -> Dict 
insertInt k = Value.insert k

insertList :: String -> List -> Dict -> Dict 
insertList k = Value.insert k

insertString :: String -> String -> Dict -> Dict 
insertString k = Value.insert k