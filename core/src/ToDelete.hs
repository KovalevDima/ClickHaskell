{-# LANGUAGE
    DataKinds
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , TypeApplications
  , UndecidableInstances
#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ToDelete where

import Data.Data    (Proxy(Proxy))
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(Text, ShowType, (:<>:)), symbolVal)

import ClickHaskell.ChTypes (ChDateTime)

data ChColumn


type family Columns (table :: [(Symbol, b)]) :: [(Symbol, b)]
  where
  Columns ( '(fst1, snd1) ': x2 ': xs)  = '( fst1, snd1 ) ': IsMissing fst1 xs
  Columns '[x1] = '[x1]


type MyColumns = Columns
  '[ '("a", String)
  ,  '("", ChDateTime)
  ]

type family IsMissing (x :: key) (l :: [(key, value)]) :: [(key, value)] where
  IsMissing k ('(k,a) ': ls) = TypeError ('Text "There is duplicated column: " ':<>: 'ShowType k )
  IsMissing k '[ '(x,a)]     = '[ '(x, a)]
  IsMissing k ('(x,a) ': ls) = IsMissing k ls
  IsMissing k '[]            = '[]


type family Keys (l :: [(key, value)]) :: [key] where
  Keys ('[] :: [(key, value)]) = TypeError ('Text "Empty list: " ':<>: 'ShowType [key])
  Keys ('(key, value) ': '[])  = '[key]
  Keys ('(key, value) ': ls)   = key ': Keys ls


type family Head  (l :: [e]) :: e where
  Head ('[] :: [e]) = TypeError ('Text "Empty list: " ':<>: 'ShowType '[e])
  Head '[e]         = e
  Head ( e ': xs)   = e


type family Fst (k :: (fst, snd)) :: fst where
  Fst '(fst, snd) = fst




type MyType = IsMissing "b" MyColumns

a :: Proxy MyType
a = Proxy

b :: String
b = symbolVal $ Proxy @(Head (Keys MyType))

-- >>> b
-- Proxy

-- c = natVal $ Proxy @A 
-- type A = "asda" ^ 5




