{-# LANGUAGE
    DataKinds
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  #-}

module ClickHaskell.DataDsl.Type
  ( SpanByColumnName
  , GetGenericProductHeadSelector
  , GetGenericProductLastSelector

  , HandleSeveralErrors
  , AssumePlacedAfter
  ) where

-- GHC included libraries imports
import Data.Type.Bool (If)
import Data.Type.Ord  (type(<=?))
import GHC.Base       (Type, Symbol)
import GHC.Generics   (type(:*:), D1, C1, S1, Meta(MetaSel))
import GHC.TypeError (ErrorMessage (..))


type family HandleSeveralErrors  (a :: [(Bool, ErrorMessage)]) :: (Bool, ErrorMessage)
  where
  HandleSeveralErrors '[] = '( 'False, 'Text "HandleSeveralErrors: Please report an issue if you see this message")
  HandleSeveralErrors ('(False, txt) ': xs) = HandleSeveralErrors xs
  HandleSeveralErrors ('(True, txt) ': xs) = '(True, txt) 


type family (sym1 :: Symbol) `AssumePlacedAfter` (sym2 :: Symbol) :: (Bool, ErrorMessage)
  where
  sym1 `AssumePlacedAfter` sym2 =
    '( sym2 <=? sym1
     ,    'Text "Records fields should be sorted alphabetically. But field \""
     :<>: 'Text sym2
     :<>: 'Text "\" placed before "
     :<>: 'Text sym1
     )



type family SpanByColumnName
  (name :: Symbol)
  (columns :: [(Symbol, Type)])
  ::
  ([(Symbol, Type)], [(Symbol, Type)])
  where
  SpanByColumnName name columns = GoSpanByColumnName name '( columns, '[])

type family GoSpanByColumnName
  (name :: Symbol)
  (acc :: ([(Symbol, Type)], [(Symbol, Type)]))
  ::
  ([(Symbol, Type)], [(Symbol, Type)])
  where
  GoSpanByColumnName name '( '[], acc2) = '(Reverse acc2, '[])
  GoSpanByColumnName name '( '(colName, colType) ': columns, acc2) =
    If (name <=? colName)
      '(Reverse acc2, '(colName, colType) ': columns)
      (GoSpanByColumnName name '(columns, '(colName, colType) ': acc2))

type family Reverse (b :: [a]) :: [a] where Reverse list = GoReverse list '[]
type family GoReverse (list :: [a]) (accum :: [a])
  where
  GoReverse '[]        acc = acc
  GoReverse (x ': xs) acc = GoReverse xs (x ': acc)




type family GetGenericProductHeadSelector (f :: k -> Type) :: Symbol where
  GetGenericProductHeadSelector (c :*: c2) = GetGenericProductHeadSelector c
  GetGenericProductHeadSelector (D1 _ f)   = GetGenericProductHeadSelector f
  GetGenericProductHeadSelector (C1 _ f)   = GetGenericProductHeadSelector f
  GetGenericProductHeadSelector (S1 (MetaSel (Just sel) _ _ _) _) = sel

type family GetGenericProductLastSelector (f :: k -> Type) :: Symbol where
  GetGenericProductLastSelector (c :*: c2) = GetGenericProductLastSelector c2
  GetGenericProductLastSelector (D1 _ f)   = GetGenericProductLastSelector f
  GetGenericProductLastSelector (C1 _ f)   = GetGenericProductLastSelector f
  GetGenericProductLastSelector (S1 (MetaSel (Just sel) _ _ _) _) = sel
