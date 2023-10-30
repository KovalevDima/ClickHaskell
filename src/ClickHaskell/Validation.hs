{-# LANGUAGE
    DataKinds
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}
module ClickHaskell.Validation
  ( HandleErrors
  , ThrowOnError

  , GetRepresentationInColumns

  , GenericTreeIsSortedAlphabetically
  , GetGenericProductHeadSelector
  , GetGenericProductLastSelector

  , SpanByColumnName

  , AssumePlacedBefore
  ) where

-- GHC included libraries imports
import Data.Type.Bool (If)
import Data.Type.Ord  (type(<=?), type(>?))
import GHC.Base       (Type, Symbol, Constraint)
import GHC.Generics   (type(:*:), D1, C1, S1, Meta(MetaSel))
import GHC.TypeError  (ErrorMessage(..), TypeError)




-- * Errors handling 

-- | Type family usefull when you have a several posible errors and need to return first one.
-- 
-- `True` indicates an error
-- 
-- @  
-- type NotAnError = '(False, 'Text "Not an error")
-- type Error1     = '(True, 'Text "error1")
-- type Error2     = '(True, 'Text "error2")
--
-- type HandleErrors '[NotAnError, Error2] = '(True, 'Text "error2")
-- type HandleErrors '[Error1,     Error2] = '(True, 'Text "error1")
-- type HandleErrors '[NotAnError]         = '(False, 'Text "HandleErrors: Please report an issue if you see this message")
-- @
type family HandleErrors (a :: [(Bool, ErrorMessage)]) :: (Bool, ErrorMessage)
  where
  HandleErrors '[] = '( 'False, 'Text "HandleErrors: Please report an issue if you see this message")
  HandleErrors ('(False, txt) ': xs) = HandleErrors xs
  HandleErrors ('(True, txt) ': xs) = '(True, txt) 

type family ThrowOnError (a :: (Bool, ErrorMessage)) :: Constraint 
  where
  ThrowOnError '(False, _) = ()
  ThrowOnError '(True, errorMsg) = TypeError errorMsg




-- * Validations over Generic rep

type family (sym1 :: Symbol) `AssumePlacedBefore` (sym2 :: Symbol) :: (Bool, ErrorMessage)
  where
  sym1 `AssumePlacedBefore` sym2 =
    '( sym1 >? sym2 
     ,    'Text "Record fields should be sorted alphabetically. But field \""
     :<>: 'Text sym2
     :<>: ' Text "\" placed before \""
     :<>: 'Text sym1
     :<>: 'Text "\""
     )

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

type family GenericTreeIsSortedAlphabetically (f :: k -> Type) :: (Bool, ErrorMessage) where
  GenericTreeIsSortedAlphabetically rep = '(True, 'Text "")




-- * Validations over Table description

type family GetRepresentationInColumns
  (names :: [Symbol])
  (columns :: [(Symbol, Type)])
  ::
  [(Symbol, Type)]
  where
  GetRepresentationInColumns '[] columns = TypeError ('Text "Report an issue if you see this message: Validation")
  GetRepresentationInColumns names columns = (GoGetRepresentationInColumns (GetMinimum names) columns)


type family GoGetRepresentationInColumns
  (minimumWithOthers :: (Symbol, [Symbol]))
  (columns :: [(Symbol, Type)])
  ::
  [(Symbol, Type)]
  where
  GoGetRepresentationInColumns '(columnName, '[])  ('(columnName, columnType)  ': columns) = '[ '(columnName, columnType)]
  GoGetRepresentationInColumns '(min, '[])         ('(columnName, _)           ': columns) = GoGetRepresentationInColumns '(min, '[]) columns
  GoGetRepresentationInColumns '(columnName, rest) ('(columnName, columnType)  ': columns) = '(columnName, columnType) ': GoGetRepresentationInColumns (GetMinimum rest) columns
  GoGetRepresentationInColumns '(min, otherElems)  ('(columnName, columnType)  ': columns) =
    If (columnName >? min)
      (TypeError ('Text "Column with name " :<>: 'Text min :<>: 'Text " is not represented in table"))
      (GoGetRepresentationInColumns '(min, otherElems) columns)
  GoGetRepresentationInColumns '(min, xs)          '[]                                     = TypeError ('Text "Column with name " :<>: 'Text min :<>: 'Text " is not represented in table")


type family GetMinimum (elems :: [Symbol]) :: (Symbol, [Symbol])
  where
  GetMinimum (x ': xs) = GoGetMinimum xs '(x, '[])
  GetMinimum '[] = TypeError ('Text "No elements in list. Please report an issue")

type family GoGetMinimum
  (elems :: [Symbol])
  (acc :: (Symbol, [Symbol]))
  ::
  (Symbol, [Symbol])
  where
  GoGetMinimum (previousMin ': xs) '(previousMin, acc) = TypeError ('Text "There are duplicated filters with name \"" :<>: 'Text previousMin :<>: 'Text "\"") 
  GoGetMinimum (x ': xs) '(previousMin, acc) = If (previousMin >? x) (GoGetMinimum xs '(x, previousMin ': acc)) (GoGetMinimum xs '(previousMin, x ': acc))
  GoGetMinimum '[] '(previousMin, acc) = '(previousMin, acc)




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
