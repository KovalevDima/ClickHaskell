{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , TypeOperators
#-}
module ClickHaskell.Validation
  ( HandleErrors
  ) where

import GHC.TypeError (ErrorMessage (..))


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
-- type HandleErrors '[NoAnError]          = '(False, 'Text "HandleErrors: Please report an issue if you see this message")
-- @
type family HandleErrors (a :: [(Bool, ErrorMessage)]) :: (Bool, ErrorMessage)
  where
  HandleErrors '[] = '( 'False, 'Text "HandleErrors: Please report an issue if you see this message")
  HandleErrors ('(False, txt) ': xs) = HandleErrors xs
  HandleErrors ('(True, txt) ': xs) = '(True, txt) 
