module Exceptions.ParseException ( ParseException (ParseException) ) where

import Control.Exception ( Exception )
import qualified Data.Text as T

newtype ParseException = ParseException { message :: T.Text}

instance Show ParseException where
  show (ParseException message) = "ParseExcention: " <> show message

instance Exception ParseException
