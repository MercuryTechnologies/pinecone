module Pinecone.Prelude
    ( -- * JSON
      aesonOptions
    , labelModifier

      -- * Re-exports
    , module Data.Aeson
    , module Data.Map
    , module Data.Text
    , module Data.Vector
    , module GHC.Generics
    , module Numeric.Natural
    , module Servant.API
    ) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Servant.API (Get, JSON, Header', Required, Strict, (:<|>), (:>))

import Data.Aeson
    ( genericToJSON
    , genericParseJSON
    , FromJSON(..)
    , Options(..)
    , ToJSON(..)
    , Value(..)
    )

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore "_" = ""
dropTrailingUnderscore ""  = ""
dropTrailingUnderscore (c : cs) = c : dropTrailingUnderscore cs

labelModifier :: String -> String
labelModifier = map Char.toLower . dropTrailingUnderscore

aesonOptions :: Options
aesonOptions = Aeson.defaultOptions
    { fieldLabelModifier = labelModifier
    , constructorTagModifier = labelModifier
    , omitNothingFields = True
    }
