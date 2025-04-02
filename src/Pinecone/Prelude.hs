module Pinecone.Prelude
    ( -- * JSON
      aesonOptions
    , labelModifier

      -- * Re-exports
    , module Data.Aeson
    , module Data.Aeson.Types
    , module Data.Map
    , module Data.Scientific
    , module Data.String
    , module Data.Text
    , module Data.Vector
    , module GHC.Generics
    , module Numeric.Natural
    , module Servant.API
    , module Web.HttpApiData
    ) where

import Data.Aeson.Types (typeMismatch)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Web.HttpApiData (ToHttpApiData(..))

import Data.Aeson
    ( genericToJSON
    , genericParseJSON
    , FromJSON(..)
    , Key
    , Object
    , Options(..)
    , ToJSON(..)
    , Value(..)
    )
import Servant.API
    ( Capture
    , DeleteAccepted
    , Get
    , Header'
    , JSON
    , NoContent
    , Patch
    , Post
    , PostCreated
    , QueryParam
    , QueryParam'
    , QueryParams
    , ReqBody
    , Required
    , Strict
    , (:<|>)(..)
    , (:>)
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
