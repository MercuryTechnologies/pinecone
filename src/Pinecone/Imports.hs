-- | Imports
module Pinecone.Imports
    ( -- * Main types
      Import(..)
    , StartImportRequest(..)
    , _StartImportRequest
    , StartImportResponse(..)
    , ListImports(..)
    , ImportModel(..)

      -- * Servant
    , API
    ) where

import Pinecone.Pagination
import Pinecone.Prelude
import Prelude hiding (id)

-- | Unique identifier for the import operation
newtype Import = Import{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/bulk\/imports@
data StartImportRequest = StartImportRequest
    { uri :: Text
    , integrationId :: Maybe Text
    , errorMode :: Maybe ErrorMode
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `StartImportRequest`
_StartImportRequest :: StartImportRequest
_StartImportRequest = StartImportRequest
    { integrationId = Nothing
    , errorMode = Nothing
    }

-- | Response body for @\/bulk\/imports@
data StartImportResponse = StartImportResponse
    { id :: Import
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A list of import operations
data ListImports = ListImport
    { data_ :: Vector ImportModel
    , pagination :: Maybe Pagination
    } deriving stock (Eq, Generic, Show)

instance FromJSON ListImports where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ListImports where
    toJSON = genericToJSON aesonOptions

-- | The model for an import operation.
data ImportModel = ImportModel
    { id :: Import
    , uri :: Text
    , status :: Status
    , createdAt :: POSIXTime
    , finishedAt :: POSIXTime
    , percentComplete :: Double
    , recordsImported :: Natural
    , error :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Indicates how to respond to errors during the import process.
data ErrorMode = ErrorMode
    { onError :: OnError
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Indicates how to respond to errors during the import process.
data OnError = Abort | Continue
    deriving stock (Eq, Generic, Show)

instance FromJSON OnError where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON OnError where
    toJSON = genericToJSON aesonOptions

-- | The status of the operation.
data Status
    = Pending
    | InProgress
    | Failed
    | Completed
    | Cancelled
    deriving stock (Eq, Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions{ constructorTagModifier = \x -> x }

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions{ constructorTagModifier = \x -> x }

-- | Servant API
type API =
        "bulk"
    :>  "imports"
    :>  (         ReqBody '[JSON] StartImportRequest
              :>  Post '[JSON] StartImportResponse

        :<|>      QueryParam "limit" Natural
              :>  QueryParam "paginationToken" Text
              :>  Get '[JSON] ListImports

        :<|>      Capture "id" Import
              :>  Get '[JSON] ImportModel

        :<|>      Capture "id" Import
              :>  Delete '[JSON] NoContent
        )
