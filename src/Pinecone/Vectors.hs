-- | Vectors
module Pinecone.Vectors
    ( -- * Main types
      UpsertVectorsRequest(..)
    , _UpsertVectorsRequest
    , UpsertVectorsResponse(..)
    , FetchVectors(..)
    , UpdateVector(..)
    , _UpdateVector
    , DeleteVectors(..)
    , _DeleteVectors
    , ListVectorIDs(..)
    , UpsertText(..)
    , _UpsertText

      -- * Other types
    , VectorObject(..)
    , SparseValues(..)
    , VectorID(..)
    , Usage(..)
    , Pagination(..)

      -- * Servant
    , API
    ) where

import Pinecone.Metadata (Filter, Scalar)
import Pinecone.Prelude
import Pinecone.Indexes (Index)
import Prelude hiding (id)

import qualified Data.Aeson.KeyMap as KeyMap

-- | Request body for @\/vectors\/upsert@
data UpsertVectorsRequest = UpsertVectorsRequest
    { vectors :: Vector VectorObject
    , namespace :: Maybe Index
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `UpsertVectorsRequest`
_UpsertVectorsRequest :: UpsertVectorsRequest
_UpsertVectorsRequest = UpsertVectorsRequest
    { namespace = Nothing
    }

-- | Response body for @\/vectors\/upsert@
data UpsertVectorsResponse = UpsertVectorsResponse
    { upsertedCount :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Response body for @\/vectors\/fetch@
data FetchVectors = FetchVectors
    { vectors :: Map Text VectorObject
    , namespace :: Index
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/vectors\/update@
data UpdateVector = UpdateVector
    { id :: Text
    , values :: Maybe (Vector Double)
    , sparseValues :: Maybe SparseValues
    , setMetadata :: Maybe (Map Text Scalar)
    , namespace :: Maybe Index
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `UpdateVector`
_UpdateVector :: UpdateVector
_UpdateVector = UpdateVector
    { values = Nothing
    , sparseValues = Nothing
    , setMetadata = Nothing
    , namespace = Nothing
    }

-- | Request body for @\/vectors\/delete@
data DeleteVectors = DeleteVectors
    { ids :: Maybe (Vector Text)
    , deleteAll :: Maybe Bool
    , namespace :: Maybe Index
    , filter :: Maybe Filter
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `DeleteVectors`
_DeleteVectors :: DeleteVectors
_DeleteVectors = DeleteVectors
    { deleteAll = Nothing
    , namespace = Nothing
    , filter = Nothing
    }

-- | Response body for @\/vectors\/list@
data ListVectorIDs = ListVectorIDs
    { vectors :: Vector VectorID
    , pagination :: Pagination
    , namespace :: Index
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/records\/namespaces\/:namespace\/upsert@
data UpsertText = UpsertText
    { id :: Text
    , text :: Text
    , metadata :: Maybe (Map Text Scalar)
    } deriving stock (Eq, Generic, Show)

instance ToJSON UpsertText where
    toJSON UpsertText{..} = Object (KeyMap.union reserved nonReserved)
      where
        reserved =
            [ ("_id", toJSON id)
            , ("text", toJSON text)
            ]

        nonReserved = case metadata of
            Nothing -> []
            Just m -> case toJSON m of
                Object o -> o
                _ -> KeyMap.empty

-- | Default `UpsertText`
_UpsertText :: UpsertText
_UpsertText = UpsertText{ }

-- | A vector
data VectorObject = VectorObject
    { id :: Text
    , values :: Maybe (Vector Double)
    , sparseValues :: Maybe SparseValues
    , metadata :: Maybe (Map Text Scalar)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Vector sparse data
data SparseValues = SparseValues
    { indices :: Vector Natural
    , values :: Vector Double
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Vector ID
data VectorID = VectorID
    { id :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Usage
data Usage = Usage
    { readUnits :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Pagination
data Pagination = Pagination
    { next :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
          (     (   "vectors"
                :>  (   "upsert"
                    :>  ReqBody '[JSON] UpsertVectorsRequest
                    :>  Post '[JSON] UpsertVectorsResponse
                    )
                )

          :<|>  (   "fetch"
                :>  QueryParam' '[Required, Strict] "ids" Text
                :>  QueryParam "namespace" Index
                :>  Get '[JSON] FetchVectors
                )
          :<|>  (   "update"
                :>  ReqBody '[JSON] UpdateVector
                :>  Post '[JSON] NoContent
                )
          :<|>  (   "delete"
                :>  ReqBody '[JSON] DeleteVectors
                :>  Post '[JSON] NoContent
                )
          :<|>  (   "list"
                :>  QueryParam "prefix" Text
                :>  QueryParam "limit" Natural
                :>  QueryParam "paginationToken" Text
                :>  QueryParam "namespace" Index
                :>  Get '[JSON] ListVectorIDs
                )
          )
    :<|>  (   "records"
          :>  "namespaces"
          :>  Capture "namespace" Index
          :>  "upsert"
          :>  ReqBody '[JSON] UpsertText
          :>  PostCreated '[JSON] NoContent
          )
