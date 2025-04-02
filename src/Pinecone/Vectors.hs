-- | Vectors
module Pinecone.Vectors
    ( -- * Main types
      Namespace(..)
    , UpsertVectorsRequest(..)
    , _UpsertVectorsRequest
    , UpsertVectorsResponse(..)
    , FetchVectors(..)
    , UpdateVector(..)
    , _UpdateVector
    , DeleteVectors(..)
    , _DeleteVectors
    , ListVectorIDs(..)
    , Record(..)
    , _Record

      -- * Other types
    , VectorObject(..)
    , SparseValues(..)
    , VectorID(..)
    , Usage(..)

      -- * Servant
    , API
    ) where

import Pinecone.Metadata (Filter, Scalar)
import Pinecone.Pagination (Pagination)
import Pinecone.Prelude
import Prelude hiding (id)

-- | The namespace of a record
newtype Namespace = Namespace{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/vectors\/upsert@
data UpsertVectorsRequest = UpsertVectorsRequest
    { vectors :: Vector VectorObject
    , namespace :: Maybe Namespace
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
    , namespace :: Namespace
    , usage :: Maybe Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/vectors\/update@
data UpdateVector = UpdateVector
    { id :: Text
    , values :: Maybe (Vector Double)
    , sparseValues :: Maybe SparseValues
    , setMetadata :: Maybe (Map Text Scalar)
    , namespace :: Maybe Namespace
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
    , namespace :: Maybe Namespace
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
    , pagination :: Maybe Pagination
    , namespace :: Namespace
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Record to upsert
data Record = Record
    { id :: Text
    , text :: Text
    , metadata :: Maybe (Map Text Scalar)
    } deriving stock (Eq, Generic, Show)

instance ToJSON Record where
    toJSON Record{..} = Object (reserved <> nonReserved)
      where
        reserved =
            [ ("_id", toJSON id)
            , ("text", toJSON text)
            ]

        nonReserved = case metadata of
            Nothing -> []
            Just m -> case toJSON m of
                Object o -> o
                _ -> []

-- | Default `Record`
_Record :: Record
_Record = Record{ }

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

-- | Servant API
type API =
          (   "vectors"
          :>  (     (   "upsert"
                    :>  ReqBody '[JSON] UpsertVectorsRequest
                    :>  Post '[JSON] UpsertVectorsResponse
                    )

              :<|>  (   "fetch"
                    :>  QueryParams "ids" Text
                    :>  QueryParam "namespace" Namespace
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
                    :>  QueryParam "namespace" Namespace
                    :>  Get '[JSON] ListVectorIDs
                    )
              )
          )
    :<|>  (   "records"
          :>  "namespaces"
          :>  Capture "namespace" Namespace
          :>  "upsert"
          :>  ReqBody '[JSON] Record
          :>  PostCreated '[JSON] NoContent
          )
