-- | Search
module Pinecone.Search
    ( -- * Main types
      SearchWithVectorRequest(..)
    , _SearchWithVectorRequest
    , SearchWithVectorResponse(..)
    , SearchWithTextRequest(..)
    , _SearchWithTextRequest
    , SearchWithTextResponse(..)

     -- * Other types
    , Match(..)
    , Query(..)
    , _Query
    , VectorQuery(..)
    , Rerank(..)
    , Result(..)
    , Hit(..)
    , Usage(..)

      -- * Servant
    , API
    ) where

import Prelude hiding (filter)
import Pinecone.Embed (Input(..))
import Pinecone.Metadata (Filter, Scalar)
import Pinecone.Prelude
import Pinecone.Vectors (Namespace, SparseValues)

-- | Request body for @\/query@
data SearchWithVectorRequest = SearchWithVectorRequest
    { topK :: Natural
    , namespace :: Maybe Namespace
    , filter :: Maybe Filter
    , includeValues :: Maybe Bool
    , includeMetadata :: Maybe Bool
    , vector :: Maybe (Vector Double)
    , sparseVector :: Maybe SparseValues
    , id :: Maybe Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `SearchWithVectorRequest`
_SearchWithVectorRequest :: SearchWithVectorRequest
_SearchWithVectorRequest = SearchWithVectorRequest
    { namespace = Nothing
    , filter = Nothing
    , includeValues = Nothing
    , includeMetadata = Nothing
    , vector = Nothing
    , sparseVector = Nothing
    , id = Nothing
    }

-- | Response body for @\/query@
data SearchWithVectorResponse = SearchWithVectorResponse
    { matches :: Vector Match
    , namespace :: Namespace
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A search request for records in a specific namespace.
data SearchWithTextRequest = SearchWithTextRequest
    { query :: Query
    , fields :: Maybe (Vector Text)
    , rerank :: Maybe Rerank
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `SearchWithTextRequest`
_SearchWithTextRequest :: SearchWithTextRequest
_SearchWithTextRequest = SearchWithTextRequest
    { fields = Nothing
    , rerank = Nothing
    }

-- | A successful search namespace response.
data SearchWithTextResponse = SearchWithTextResponse
    { result :: Result
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Match for a vector
data Match = Match
    { id :: Text
    , score :: Maybe Double
    , values :: Maybe (Vector Double)
    , sparseValues :: Maybe SparseValues
    , metadata :: Maybe (Map Text Scalar)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The query inputs to search with
data Query = Query
    { top_k :: Natural
    , filter :: Maybe Filter
    , input :: Maybe Text
    , vector :: Maybe VectorQuery
    } deriving stock (Eq, Generic, Show)

data Query_ = Query_
    { top_k :: Natural
    , filter :: Maybe Filter
    , inputs :: Maybe Input
    , vector :: Maybe VectorQuery
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

instance FromJSON Query where
    parseJSON value = do
        Query_{..} <- parseJSON value

        let input = do
                Input{..} <- inputs
                return text

        return Query{..}

instance ToJSON Query where
    toJSON Query{..} = toJSON Query_{..}
      where
        inputs = do
            text <- input

            return Input{..}

-- | Default `Query`
_Query :: Query
_Query = Query
    { filter = Nothing
    , input = Nothing
    , vector = Nothing
    }

-- | Vector query
data VectorQuery = VectorQuery
    { values :: Maybe (Vector Double)
    , sparse_values :: Maybe (Vector Double)
    , sparse_indices :: Maybe (Vector Natural)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Parameters for reranking the initial search results
data Rerank = Rerank
    { model :: Text
    , rank_fields :: Vector Text
    , top_n :: Maybe Natural
    , parameters :: Maybe (Map Text Value)
    , query :: Maybe Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Search result
data Result = Result
    { hits :: Vector Hit
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Hit for the search document request
data Hit = Hit
    { _id :: Text
    , _score :: Double
    , fields :: Value
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Usage
data Usage = Usage
    { read_units :: Maybe Natural
    , embed_total_tokens :: Maybe Natural
    , rerank_units :: Maybe Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
          (   "query"
          :>  ReqBody '[JSON] SearchWithVectorRequest
          :>  Post '[JSON] SearchWithVectorResponse
          )
    :<|>  (   "records"
          :>  "namespaces"
          :>  Capture "namespace" Namespace
          :>  "search"
          :>  ReqBody '[JSON] SearchWithTextRequest
          :>  Post '[JSON] SearchWithTextResponse
          )
