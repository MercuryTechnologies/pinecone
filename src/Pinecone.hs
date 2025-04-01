-- | @\/@
module Pinecone
    ( -- * Methods
      getClientEnv
    , makeControlMethods
    , ControlMethods(..)
    , makeDataMethods
    , DataMethods(..)

      -- * Servant
    , ControlAPI
    , DataAPI
    ) where

import Data.Foldable (toList)
import Data.Functor (void)
import Data.Proxy (Proxy(..))
import Pinecone.Prelude
import Servant.Client (ClientEnv)

import Pinecone.Indexes
    ( ConfigureIndex
    , CreateIndex
    , CreateIndexWithEmbedding
    , GetIndexStats
    , Host(..)
    , Index
    , IndexModel
    , IndexModels
    , IndexStats
    )
import Pinecone.Search
    ( SearchWithTextRequest
    , SearchWithTextResponse
    , SearchWithVectorRequest
    , SearchWithVectorResponse
    )
import Pinecone.Vectors
    ( DeleteVectors
    , FetchVectors
    , ListVectorIDs
    , Namespace
    , Record
    , UpdateVector
    , UpsertVectorsRequest
    , UpsertVectorsResponse
    )
import Servant.Client.Core (BaseUrl(..), Scheme(..))

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Pinecone.Indexes as Indexes
import qualified Pinecone.Search as Search
import qualified Pinecone.Vectors as Vectors
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Client as Client

-- | Convenient utility to get a `ClientEnv` for the most common use case
getClientEnv
    :: Host
    -- ^ Base URL for API
    --
    -- __CAREFULLY NOTE:__ This should be @app.pinecone.io@ for
    -- `makeControlMethods` and should be /the index host/ for
    -- `makeDataMethods`
    -> IO ClientEnv
getClientEnv (Host baseUrlText) = do
    baseUrl <- Client.parseBaseUrl (Text.unpack baseUrlText)

    -- The hosts returned by the Pinecone API don't include the `https://`
    -- prefix, which means that by default they're parsed as insecure URLs,
    -- which we fix here.
    --
    -- If you don't like this behavior or you specifically want to use an
    -- insecure URL then just use the `mkClientEnv` function yourself instead
    -- of using this `getClientEnv` utility function.
    let newBaseUrl = baseUrl
          { baseUrlScheme = Https
          , baseUrlPort = 443
          }

    let managerSettings = TLS.tlsManagerSettings
            { HTTP.Client.managerResponseTimeout =
                HTTP.Client.responseTimeoutNone
            }

    manager <- TLS.newTlsManagerWith managerSettings

    pure (Client.mkClientEnv manager newBaseUrl)

apiVersion :: Text
apiVersion = "2025-01"

-- | Get a record of control API methods after providing an API token
makeControlMethods
    :: ClientEnv
    -- ^
    -> Text
    -- ^ API token
    -> ControlMethods
makeControlMethods clientEnv token = ControlMethods{..}
  where
    (       listIndexes
      :<|>  createIndex
      :<|>  createIndexWithEmbedding
      :<|>  describeIndex
      :<|>  deleteIndex_
      :<|>  configureIndex
      ) = Client.hoistClient @ControlAPI Proxy (run clientEnv) (Client.client @ControlAPI Proxy) token apiVersion

    deleteIndex a = void (deleteIndex_ a)

-- | Get a record of control API methods after providing an API token
makeDataMethods
    :: ClientEnv
    -- ^
    -> Text
    -- ^ API token
    -> DataMethods
makeDataMethods clientEnv token = DataMethods{..}
  where
    (      getIndexStats
      :<|> (      (     upsertVectors
                  :<|>  fetchVectors_
                  :<|>  updateVector_
                  :<|>  deleteVectors_
                  :<|>  listVectorIDs
                  )
            :<|>  upsertText_
            )
      :<|>  (     searchWithVector
            :<|>  searchWithText
            )
      ) = Client.hoistClient @DataAPI Proxy (run clientEnv) (Client.client @DataAPI Proxy) token apiVersion

    fetchVectors a = fetchVectors_ (toList a)
    updateVector a = void (updateVector_ a)
    deleteVectors a = void (deleteVectors_ a)
    upsertText a b = void (upsertText_ a b)

run :: Client.ClientEnv -> Client.ClientM a -> IO a
run clientEnv clientM = do
    result <- Client.runClientM clientM clientEnv
    case result of
        Left exception -> Exception.throwIO exception
        Right a -> return a

-- | API methods
data ControlMethods = ControlMethods
    { listIndexes :: IO IndexModels
    , createIndex :: CreateIndex -> IO IndexModel
    , createIndexWithEmbedding :: CreateIndexWithEmbedding -> IO IndexModel
    , describeIndex :: Index -> IO IndexModel
    , deleteIndex :: Index -> IO ()
    , configureIndex :: Index -> ConfigureIndex -> IO IndexModel
    }

data DataMethods = DataMethods
    { getIndexStats :: GetIndexStats -> IO IndexStats
    , upsertVectors :: UpsertVectorsRequest -> IO UpsertVectorsResponse
    , upsertText :: Namespace -> Record -> IO ()
    , fetchVectors
        :: Vector Text
        -- ^ IDs
        -> Maybe Namespace
        -- ^ namespace
        -> IO FetchVectors
    , updateVector :: UpdateVector -> IO ()
    , deleteVectors :: DeleteVectors -> IO ()
    , listVectorIDs
        :: Maybe Text
        -- ^ prefix
        -> Maybe Natural
        -- ^ limit
        -> Maybe Text
        -- ^ pagination token
        -> Maybe Namespace
        -- ^ namespace
        -> IO ListVectorIDs
    , searchWithVector :: SearchWithVectorRequest -> IO SearchWithVectorResponse
    , searchWithText
        :: Namespace
        -> SearchWithTextRequest
        -> IO SearchWithTextResponse
    }

-- | Index management
type ControlAPI =
        Header' [ Required, Strict ] "Api-Key" Text
    :>  Header' [ Required, Strict ] "X-Pinecone-API-Version" Text
    :>  Indexes.ControlAPI

-- | Index operations
type DataAPI =
        Header' [ Required, Strict ] "Api-Key" Text
    :>  Header' [ Required, Strict ] "X-Pinecone-API-Version" Text
    :>  (Indexes.DataAPI :<|> Vectors.API :<|> Search.API)
