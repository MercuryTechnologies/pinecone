-- | @\/@
module Pinecone
    ( -- * Methods
      getClientEnv
    , makeMethods
    , Methods(..)

      -- * Servant
    , API
    ) where

import Data.Functor (void)
import Data.Proxy (Proxy(..))
import Pinecone.Prelude
import Servant.Client (ClientEnv)

import Pinecone.Indexes
    ( ConfigureIndex
    , CreateIndex
    , CreateIndexWithEmbedding
    , GetIndexStats
    , Index
    , IndexModel
    , IndexModels
    , IndexStats
    )

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Pinecone.Indexes as Indexes
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Client as Client

-- | Convenient utility to get a `ClientEnv` for the most common use case
getClientEnv
    :: Text
    -- ^ Base URL for API
    -> IO ClientEnv
getClientEnv baseUrlText = do
    baseUrl <- Client.parseBaseUrl (Text.unpack baseUrlText)
    manager <- TLS.newTlsManager
    pure (Client.mkClientEnv manager baseUrl)

-- | Get a record of API methods after providing an API token
makeMethods
    :: ClientEnv
    -- ^
    -> Text
    -- ^ API token
    -> Methods
makeMethods clientEnv token = Methods{..}
  where
    (       (     listIndexes
            :<|>  createIndex
            :<|>  createIndexWithEmbedding
            :<|>  describeIndex
            :<|>  deleteIndex_
            :<|>  configureIndex
            )
      :<|>  getIndexStats
      ) = Client.hoistClient @API Proxy run (Client.client @API Proxy) token "2025-01"

    deleteIndex a = void (deleteIndex_ a)

    run :: Client.ClientM a -> IO a
    run clientM = do
        result <- Client.runClientM clientM clientEnv
        case result of
            Left exception -> Exception.throwIO exception
            Right a -> return a

-- | API methods
data Methods = Methods
    { listIndexes :: IO IndexModels
    , createIndex :: CreateIndex -> IO IndexModel
    , createIndexWithEmbedding :: CreateIndexWithEmbedding -> IO IndexModel
    , describeIndex :: Index -> IO IndexModel
    , deleteIndex :: Index -> IO ()
    , configureIndex :: Index -> ConfigureIndex -> IO IndexModel
    , getIndexStats :: GetIndexStats -> IO IndexStats
    }

-- | Servant API
type API =
        Header' [ Required, Strict ] "Api-Key" Text
    :>  Header' [ Required, Strict ] "X-Pinecone-API-Version" Text
    :>  Indexes.API
