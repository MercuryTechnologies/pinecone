-- | @\/@
module Pinecone
    ( -- * Methods
      getClientEnv
    , makeMethods
    , Methods(..)

      -- * Servant
    , API
    ) where

import Data.Proxy (Proxy(..))
import Pinecone.Prelude
import Pinecone.Indexes (IndexModels)
import Servant.Client (ClientEnv)

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
    ( listIndexes
      ) = Client.hoistClient @API Proxy run (Client.client @API Proxy) token

    run :: Client.ClientM a -> IO a
    run clientM = do
        result <- Client.runClientM clientM clientEnv
        case result of
            Left exception -> Exception.throwIO exception
            Right a -> return a

-- | API methods
data Methods = Methods
    { listIndexes :: IO IndexModels
    }

-- | Servant API
type API =
        Header' [ Required, Strict ] "Api-Key" Text
    :>  Indexes.API
