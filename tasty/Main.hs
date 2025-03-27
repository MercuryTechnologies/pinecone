{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Pinecone (Methods(..))

import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Pinecone
import qualified Servant.Client as Client
import qualified System.Environment as Environment
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = do
    let managerSettings = TLS.tlsManagerSettings
            { HTTP.Client.managerResponseTimeout =
                HTTP.Client.responseTimeoutNone
            }

    manager <- TLS.newTlsManagerWith managerSettings

    baseUrl <- Client.parseBaseUrl "https://api.pinecone.io"

    let clientEnv = Client.mkClientEnv manager baseUrl

    key <- Environment.getEnv "PINECONE_KEY"

    let Methods{..} = Pinecone.makeMethods clientEnv (Text.pack key)

    let indexesTest =
            HUnit.testCase "Create index" do
                _ <- listIndexes

                return ()

    let tests =
            [ indexesTest
            ]

    Tasty.defaultMain (Tasty.testGroup "Tests" tests)
