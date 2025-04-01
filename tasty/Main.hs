{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import Pinecone (Methods(..))
import Pinecone.Indexes
    ( Cloud(..)
    , CreateIndexWithEmbedding(..)
    , ConfigureIndex(..)
    , EmbedRequest(..)
    , IndexModel(..)
    , IndexModels(..)
    , Status(..)
    )

import qualified Control.Exception as Exception
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
                let open = do
                        createIndexWithEmbedding CreateIndexWithEmbedding
                            { name = "test"
                            , cloud = AWS
                            , region = "us-east-1"
                            , embed = EmbedRequest
                                { model = "llama-text-embed-v2"
                                , field_map = [ ("text", "contents") ]
                                , metric = Nothing
                                , read_parameters = Nothing
                                , write_parameters = Nothing
                                }
                            , deletion_protection = Nothing
                            , tags = Nothing
                            }

                let close IndexModel{ name } = deleteIndex name

                Exception.bracket open close \IndexModel{ name } -> do
                    let waitUntilReady = do
                            indexModel <- describeIndex name

                            let IndexModel{ status } = indexModel

                            let Status{ ready } = status

                            if ready
                                then return indexModel
                                else waitUntilReady

                    indexModel <- waitUntilReady

                    IndexModels{ indexes } <- listIndexes

                    case indexes of
                        [ indexModel₀ ]
                            | indexModel == indexModel₀ -> return ()
                        _ -> HUnit.assertFailure "GET /indexes - wrong models"

                    _ <- configureIndex name ConfigureIndex
                        { spec = Nothing
                        , deletion_protection = Nothing
                        , tags = Just [ ("foo", "bar") ]
                        , embed = Nothing
                        }

                    return ()

    let tests =
            [ indexesTest
            ]

    Tasty.defaultMain (Tasty.testGroup "Tests" tests)
