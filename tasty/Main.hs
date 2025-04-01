{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import Pinecone (Methods(..))
import Prelude hiding (id)

import Pinecone.Indexes
    ( Cloud(..)
    , CreateIndexWithEmbedding(..)
    , ConfigureIndex(..)
    , EmbedRequest(..)
    , IndexModel(..)
    , IndexModels(..)
    , Status(..)
    )
import Pinecone.Vectors
    ( DeleteVectors(..)
    , FetchVectors(..)
    , ListVectorIDs(..)
    , UpdateVector(..)
    , Record(..)
    , UpsertVectorsRequest(..)
    , UpsertVectorsResponse(..)
    , VectorID(..)
    , VectorObject(..)
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

    let namespace = "test"

    let indexesTest =
            HUnit.testCase "Indexes" do
                let open = do
                        createIndexWithEmbedding CreateIndexWithEmbedding
                            { name = "indexes-test"
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

    let vectorsTest =
            HUnit.testCase "Vectors" do
                let open = do
                        createIndexWithEmbedding CreateIndexWithEmbedding
                            { name = "vectors-test"
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
                                then return ()
                                else waitUntilReady

                    waitUntilReady

                    UpsertVectorsResponse{..} <- upsertVectors UpsertVectorsRequest
                        { vectors =
                            [ VectorObject
                                { id = "vector-0"
                                , values = Just [ 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1 ]
                                , sparseValues = Nothing
                                , metadata = Nothing
                                }
                            ]
                        , namespace = Nothing
                        }

                    HUnit.assertEqual "" upsertedCount 1

                    upsertText namespace
                        [ Record
                            { id = "vector-1"
                            , text = "Hello, world!"
                            , metadata = Just [ ("category", "farewell") ]
                            }
                        ]

                    updateVector UpdateVector
                        { id = "vector-1"
                        , values = Nothing
                        , sparseValues = Nothing
                        , setMetadata = Just [ ("category", "greeting") ]
                        , namespace = Nothing
                        }

                    FetchVectors{ vectors } <- fetchVectors "vector-1" (Just namespace)

                    case vectors of
                        [   ( "vector-0"
                            , VectorObject{ values = Just [ 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1 ] }
                            )
                          , ( "vector-1"
                            , VectorObject{ metadata = Just [ ("category", "greeting") ] }
                            )
                          ] -> do
                            return ()
                        _ -> do
                            HUnit.assertFailure "GET /vectors/fetch - wrong vectors"

                    ListVectorIDs{ vectors = vectorIDs } <- listVectorIDs Nothing Nothing Nothing (Just namespace)

                    deleteVectors DeleteVectors
                        { ids = Just (fmap (\VectorID{ id } -> id) vectorIDs)
                        , deleteAll = Nothing
                        , namespace = Nothing
                        , filter = Nothing
                        }

                    return ()


    let tests =
            [ indexesTest
            , vectorsTest
            ]

    Tasty.defaultMain (Tasty.testGroup "Tests" tests)
