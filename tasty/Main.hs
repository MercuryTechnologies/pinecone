{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import Pinecone (DataMethods(..), ControlMethods(..))
import Pinecone.Embed (GenerateVectors(..), Input(..))
import Prelude hiding (id)

import Pinecone.Indexes
    ( Cloud(..)
    , CreateIndexWithEmbedding(..)
    , ConfigureIndex(..)
    , EmbedRequest(..)
    , GetIndexStats(..)
    , IndexModel(..)
    , IndexModels(..)
    , IndexStats(..)
    , Status(..)
    )
import Pinecone.Search
    ( Result(..)
    , Query(..)
    , SearchWithTextRequest(..)
    , SearchWithTextResponse(..)
    , SearchWithVectorRequest(..)
    , SearchWithVectorResponse(..)
    )
import Pinecone.Vectors
    ( DeleteVectors(..)
    , UpdateVector(..)
    , Record(..)
    , UpsertVectorsRequest(..)
    , UpsertVectorsResponse(..)
    , VectorObject(..)
    )

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Pinecone
import qualified System.Environment as Environment
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = do
    controlEnv <- Pinecone.getClientEnv "https://api.pinecone.io"

    key <- Environment.getEnv "PINECONE_KEY"

    let token = Text.pack key

    let ControlMethods{..} = Pinecone.makeControlMethods controlEnv token

    let namespace = "test"

    Tasty.defaultMain do
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

            Exception.bracket open close \IndexModel{ name, host } -> do
                _ <- generateVectors GenerateVectors
                    { model = "llama-text-embed-v2"
                    , inputs = [ Input{ text = "Hello, world!" } ]
                    , parameters = [ ("input_type", "query") ]
                    }

                dataEnv <- Pinecone.getClientEnv host

                let DataMethods{..} = Pinecone.makeDataMethods dataEnv token

                let waitUntilIndexReady = do
                        indexModel <- describeIndex name

                        let IndexModel{ status } = indexModel

                        let Status{ ready } = status

                        if ready
                            then return indexModel
                            else waitUntilIndexReady

                indexModel <- waitUntilIndexReady

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

                UpsertVectorsResponse{..} <- upsertVectors UpsertVectorsRequest
                    { vectors =
                        [ VectorObject
                            { id = "vector-0"
                            , values = Just (Vector.replicate 1024 0.1)
                            , sparseValues = Nothing
                            , metadata = Nothing
                            }
                        ]
                    , namespace = Just namespace
                    }

                HUnit.assertEqual "" 1 upsertedCount

                upsertText namespace Record
                    { id = "vector-1"
                    , text = "Hello, world!"
                    , metadata = Just [ ("category", "farewell") ]
                    }

                updateVector UpdateVector
                    { id = "vector-1"
                    , values = Nothing
                    , sparseValues = Nothing
                    , setMetadata = Just [ ("category", "greeting") ]
                    , namespace = Just namespace
                    }

                let waitUntilVectorsReady = do
                        IndexStats{..} <- getIndexStats GetIndexStats
                            { filter = Nothing
                            }

                        if totalVectorCount == 2
                            then return ()
                            else waitUntilVectorsReady

                waitUntilVectorsReady

                _ <- fetchVectors [ "vector-1" ] (Just namespace)

                _ <- listVectorIDs Nothing Nothing Nothing (Just namespace)

                SearchWithTextResponse{ result } <- searchWithText namespace SearchWithTextRequest
                    { query = Query
                        { top_k = 1
                        , filter = Nothing
                        , input = Just "Hi!"
                        , vector = Nothing
                        }
                    , fields = Nothing
                    , rerank = Nothing
                    }

                let Result{ hits } = result

                HUnit.assertEqual "" 1 (Vector.length hits)

                SearchWithVectorResponse{ matches } <- searchWithVector SearchWithVectorRequest
                    { topK = 1
                    , namespace = Just namespace
                    , filter = Nothing
                    , includeValues = Nothing
                    , includeMetadata = Nothing
                    , vector = Just (Vector.replicate 1024 0.1)
                    , sparseVector = Nothing
                    , id = Nothing
                    }

                HUnit.assertEqual "" 1 (Vector.length matches)

                deleteVectors DeleteVectors
                    { ids = Nothing
                    , deleteAll = Just True
                    , namespace = Just namespace
                    , filter = Nothing
                    }

                return ()
