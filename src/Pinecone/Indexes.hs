-- | @\/indexes@
module Pinecone.Indexes
    ( -- * Main types
      IndexModels(..)
    , IndexModel(..)

      -- * Other types
    , Metric(..)
    , Spec(..)
    , Pod(..)
    , PodType(..)
    , Prefix(..)
    , Suffix(..)
    , MetadataConfig(..)
    , Serverless(..)
    , Cloud(..)
    , Status(..)
    , State(..)
    , VectorType(..)
    , DeletionProtection(..)
    , Embed(..)

      -- * API
    , API
    ) where

import Pinecone.Prelude

import qualified Data.Text as Text

data IndexModels = IndexModels
    { indexes :: Vector IndexModel
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The `IndexModel` describes the configuration and status of a Pinecone
-- index
data IndexModel = IndexModel
    { name :: Text
    , metric :: Metric
    , host :: Text
    , spec :: Spec
    , status :: Status
    , vector_type :: VectorType
    , dimension :: Maybe Natural
    , deletion_protection :: Maybe DeletionProtection
    , tags :: Maybe (Map Text Text)
    , embed :: Maybe Embed
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The distance metric to be used for similarity search
data Metric = Cosine | Euclidean | DotProduct
    deriving stock (Generic, Show)

instance FromJSON Metric where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Metric where
    toJSON = genericToJSON aesonOptions

-- | `Spec` objet
data Spec = Spec
    { pod :: Maybe Pod
    , serverless :: Maybe Serverless
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Configuration needed to deploy a pod-based index.
data Pod = Pod
    { environment :: Text
    , pod_type :: PodType
    , replicas :: Maybe Natural
    , shards :: Maybe Natural
    , pods :: Maybe Natural
    , metadata_config :: Maybe MetadataConfig
    , source_collection :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The type of pod to use
data PodType = PodType
    { prefix :: Prefix
    , suffix :: Suffix
    } deriving stock (Generic, Show)

instance ToJSON PodType where
    toJSON PodType{..} = do
        toJSON (Text.concat [ prefixText, ".", suffixText ])
      where
        prefixText = case toJSON prefix of
            String text -> text
            _ -> ""

        suffixText = case toJSON suffix of
            String text -> text
            _ -> ""

instance FromJSON PodType where
    parseJSON value = do
        text <- parseJSON value

        case Text.splitOn "." text of
            [ prefixText, suffixText ] -> do
                prefix <- parseJSON (String prefixText)
                suffix <- parseJSON (String suffixText)
                return PodType{..}
            _ -> do
                fail "Pod type must be of the form PREFIX.SUFFIX"

-- | The first component of a pod type
data Prefix = S1 | P1 | P2
    deriving stock (Generic, Show)

instance FromJSON Prefix where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Prefix where
    toJSON = genericToJSON aesonOptions

-- | The second component of a pod type
data Suffix = X1 | X2 | X4 | X8
    deriving stock (Generic, Show)

instance FromJSON Suffix where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Suffix where
    toJSON = genericToJSON aesonOptions

-- | Configuration for the behavior of Pinecone's internal metadata index
data MetadataConfig = MetadataConfig
    { indexed :: Maybe (Vector Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Configuration needed to deploy a serverless index
data Serverless = Serverless
    { cloud :: Cloud
    , region :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The public cloud where you would like your index hosted
data Cloud = GCP | AWS | Azure
    deriving stock (Generic, Show)

instance FromJSON Cloud where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Cloud where
    toJSON = genericToJSON aesonOptions

-- | Index status
data Status = Status
    { ready :: Bool
    , state :: State
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Index state
data State
    = Initializing
    | InitializationFailed
    | ScalingUp
    | ScalingDown
    | ScalingUpPodSize
    | ScalingDownPodSize
    | Terminating
    | Ready
    deriving stock (Generic, Show)

instance FromJSON State where
    parseJSON = genericParseJSON aesonOptions{ constructorTagModifier = id }

instance ToJSON State where
    toJSON = genericToJSON aesonOptions{ constructorTagModifier = id }

data VectorType = Dense | Sparse
    deriving stock (Generic, Show)

instance FromJSON VectorType where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON VectorType where
    toJSON = genericToJSON aesonOptions

data DeletionProtection = Disabled | Enabled
    deriving stock (Generic, Show)

instance FromJSON DeletionProtection where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON DeletionProtection where
    toJSON = genericToJSON aesonOptions

data Embed = Embed
    { model :: Text
    , metric :: Maybe Metric
    , dimension :: Maybe Natural
    , vector_type :: Maybe VectorType
    , field_map :: Maybe (Map Text Text)
    , read_parameters :: Maybe (Map Text Text)
    , write_parameters :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "indexes"
    :>  Get '[JSON] IndexModels
