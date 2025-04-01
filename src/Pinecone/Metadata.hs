-- | Pinecone's filtering query language
module Pinecone.Metadata
    ( Scalar(..)
    , Filter(..)
    ) where

import Pinecone.Prelude

import qualified Data.Aeson as Aeson

-- | A scalar value used for metadata filters
data Scalar
    = ScalarNumber Scientific
    | ScalarString Text
    | ScalarBoolean Bool
    deriving stock (Eq, Generic, Show)

instance IsString Scalar where
    fromString string = ScalarString (fromString string)

instance Num Scalar where
    fromInteger n = ScalarNumber (fromInteger n)

    ScalarNumber l + ScalarNumber r = ScalarNumber (l + r)
    _ + ScalarNumber r = ScalarNumber r
    l + _ = l

    ScalarNumber l * ScalarNumber r = ScalarNumber (l * r)
    _ * ScalarNumber r = ScalarNumber r
    l * _ = l

    abs (ScalarNumber n) = ScalarNumber (abs n)
    abs n = n

    signum (ScalarNumber n) = ScalarNumber (signum n)
    signum n = n

    negate (ScalarNumber n) = ScalarNumber (negate n)
    negate n = n

instance ToJSON Scalar where
    toJSON (ScalarNumber scientific) = Number scientific
    toJSON (ScalarString text) = String text
    toJSON (ScalarBoolean bool) = Bool bool

instance FromJSON Scalar where
    parseJSON (Number scientific) = pure (ScalarNumber scientific)
    parseJSON (String text) = pure (ScalarString text)
    parseJSON (Bool bool) = pure (ScalarBoolean bool)
    parseJSON object = typeMismatch "Number/String/Boolean" object

-- | Metadata query language
data Filter
    = Equal Key Scalar
    | NotEqual Key Scalar
    | GreaterThan Key Scientific
    | GreaterThanOrEqual Key Scientific
    | LessThan Key Scientific
    | LessThanOrEqual Key Scientific
    | In Key Scalar
    | NotIn Key Scalar
    | Exists Key Bool
    | And (Vector Filter)
    | Or (Vector Filter)
    deriving stock (Eq, Generic, Show)

instance ToJSON Filter where
    toJSON (Equal field literal) =
        Object [ (field, Object [ ("$eq", toJSON literal) ]) ]
    toJSON (NotEqual field literal) =
        Object [ (field, Object [ ("$ne", toJSON literal) ]) ]
    toJSON (GreaterThan field literal) =
        Object [ (field, Object [ ("$gt", toJSON literal) ]) ]
    toJSON (GreaterThanOrEqual field literal) =
        Object [ (field, Object [ ("$gte", toJSON literal) ]) ]
    toJSON (LessThan field literal) =
        Object [ (field, Object [ ("$lt", toJSON literal) ]) ]
    toJSON (LessThanOrEqual field literal) =
        Object [ (field, Object [ ("$lte", toJSON literal) ]) ]
    toJSON (In field literal) =
        Object [ (field, Object [ ("$in", toJSON literal) ]) ]
    toJSON (NotIn field literal) =
        Object [ (field, Object [ ("$nin", toJSON literal) ]) ]
    toJSON (Exists field literal) =
        Object [ (field, Object [ ("$exists", toJSON literal) ]) ]
    toJSON (And clauses) =
        Object [ ("$and", toJSON clauses) ]
    toJSON (Or clauses) =
        Object [ ("$or", toJSON clauses) ]

instance FromJSON Filter where
    parseJSON =
        Aeson.withObject "Filter" \object -> do
            case object of
                [ (field, Object [ ("$eq", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (Equal field literal)
                [ (field, Object [ ("$ne", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (NotEqual field literal)
                [ (field, Object [ ("$gt", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (GreaterThan field literal)
                [ (field, Object [ ("$gte", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (GreaterThanOrEqual field literal)
                [ (field, Object [ ("$lt", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (LessThan field literal)
                [ (field, Object [ ("$lte", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (LessThanOrEqual field literal)
                [ (field, Object [ ("$in", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (In field literal)
                [ (field, Object [ ("$nin", value) ]) ] -> do
                    literal <- parseJSON value
                    pure (NotIn field literal)
                [ ("$and", value) ] -> do
                    clauses <- parseJSON value
                    pure (And clauses)
                [ ("$or", value) ] -> do
                    clauses <- parseJSON value
                    pure (Or clauses)
                _ -> do
                    fail "Invalid filter"
