{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Autodocodec
import Autodocodec.OpenAPI
import Data.Aeson hiding (object, (.=), (<?>))
import Data.Aeson.Casing
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types qualified as J
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Declare
import Data.Proxy
import Data.Text (Text)
import GHC.Generics

data Person = Person
  { personName :: Text,
    personAge :: Maybe Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Person

instance HasCodec Person where
  codec =
    object "Person" personCodec
      <?> "An object representing a person"
    where
      personCodec =
        Person
          <$> requiredField "name" "The person's name" .= personName
          <*> optionalField "age" "The person's age" .= personAge

-- instance ToJSON Person
-- instance FromJSON Person
-- instance ToSchema Person

-- myOptions :: Options
-- myOptions = aesonPrefix snakeCase

-- instance ToJSON Person where
--   toJSON = genericToJSON myOptions

-- instance FromJSON Person where
--   parseJSON = genericParseJSON myOptions

-- instance ToSchema Person where
--   declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions myOptions

aPerson :: Person
aPerson = Person "John Smith" (Just 21)

data Colour = Red | Green | Blue
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Colour

instance HasCodec Colour where
  codec = stringConstCodec [(Red, "red"), (Green, "green"), (Blue, "blue")]

newtype Name = Name {unName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Name

instance HasCodec Name where
  codec = dimapCodec Name unName textCodec <?> "A name"

data Expression
  = LiteralExpression Int
  | SumExpression Expression Expression
  | ProductExpression Expression Expression
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via (Autodocodec Expression)

instance HasCodec Expression where
  codec =
    named "Expression" $
      object "Expression" $
        discriminatedUnionCodec "type" enc dec
    where
      valueFieldCodec = requiredField' "value"
      lrFieldsCodec =
        (,)
          <$> requiredField' "left" .= fst
          <*> requiredField' "right" .= snd
      enc = \case
        LiteralExpression n -> ("literal", mapToEncoder n valueFieldCodec)
        SumExpression l r -> ("sum", mapToEncoder (l, r) lrFieldsCodec)
        ProductExpression l r -> ("product", mapToEncoder (l, r) lrFieldsCodec)
      dec =
        HashMap.fromList
          [ ( "literal",
              ("LiteralExpression", mapToDecoder LiteralExpression valueFieldCodec)
            ),
            ( "sum",
              ("SumExpression", mapToDecoder (uncurry SumExpression) lrFieldsCodec)
            ),
            ( "product",
              ("ProductExpression", mapToDecoder (uncurry ProductExpression) lrFieldsCodec)
            )
          ]

main :: IO ()
main = do
  -- print aPerson
  -- BS.putStrLn $ encodePretty aPerson
  -- BS.putStrLn $ encodePretty $ toSchema (Proxy @Person)
  -- BS.putStrLn $ encodePretty $ toSchema (Proxy @Colour)
  -- BS.putStrLn $ encodePretty $ toSchema (Proxy @Name)
  -- BS.putStrLn $ encodePretty $ toSchema (Proxy @Expression)
  let (definitions, s) = flip runDeclare mempty $ do
        NamedSchema mName schema <- declareNamedSchemaViaCodec (Proxy @Expression)
        declare [(fromMaybe "Expression" mName, schema)]
        pure schema
  let openAPI = mempty {_openApiComponents = mempty {_componentsSchemas = definitions}}
  BS.putStrLn $ encodePretty $ toJSON openAPI
