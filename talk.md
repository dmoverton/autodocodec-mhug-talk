---
marp: true
---

# Autodocodec

## A self-documenting encoder and decoder

### MHUG Talk 28th July 2022

### David Overton & Daniel Chambers

---

# Agenda

1. Introduction
2. Deep-dive
3. ...

---

# Introduction

---

# Haskell Examples

## Record type

```haskell
data Person =
  { personName :: Name,
    personAge :: Int,
    personFavouriteColour :: Colour
  }
```

---

## Bounded enum

```haskell
data Colour = Red | Green | Blue
```

---

## Newtype

```haskell
newtype Name = Name { unName :: Text }
```

---

## (Recursive) Sum type

```haskell
data Expression
  = ExpressionLiteral Int
  | ExpressionSum Expression Expression
```

---
# How does it work?

---
## The `Codec` GADT type

```haskell
data Codec context input output where
  ...
```

A `Codec` is a recursive data structure that captures the structure of a data type, along with information about how to construct/destructure it.

* `context`: Used to split the GADT into two parts: 
  1. codecs for JSON Values `type ValueCodec = Codec JSON.Value`
  1. codecs for JSON Objects `type ObjectCodec = Codec JSON.Object`
* `input`: The type that this codec can encode
* `output`: The type that this codec can decode

---
## The `HasCodec` typeclass

```haskell
class HasCodec value where
  codec :: ValueCodec value value
```

Types can have an instance of this typeclass when they have a codec that describes how to encode and decode them as a JSON value.

The `Codec` type parameters are therefore set as such:
* `context`: `JSON.Value`
* `input`: `value`
* `output`: `value`

---
## Basic Codecs

These capture the basic JSON data types.

```haskell
NullCodec   :: ValueCodec () ()
BoolCodec   :: {- Name of bool -}   Maybe Text -> ValueCodec Bool Bool
StringCodec :: {- Name of string -} Maybe Text -> ValueCodec Text Text
NumberCodec :: {- Name of number -} Maybe Text -> Maybe NumberBounds -> ValueCodec Scientific Scientific
```

The matching basic data types have `HasClass` instances already. For example:

```haskell
instance HasCodec Text where
  codec = StringCodec Nothing
```

Autodocodec knows how to encode/decode these basic types, so these codecs effectively act as placeholders.

---

## The ArrayOf Codec

```haskell
ArrayOfCodec ::
    -- | Name of the @array@, for error messages and documentation.
    Maybe Text ->
    ValueCodec input output ->
    ValueCodec (Vector input) (Vector output)
```

How to encode/decode an array is also built in to Autodocodec. All you need to do is tell it how do encode and decode each of the values (`ValueCodec input output`). 

Naturally, a typeclass instance exists to help encode/decode lists:

```haskell
instance HasCodec a => HasCodec [a] where
  codec = dimapCodec Vector.toList Vector.fromList . ArrayOfCodec Nothing
```

--- 

## What's that `dimapCodec` nonsense?

```haskell
dimapCodec ::
  (oldOutput -> newOutput) ->
  (newInput -> oldInput) ->
  Codec context oldInput oldOutput ->
  Codec context newInput newOutput
```

The `ArrayOfCodec` is a 
`ValueCodec (Vector input) (Vector output)`
but we need a 
`ValueCodec [input] [output]`

`dimapCodec` lets us provide mapping functions to convert the Vector to and from a list.

```haskell
codec = dimapCodec Vector.toList Vector.fromList . ArrayOfCodec Nothing
```

---

## The Bimap Codec captures encoding/decoding logic

```haskell
BimapCodec ::
    (oldOutput -> Either String newOutput) -> -- Decoding function
    (newInput -> oldInput) ->                 -- Encoding function
    Codec context oldInput oldOutput ->       -- The old codec
    Codec context newInput newOutput
```

The decoding function is allowed to fail. The encoding function must always succeed.

`dimapCodec` records our mapping functions using a `BimapCodec`:

```
dimapCodec decode encode codec = BimapCodec (Right . decode) encode codec
```

--- 

# Okay, what about JSON objects?

Let's start talking about `ObjectCodec`s.

---

## Capturing object properties

```haskell
RequiredKeyCodec ::
  Text ->                    -- Property name
  ValueCodec input output -> -- Codec for the property value
  Maybe Text ->              -- Doco about the property
  ObjectCodec input output
  
OptionalKeyCodec ::
  Text ->                    -- Property name
  ValueCodec input output -> -- Codec for the property value
  Maybe Text ->              -- Doco about the property
  ObjectCodec (Maybe input) (Maybe output)
```

These two codecs allow us to capture the existence of property ("key") on an object, along with how to encode/decode the property value. 

`RequiredKeyCodec` is for when the property must exist on the object, `OptionalKeyCodec` is for when it does not need to exist (hence the `Maybe` in the codec's `input` and `output` types).

---

## The ObjectOf Codec 

```haskell
ObjectOfCodec ::
  Maybe Text ->               -- Name of the object
  ObjectCodec input output -> -- Codec of the object
  ValueCodec input output
```

Once we know how to encode/decode an object, we can capture that as a `ValueCodec` that can en/decode a Value that is an Object by using `ObjectOfCodec`. (A JSON Object is a JSON Value)

---

## Example: A simple JSON object
```json
{ "newZealandText": "simple as, bro" }
```
The matching Haskell type:
```haskell
data NewZealandObject = NewZealandObject { _nzoText :: Text }
```
The codec:
```haskell
instance HasCodec NewZealandObject where
  codec :: ValueCodec NewZealandObject NewZealandObject
  codec =
    RequiredKeyCodec propName propValueCodec doco  -- Make the property codec
      & dimapCodec NewZealandObject _nzoText       -- Map Text type in/outof the record type
      & ObjectOfCodec objectName                   -- Wrap in ObjectOf
    where
      objectName = Just "NewZealandObject"
      propName = "newZealandText"
      propValueCodec = StringCodec Nothing
      doco = Just "Must be said in an NZ accent"
```