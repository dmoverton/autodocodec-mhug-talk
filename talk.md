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
