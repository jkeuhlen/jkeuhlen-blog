---
layout: post
title: "Introduction to GHC Generics"
date: 2019-01-28
tags: haskell generics meta-programming
---
## What are Generics? 

GHC.Generics is a well supported library, included in base, that can be used to explore the structure of data types and perform meta-programming tasks. 

[//]: # (Add more introduction on the typeclass definition)
 

The following tutorial consists of examples which utilize GHC.Generics to create typeclasses that can provide information about the structure of a record type.

We'll create two type classes that are closely related. The first tells you how many fields exist in a record. The second tells you how many fields in a record contain a specific character.

This will be our example data structures. Generic is derived automatically for us by the compiler to generate the necessary type representations we'll be exploiting below. You will need to turn on the DeriveGeneric extension for this to work.

```haskell
data TestRowCounts = TestRowCounts {foo :: Text, bar :: Bool, baz :: Int} deriving (Generic, Show)

data TestRowCounts' = TestRowCounts' {foo' :: Text} deriving (Generic, Show)

data TestRowCounts2 = Foo TestRowCounts TestRowCounts | Bar TestRowCounts' deriving (Generic, Show)
```
 

### EXAMPLE 1 : How many fields does each record contain?

We'll start with a dummy class that will not be exposed externally. This is the class that will allow us to write the important instances out of GHC generics.

```haskell
class RowCounts' f where
  countRows' :: f p -> Int
```
[//]: # (FIXME: What is p)


V1 represents the uninhabitable phantom types. Obviously, you don't have any fields there so we can return 0
```haskell
instance RowCounts' V1 where
  countRows' _ = 0
```

U1 represents the Unit type which corresponds to the single value (). It also is not a record, so return 0.
```haskell
instance RowCounts' U1 where
  countRows' _ = 0
```
:+: is the sum type, meaning combining two other fields. | in an ADT or `Either` are good mental comparisons to have. In this case, we just count either case separately. Just like `Either`, we have two data constructors: `L1` and `R1`. 
```haskell
instance (RowCounts' f, RowCounts' g) => RowCounts' (f G.:+: g) where
  countRows' (L1 x) = countRows' x
  countRows' (R1 x) = countRows' x
```

Dually, :*: is the product type, it combines fields in records and corresponds to `(,)` at the type level. Much like the constructor for tuple, `:*:` is also the data constructor for the type. 
```haskell
instance (RowCounts' f, RowCounts' g) => RowCounts' (f :*: g) where
  countRows' (x :*: y) = countRows' x + countRows' y
```
M1 is the higher meta data level that tells us about the various levels of data contained within a record. We don't need it here, so just unwrap and pass the underlying structure to countRows'

```haskell
instance RowCounts' f => RowCounts' (M1 i t f) where
  countRows' (M1 x) = countRows' x
```
Here is the meat, if you hit a K1 constructor, you've hit a record field so return the count at this level (1). Count on the product instance to do the summation for you.

[//]: # (EXTEND: this could be unwrapped further to handle the ADT case better. K1 may be Foo | Bar in the above example which will only give you the number of records in the top half of the ADT, not sum all the way down through it's fields. If you wanted, you could extend this instance to go all the way to non-record fields.)


```haskell
instance RowCounts' (K1 i c) where
  countRows' _ = 1
```
 

Here is our exposed typeclass.
```haskell
class RowCounts a where
  countRows :: a -> Int
  default countRows :: (Generic a, RowCounts' (Rep a)) => a -> Int
  countRows x = countRows' (from x)
``` 
[//]: # (FIXME: add more details on what the default signature does)

This tells us that anything with Generic is going to have RowCounts.
```haskell
instance (Generic a, RowCounts' (Rep a)) => RowCounts a
```
 

### EXAMPLE 2 : How many fields contain a particular character?

Again, we have an internal typeclass to start with. We'll expose the normal version later.

```haskell
class RowCountChar' f where
  countRowsChar' :: Char -> f p -> Int
```

Phantom types still don't have field names so return 0

```haskell
instance RowCountChar' V1 where
  countRowsChar' _ _ = 0
```

Same deal with Unit

```haskell
instance RowCountChar' U1 where
  countRowsChar' _ _ = 0
```

Our Sum type instance is unchanged

```haskell
instance (RowCountChar' f, RowCountChar' g) => RowCountChar' (f G.:+: g) where
  countRowsChar' c (L1 x) = countRowsChar' c x
  countRowsChar' c (R1 x) = countRowsChar' c x
```
Our product type instance is unchanged
```haskell
instance (RowCountChar' f, RowCountChar' g) => RowCountChar' (f :*: g) where
  countRowsChar' c (x :*: y) = countRowsChar' c x + countRowsChar' c y
```

Here we're diving into the meta data section, but we're going to break it into the three levels: D, C, and S. 
  D provides meta data about a type. Which package did it come from? What type is it? Which package provided it? Is it just a newtype? 
  C provides meta data about the constructors. What is the constructor name? Is it an operator (prefix or infix notation)? Is it a record? 
  S tells about the selectors (fields). What is the selector name? Is it packed? Was it marked strict in source? Does GHC think it's strict?

In the D instance, we don't need any information at the top level, unwrap and pass it through.

```haskell
instance (RowCountChar' f) => RowCountChar' (M1 D s f) where
  countRowsChar' c (M1 x) = countRowsChar' c x
```
Same thing for C
```haskell
instance (RowCountChar' f) => RowCountChar' (M1 C s f) where
  countRowsChar' c (M1 x) = countRowsChar' c x
```
Now that we are at the selector level, we can ask about each field! Get the name of the field, and check if your character is in it.
```haskell
instance (RowCountChar' f, Selector s) => RowCountChar' (M1 S s f) where
  countRowsChar' c meta =
    let field = selName meta
    in if c `elem` field
        then
          1
        else
          0
```
In this case, you've hit a value that isn't a record. Just return 0
```haskell
instance RowCountChar' (K1 i c) where
  countRowsChar' c _ = 0
```
 

Again, expose the real typeclass that we want to use.

```haskell
class RowCountChar a where
  countRowsChar :: Char -> a -> Int
  default countRowsChar :: (Generic a, RowCountChar' (Rep a)) => Char -> a -> Int
  countRowsChar c x = countRowsChar' c (from x)

instance (Generic a, RowCountChar' (Rep a)) => RowCountChar a
```
### Example 3: Generics in Practice

[Aeson](http://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html) is the defacto library for working with JSON in haskell. Because serialization back and forth between JSON and haskell datatypes is so standardized, it's a perfect place to utilize boilerplate reducing meta programming. Aeson conveniently provides your choice of meta programming tools: template haskell or generics. 

Let's look at their implementation of generics as a real-world example of its usage. 

When you want to use generics to generate your To/From JSON instances, you first need to define your data type deriving generic. 

```haskell
data SpecialRecord = 
  SpecialRecord { 
    specialName  :: Text
  , specialFlag  :: Bool 
  , specialCount :: Int
  } deriving (Generic)
```

Then, when you define your instances, you can either use the default signature the typeclasses defined or specify your own custom options set:
```haskell
instance ToJSON SpecialRecord
instance FromJSON SpecialRecord

~OR~

instance ToJSON SpecialRecord where 
  toJSON = genericToJSON defaultOptions -- ^ Modify these options to do things like enforce snake case, or to drop the "special" in each record name
instance FromJSON SpecialRecord where 
  parseJSON = genericParseJSON defaultOptions
```

So how does Aeson make this work? Let's dive into the [source](http://hackage.haskell.org/package/aeson-1.4.2.0/docs/src/Data.Aeson.html) code for [`genericToJSON`](http://hackage.haskell.org/package/aeson-1.4.2.0/docs/src/Data.Aeson.Types.ToJSON.html#genericToJSON):
```haskell
-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON Value Zero (Rep a))
              => Options -> a -> Value
genericToJSON opts = gToJSON opts NoToArgs . from
```

So first thing to know, is Aeson makes use of an intermediary typeclass called [`GToJSON`](http://hackage.haskell.org/package/aeson-1.4.2.0/docs/src/Data.Aeson.Types.ToJSON.html#GToJSON). This typeclass does most of the heavy lifting, and is where the generic instances we want to look at are located. Aeson is fairly well commented, so if you really want to understand what's happening under the hood, dig into the source code!

```haskell
--------------------------------------------------------------------------------
-- Generic toJSON

-- Note: Refactoring 'ToJSON a' to 'ToJSON enc a' (and 'ToJSON1' similarly) is
-- possible but makes error messages a bit harder to understand for missing
-- instances.

instance GToJSON Value arity V1 where
    -- Empty values do not exist, which makes the job of formatting them
    -- rather easy:
    gToJSON _ _ x = x `seq` error "case: V1"
    {-# INLINE gToJSON #-}

instance ToJSON a => GToJSON Value arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts _ = toJSON . unK1
    {-# INLINE gToJSON #-}

instance ToJSON1 f => GToJSON Value One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToJSON1 instance:
    gToJSON _opts (To1Args tj tjl) = liftToJSON tj tjl . unRec1
    {-# INLINE gToJSON #-}

instance GToJSON Value arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ _ = emptyArray
    {-# INLINE gToJSON #-}

instance ( WriteProduct arity a, WriteProduct arity b
         , ProductSize        a, ProductSize        b
         ) => GToJSON Value arity (a :*: b)
  where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'writeProduct':
    gToJSON opts targs p =
        Array $ V.create $ do
          mv <- VM.unsafeNew lenProduct
          writeProduct opts targs mv 0 lenProduct p
          return mv
        where
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize
    {-# INLINE gToJSON #-}

instance ( ToJSON1 f
         , GToJSON Value One g
         ) => GToJSON Value One (f :.: g)
  where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToJSON opts targs =
      let gtj = gToJSON opts targs in
      liftToJSON gtj (listValue gtj) . unComp1
    {-# INLINE gToJSON #-}
```

