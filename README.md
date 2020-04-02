# ejdb2haskell

ejdb2haskell is a Haskell binding to [ejdb2](https://github.com/Softmotions/ejdb), an embeddable JSON Database engine C library.

## Requirements

**libejdb2**

Install libejdb2 by installing [ejdb2](https://github.com/Softmotions/ejdb#use-cases)

## Build

```bash
cabal build
```

## Usage

Every API are tested, so have a look to test folder.

```haskell
putNewTest :: IO Database -> TestTree
putNewTest databaseIO = testCase "putNewTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    storedPlant <- getById database "plants" id
    storedPlant @?= Just plant
  where
    plant = Plant { name        = Just "pinus"
                  , isTree      = Just True
                  , year        = Just 1753
                  , description = Just "wow 🌲"
                  }

getListTest' :: IO Database -> TestTree
getListTest' databaseIO = testCase "getList'" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[isTree=:tree] | asc /name"
    Query.setBool False "tree" query
    plants <- getList' database query
    plants @?= [ Just nothingPlant { id          = Just 2
                                   , name        = Just "gentiana brentae"
                                   , isTree      = Just False
                                   , year        = Just 2008
                                   , description = Just "violet 🌺flower"
                                   , ratio       = Nothing
                                   }
               , Just nothingPlant { id          = Just 3
                                   , name        = Just "leontopodium"
                                   , isTree      = Just False
                                   , year        = Just 1817
                                   , description = Just "tipical alpine flower"
                                   , ratio       = Nothing
                                   }
               , Just nothingPlant { id          = Just 4
                                   , name        = Just "leucanthemum vulgare"
                                   , isTree      = Just False
                                   , year        = Just 1778
                                   , description =
                                         Just "very common flower in Italy 🍕"
                                   , ratio       = Just 1.618
                                   }
               ]
```
