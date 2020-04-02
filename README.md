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
```
