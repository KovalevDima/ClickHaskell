<h1> Documentation </h1>

It's a ClickHaskell documentation compiler code\
which is powered by [Hakyll static compiler](https://hackage.haskell.org/package/hakyll)

You can **start** live server on [http://127.0.0.1:8000](http://127.0.0.1:8000) via cabal\
```bash
cabal run documentation-compiler -- watch
```
or **build** static site via cabal or nix wrapper
```bash
cabal run documentation-compiler -- build
```
```bash
nix build .#documentation
```

<h1> Compiler </h1>

```haskell
{-# LANGUAGE OverloadedStrings #-}

module DocumentationCompiler where

import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import System.FilePath
  ( (</>), normalise, dropFileName, dropExtension
  , replaceExtension, takeBaseName, replaceFileName, dropTrailingPathSeparator
  )
import Hakyll
import Data.Bool (bool)
import Data.List (sort)

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith defaultConfiguration{providerDirectory="."} $ do

    match "template.html" $ compile templateCompiler

    let pattern =
          ("**.lhs" .||. "**.md" .||. "index.html")
          .&&. (complement "ChangeLog.md")
          .&&. (complement "README.md")

        -- tranforms file paths to actual links
        filePathToUrlPath filePath =
          case takeBaseName filePath of
            "README" -> replaceFileName filePath "index.html"
            _        -> replaceExtension filePath "html"

        beautifyUrl path =
          ( dropTrailingPathSeparator
          . normalise . ("/" </>)
          . bool id dropFileName (takeBaseName path == "index")
          ) path

    -- documentation references compilation
    match pattern $ do
      route (customRoute $ filePathToUrlPath . toFilePath)
      compile $ do
        -- load all used file paths to pass it into <nav>
        navigation <-
          traverse (makeItem) . sort . map (beautifyUrl . filePathToUrlPath . toFilePath)
            =<< getMatches pattern

        -- compile every file
        getResourceBody
          >>=
            (\item ->
              if (toFilePath . itemIdentifier) item `elem` migratingFiles
              then pure item
              else renderPandoc item
            )
          >>=
            loadAndApplyTemplate
              "template.html"
              (defaultContext <> mkNavigationCtx navigation)
          >>= relativizeUrls

    match "./assets/**" $ do
      route idRoute
      compile copyFileCompiler

migratingFiles :: [String]
migratingFiles = ["./usage/select/index.lhs"]

mkNavigationCtx :: [Item FilePath] -> Context String
mkNavigationCtx navigation =
  listField
    "nav"
    (mconcat
      [ field "link"     (pure . itemBody)
      , field "linkName" (pure . dropExtension . itemBody)
      ]
    )
    (pure navigation)
```
