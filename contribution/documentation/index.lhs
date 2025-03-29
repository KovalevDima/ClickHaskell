<h1> Documentation </h1>

It's a ClickHaskell documentation compiler code <br>
which is powered by <a href="https://hackage.haskell.org/package/hakyll"> Hakyll static compiler </a>

You can <b>start</b> live server on <a href="http://127.0.0.1:8000"> http://127.0.0.1:8000</a> via cabal

<pre><code data-lang="bash" class="bash"
>cabal run documentation-compiler -- watch
</code></pre>

or <b>build</b> static site via cabal or nix wrapper

<pre><code data-lang="bash" class="bash"
>cabal run documentation-compiler -- build
</code></pre>

<pre><code data-lang="bash" class="bash"
>nix build .#documentation
</code></pre>

<h1> Compiler </h1>

<pre><code data-lang="haskell" class="haskell"
>{-# LANGUAGE OverloadedStrings #-}

module DocumentationCompiler where

import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import System.FilePath
  ( (&lt/&gt), normalise, dropFileName, dropExtension
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
          . normalise . ("/" &lt/&gt)
          . bool id dropFileName (takeBaseName path == "index")
          ) path

    -- documentation references compilation
    match pattern $ do
      route (customRoute $ filePathToUrlPath . toFilePath)
      compile $ do
        -- load all used file paths to pass it into nav tag
        navigation <-
          traverse (makeItem) . sort . map (beautifyUrl . filePathToUrlPath . toFilePath)
            =<< getMatches pattern

        -- compile every file
        getResourceBody
          >>=
            loadAndApplyTemplate
              "template.html"
              (defaultContext <> mkNavigationCtx navigation)
          >>= relativizeUrls

    match "./assets/**" $ do
      route idRoute
      compile copyFileCompiler

migratingFiles :: [String]
migratingFiles =
  [ "./contribution/begin.md"
  , "./contribution/documentation/index.lhs"
  , "./testing/README.lhs"
  , "./usage/generateRandom/index.lhs"
  , "./usage/insertInto/index.lhs"
  , "./usage/select/index.lhs"
  , "./usage/selectFromView/index.lhs"
  ]

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
</code></pre>
