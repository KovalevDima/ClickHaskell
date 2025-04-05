<h1> Documentation </h1>

It's a ClickHaskell documentation compiler code <br>
which is powered by <a href="https://hackage.haskell.org/package/hakyll"> Hakyll static compiler </a><br>

You can <b>start</b> live server on <a href="http://127.0.0.1:8000">http://127.0.0.1:8000</a> via cabal<br>

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

<h1>Compiler</h1>

<pre><code data-lang="haskell" class="haskell"
>{-# LANGUAGE OverloadedStrings #-}

module DocumentationCompiler where

import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import System.FilePath (replaceExtension, takeBaseName, replaceFileName)
import Hakyll

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith defaultConfiguration{providerDirectory="."} $ do

    match "template.html" $ compile templateCompiler

    let pattern = ("**.lhs" .||. "**.html")
        filePathToUrlPath filePath =
          case takeBaseName filePath of
            "README" -> replaceFileName filePath "index.html"
            _        -> replaceExtension filePath "html"

    -- documentation references compilation
    match pattern $ do
      route (customRoute $ filePathToUrlPath . toFilePath)
      compile $ do
        getResourceBody
          >>= loadAndApplyTemplate "template.html" defaultContext
          >>= relativizeUrls

    match "./assets/**" $ do
      route idRoute
      compile copyFileCompiler
