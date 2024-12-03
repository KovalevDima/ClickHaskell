{-# LANGUAGE OverloadedStrings #-}

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>), takeDirectory, takeFileName, replaceExtensions, replaceExtension)
import Hakyll
import Hakyll.Core.Compiler.Internal
import Debug.Trace (traceShowId)
import Control.Monad ((<$!>))

configuration :: Configuration
configuration = defaultConfiguration{providerDirectory="QA"}

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith configuration $ do

    match "documentation-compiler/tmpl-*" $ compile templateCompiler

    let pattern = ("**.lhs" .||. "**.md")

    match pattern $ do
      let 
        filePathToUrlPath =
          (\filePath ->
            case takeFileName filePath of
              "README.html" -> takeDirectory filePath </> "index.html"
              _ -> filePath
          )

      route (customRoute $ filePathToUrlPath . toFilePath)
      compile $ do
        navigation <-
          map (filePathToUrlPath . (`replaceExtension` "html") . toFilePath)
          <$> getMatches pattern

        pandocCompiler
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-main.html" defaultContext
          >>= relativizeUrls
