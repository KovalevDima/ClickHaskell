{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import System.FilePath ( (</>), takeDirectory )
import Hakyll

configuration :: Configuration
configuration = defaultConfiguration{providerDirectory="QA"}

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith configuration $ do
    match "./README.md" $ do
      route (constRoute "index.html")
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-main.html" defaultContext
          >>= relativizeUrls

    let indexFilePattern    = "**/README.md" .||. "**/README.lhs"
        nonIndexFilePattern = "**.lhs" .||. "**.md"

    match
      (nonIndexFilePattern .&&. complement indexFilePattern)
      (do
        route (setExtension "html")
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "documentation-compiler/tmpl-code.html" defaultContext
            >>= loadAndApplyTemplate "documentation-compiler/tmpl-main.html" defaultContext
            >>= relativizeUrls
      )

    match indexFilePattern
      $ do
      route $ customRoute ((</> "index.html") . takeDirectory . toFilePath)
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-main.html" defaultContext
          >>= relativizeUrls

    match "documentation-compiler/tmpl-*" $ compile templateCompiler
