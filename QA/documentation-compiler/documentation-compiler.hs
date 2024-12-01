{-# LANGUAGE OverloadedStrings #-}

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import Hakyll
import System.FilePath
import Debug.Trace (traceShowWith, traceShowId)

configuration :: Configuration
configuration = defaultConfiguration{providerDirectory = "QA"}

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith configuration $ do
    match "./README.md" $ do
      route (constRoute "index.html")
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "compiler/tmpl-main.html" defaultContext
          >>= relativizeUrls

    match
      (    ("**.lhs" .||. "**.md")
      .&&. complement "**/README.md"
      )
      (do
        route (setExtension "html")
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "compiler/tmpl-code.html" defaultContext
            >>= loadAndApplyTemplate "compiler/tmpl-main.html" defaultContext
            >>= relativizeUrls
      )

    match "**/README.md"
      $ do
      route $ customRoute ((</> "index.html") . takeDirectory . toFilePath)
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "compiler/tmpl-main.html" defaultContext
          >>= relativizeUrls

    match "compiler/tmpl-*" $ compile templateCompiler
