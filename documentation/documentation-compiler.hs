{-# LANGUAGE OverloadedStrings #-}

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import Hakyll
import System.FilePath

configuration :: Configuration
configuration = defaultConfiguration{providerDirectory = "."}

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith configuration $ do
    match "./documentation/README.md" $ do
      route (constRoute "index.html")
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "documentation/compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "documentation/compiler/tmpl-main.html" defaultContext
          >>= relativizeUrls

    match (
      foldr1 (.||.)
        [ "documentation/*.md"
        , "documentation/user/**.lhs"
        , "documentation/developer/**.md"
        ]
      ) $ do
      route (setExtension "html")
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "documentation/compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "documentation/compiler/tmpl-main.html" defaultContext
          >>= relativizeUrls

    match "documentation/compiler/tmpl-*" $ compile templateCompiler
