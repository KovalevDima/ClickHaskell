{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import Hakyll
import System.FilePath

configuration :: Configuration
configuration = defaultConfiguration{providerDirectory = "documentation"}

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith configuration $ do
    match "README.md" $ do
      route (constRoute "index.html")
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "compiler/tmpl-default.html" defaultContext
          >>= relativizeUrls

    match "example**.lhs" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "compiler/tmpl-article.html" defaultContext
          >>= loadAndApplyTemplate "compiler/tmpl-default.html" defaultContext
          >>= relativizeUrls

    match "compiler/tmpl-*" $ compile templateCompiler
