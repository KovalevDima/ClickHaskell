{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>), takeFileName, replaceExtension, takeBaseName, normalise, dropFileName, replaceFileName)
import Hakyll

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
        filePathToUrlPath filePath =
          case takeBaseName filePath of
            "README" -> replaceFileName filePath "index.html"
            _        -> replaceExtension filePath "html" 

        trailIndexHtml path
          | takeFileName path == "index.html" = dropFileName path
          | otherwise                         = path

      route (customRoute $ filePathToUrlPath . toFilePath)
      compile $ do
        navigation <-
          traverse (makeItem . MkNavigationLink . normalise . ("/" </>) . trailIndexHtml . filePathToUrlPath . toFilePath)
            =<< getMatches pattern

        pandocCompiler
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "documentation-compiler/tmpl-main.html" (defaultContext <> listField "nav" mkLinkCtx (pure navigation))
          >>= relativizeUrls

data NavigationLink = MkNavigationLink { link :: FilePath }

mkLinkCtx :: Context NavigationLink
mkLinkCtx = mconcat [field "link" (pure . link . itemBody)]
