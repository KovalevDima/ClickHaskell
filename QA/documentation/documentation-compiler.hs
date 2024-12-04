{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>), takeFileName, replaceExtension, takeBaseName, normalise, dropFileName, replaceFileName)
import Hakyll

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith defaultConfiguration{providerDirectory="QA"} $ do

    match "documentation/tmpl-*" $ compile templateCompiler

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
          >>= loadAndApplyTemplate "documentation/tmpl-code.html" defaultContext
          >>= loadAndApplyTemplate "documentation/tmpl-main.html" (defaultContext <> mkNavigationCtx navigation)
          >>= relativizeUrls

data NavigationLink = MkNavigationLink { link :: FilePath }

mkNavigationCtx :: [Item NavigationLink] -> Context String
mkNavigationCtx navigation = listField "nav" (mconcat [field "link" (pure . link . itemBody)]) (pure navigation)
