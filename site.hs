{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import qualified Text.Pandoc.Shared as Pandoc (headerShift)
import           Data.List (isInfixOf)
import           System.FilePath.Posix (splitFileName, takeBaseName, takeDirectory, (</>))


main :: IO ()
main =
  hakyll $ do
    match ("images/**/*" .||. "fonts/*" .||. "fi/**/*.pdf") $ do
      route   idRoute
      compile copyFileCompiler

    match "style.css" $ do
      route   idRoute
      compile compressCssCompiler

    match ("e404.html" .||. "fi/**/*.html") $ do
      route idRoute
      compile $
        getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "posts/*" $ do
      route niceRoute
      compile $ postPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= removeIndexHtml
        >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx = listField "posts" postCtx (return posts)
                    <> defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= removeIndexHtml
          >>= relativizeUrls

    match "templates/*.html" $
      compile $ templateBodyCompiler


postCtx :: Context String
postCtx =
     dateField "date" "%B %Y"
  <> defaultContext


postPandocCompiler :: Compiler (Item String)
postPandocCompiler =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (Pandoc.headerShift 1)


-- http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      let p = toFilePath ident
       in takeDirectory p </> takeBaseName p </> "index.html"

removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") | isLocal dir -> dir
        _                                 -> url
        where isLocal uri = not ("://" `isInfixOf` uri)
