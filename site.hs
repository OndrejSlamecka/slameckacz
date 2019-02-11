{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, (<>))
import Data.Maybe (isJust)
import Data.Map.Strict hiding (fromList)
import qualified Data.Map.Strict
import Hakyll
import Hakyll.Web.Html (stripTags)
import Hakyll.Web.Sass (sassCompiler)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))


-- Nice routes
-- http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = if baseName == "index"
      then path
      else takeDirectory path </> baseName </> "index.html"
      where
        path = toFilePath ident
        baseName = takeBaseName path


-- Create title tag from page title
titleTagContext :: Context a
titleTagContext = field "titleTag" $ \item -> do
    fieldTitle <- getMetadataField (itemIdentifier item) "title"
    isIndex    <- isJust <$> getMetadataField (itemIdentifier item) "isIndex"
    let value = case (fieldTitle, isIndex) of
                  (Just t, False)  -> stripTags t `mappend` " | "
                  _ -> ""
    return $ value `mappend` "Ondřej Slámečka"


myDefaultContext = titleTagContext <> defaultContext


-- Distinguish different types of content
data FileCategory = SCSS | HTML | Errors | Templates
  deriving (Eq, Ord)


-- Patterns for different filetypes
matchMap :: Map FileCategory Pattern
matchMap = Data.Map.Strict.fromList
  [ (SCSS, "css/*.scss")
  , (HTML, ("*.html" .||. "**/*.html") .&&. complement (templates .||. errors))
  , (Errors, errors)
  , (Templates, templates)
  ]
  where
    templates = "templates/*"
    errors = "e404.html"


-- Pattern for files not matched by any pattern in matchMap
unmatched :: Pattern
unmatched = complement $ foldr1 (.||.) matchMap


-- Pattern for files which should be ignored
avoid :: Pattern
avoid = fromList ["s5upload.yml", "site.hs", "stack.yaml", "slameckacz.cabal", "package.yaml"]


-- Compiles an HTML page (selects given layout and uses default context)
compilePage = compile $ do
  file <- getUnderlying
  layoutMay <- getMetadataField file "layout"

  page <- getResourceBody >>= applyAsTemplate myDefaultContext
  case layoutMay of
    Nothing     -> return page
    Just layout -> loadAndApplyTemplate (template layout) myDefaultContext page

  where
    -- Return file identifier given a template name
    template layout = fromFilePath $ "templates/" <> layout <> ".html"


main :: IO ()
main = hakyll $ do
  match (unmatched .&&. complement avoid) $ do
    route   idRoute
    compile copyFileCompiler

  match (matchMap ! SCSS) $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match (matchMap ! Errors) $ do
    route idRoute
    compilePage

  match (matchMap ! HTML) $ do
    route niceRoute
    compilePage

  match (matchMap ! Templates) $ compile templateBodyCompiler
