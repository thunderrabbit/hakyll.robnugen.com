module Main where

-- [HP] http://hackage.haskell.org/package/base
import Control.Applicative ((<$>))
import Data.Monoid ((<>), mempty)

-- [HP] http://hackage.haskell.org/package/filepath
import System.FilePath (splitFileName)

-- [HP] http://hackage.haskell.org/package/haskell2010
import Data.List (isInfixOf)

-- http://hackage.haskell.org/package/hakyll
import Hakyll

-- http://hackage.haskell.org/package/tagsoup
import qualified Text.HTML.TagSoup as TS (Tag(TagOpen))

------------------------------------------------------------------------------
-- configuration

-- | Site configuration: used to define the default context
siteConfig :: [(String, String)]
siteConfig =
    [ ("sTitle",       "Keep Pushing the Limits")
    , ("mAuthor",      "Rob Nugen, <hakyll.robnugen.com@robnugen.com>")
    , ("mDescription", "blatherings of Rob Nugen; don't read unless bored.")
    , ("mKeywords",    "blah, blah, blah")
    ]

-- | Hakyll configuration: used to define deployment commands
hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { deployCommand = rsync ++ "_site/ ~/public; " ++
                      rsync ++ "_site/ hakyll:/usr/share/nginx/html"
    }
  where
    rsync :: String
    rsync = "rsync -avcz --no-p --no-g --chmod=a+r --checksum "

-- | Feed configuration
feedConfig :: String -> FeedConfiguration
feedConfig subtitle = FeedConfiguration
    { feedTitle       = "Rob Nugen - " ++ subtitle
    , feedDescription = "blatherings of Rob Nugen; don't read unless bored."
    , feedAuthorName  = "Rob Nugen - thunderrabbit"
    , feedAuthorEmail = "hakyll.robnugen.com@robnugen.com"
    , feedRoot        = "http://hakyll.robnugen.com"
    }

------------------------------------------------------------------------------
-- main

main :: IO ()
main = hakyllWith hakyllConfig $ do
    -- Copy static files.
    match ("images/**" .||. "static/**") $ do
        route $ gsubRoute "^static/" (const "")
        compile copyFileCompiler

    -- Copy and compress the css.
    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    -- Compile each blog post and save a teaser for later use.
    match (posts .||. travels .||. "*.markdown") $ do
        route   $ gsubRoute "posts/" (const "") `composeRoutes`
                  markdownToFolderRoute
        compile $ pandocCompiler
          >>= return . fmap (demoteHeaders . mkPreFocusable)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html"    dateCtx
          >>= saveSnapshot "post"
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= removeIndexHtml
          >>= relativizeUrls

    -- With the teasers, create an rss feed for the most recent 15 posts.
    create ["recent.rss"] $ do
        route idRoute
        compile $ loadAllSnapshots posts "content"
          >>= (take 15 <$>) . recentFirst
          >>= renderAtom (feedConfig "All posts")
                  (teaserField "description" "content"
                <> bodyField "description" 
                <> defaultContext)
          >>= removeIndexHtml

    -- With all posts in a list, create the archive. TODO: paginate.
    create ["archive/index.html"] $ do
        route idRoute
        compile $ makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/archive.html"
              (listField "posts" dateCtx
                (loadAllSnapshots posts "content" >>= recentFirst)
              <> archiveCtx)
          >>= loadAndApplyTemplate "templates/default.html"
                (archiveCtx <> defaultCtx)
          >>= removeIndexHtml
          >>= relativizeUrls

    -- Compile the latest 10 posts into teasers. TODO: paginate.
    create ["index.html"] $ do
        route idRoute
        compile $ makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/index.html"
              (listField "posts" (dateCtx <> teaserCtx)
                (loadAllSnapshots posts "content" >>= recentFirst)
                <> constField "index" "true"
                <> constField "title" "Home")
          >>= loadAndApplyTemplate "templates/default.html"
                  (constField "title" "Blog" <> dateCtx <> defaultCtx)
          >>= removeIndexHtml
          >>= relativizeUrls

    -- Compile the templates
    match "templates/*" $ compile templateCompiler
  where
    travels   = "travels/**.markdown"
    posts      = "posts/**.markdown"
    archiveCtx = constField  "title"  "Archives"
    dateCtx    = dateField   "date"   "%B %e, %Y"
                 <> dateField "Tdate" "%Y-%m-%d"
                 <> defaultContext
    defaultCtx = listToFields siteConfig          <> defaultContext
    teaserCtx  = teaserField "teaser" "content"   <> defaultContext

-- | Make all <pre>'s focusable by adding the attribute tabindex="0".
mkPreFocusable :: String -> String
mkPreFocusable = withTags process
  where
    process :: TS.Tag String -> TS.Tag String
    process (TS.TagOpen "pre" a) = TS.TagOpen "pre" $ ("tabindex", "0") : a
    process t                    = t

listToFields :: [(String, String)] -> Context String
listToFields ((k, v):xs) = constField k v <> listToFields xs
listToFields _           = mempty

-- | Replace .markdown with /index.html, creating a folder for the page.
markdownToFolderRoute :: Routes
markdownToFolderRoute = gsubRoute ".markdown" (const "/index.html")

-- | Remove index.html from all links.
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml = return . (withUrls removeIndexStr <$>)
  where removeIndexStr :: String -> String
        removeIndexStr u = case splitFileName u of
            (d, "index.html") | isLocal d -> d
            _                             -> u
        isLocal :: String -> Bool
        isLocal = not . isInfixOf "://"
