module Main where

-- [HP] http://hackage.haskell.org/package/base
import Control.Exception (catch)
import Data.Monoid ((<>), mempty)

-- [HP] http://hackage.haskell.org/package/directory
import System.Directory (setCurrentDirectory)

-- [HP] http://hackage.haskell.org/package/filepath
import System.FilePath (combine, splitFileName)

-- [HP] http://hackage.haskell.org/package/haskell2010
import Data.List (isInfixOf)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- http://hackage.haskell.org/package/hakyll
import Hakyll

-- http://hackage.haskell.org/package/tagsoup
import qualified Text.HTML.TagSoup as TS (Tag(TagOpen))

------------------------------------------------------------------------------
-- configuration

-- | Site configuration: used to define the default context
siteConfig :: Context String
siteConfig = toContext
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

-- | Title of index.html
indexTitle :: String
indexTitle = "Blog"

-- | Number of post teasers included on index.html
indexNumPosts :: Int
indexNumPosts = 10

-- | Number of post teasers included in the feed.
feedNumPosts :: Int
feedNumPosts = 15

------------------------------------------------------------------------------
-- rules

siteRules :: Rules ()
siteRules = do
    -- Compile templates.
    match "templates/*" $ compile templateCompiler

    -- Copy static files into the root.
    match "static/*" $ do
      route $ routeRemove "^static/"
      compile copyFileCompiler

    -- Copy image files.
    match "images/**" $ do
      route idRoute
      compile copyFileCompiler

    -- Copy and compress CSS files.
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    -- Compile blog posts, saving teasers in snapshot "posts".
    match "posts/**" $ do
      route $ routeRemove "^posts/" `composeRoutes` mdToDirRoute
      compile $ pandocCompiler
        >>= return . fmap (demoteHeaders . makePreFocusable)
        >>= saveSnapshot "posts"
        >>= loadAndApplyTemplate "templates/post.html" (dateCtx <> defaultCtx)
        >>= loadAndApplyTemplate "templates/default.html" defaultCtx
        >>= removeIndexHtml
        >>= relativizeUrls

    -- Compile travels content.
    match "travels/**" $ do
      route mdToDirRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultCtx
        >>= removeIndexHtml
        >>= relativizeUrls

    -- Compile root content.
    match ("about.md" .||. "contact.md" .||.
           "search.md" .||. "travels.md") $ do
      route mdToDirRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultCtx
        >>= removeIndexHtml
        >>= relativizeUrls

    -- Create index.html using the latest post teasers.
    create ["index.html"] $ do
      route idRoute
      compile $ makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/index.html"
              (listField "posts" (teaserCtx <> dateCtx <> defaultCtx)
                 (loadAllSnapshots "posts/**" "posts"
                    >>= fmap (take indexNumPosts) . recentFirst))
        >>= loadAndApplyTemplate "templates/default.html"
              (constField "title" indexTitle <> dateCtx <> defaultCtx)
        >>= removeIndexHtml
        >>= relativizeUrls

    -- Create archive/index.html showing all posts.  (temporary)
    create ["archive/index.html"] $ do
      route idRoute
      compile $ makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/archive.html"
              (listField "posts" (dateCtx <> defaultCtx)
                 (loadAllSnapshots "posts/**" "posts" >>= recentFirst)
               <> archiveCtx)
        >>= loadAndApplyTemplate "templates/default.html"
              (archiveCtx <> defaultCtx)
        >>= removeIndexHtml
        >>= relativizeUrls

    -- Create recent.rss using lastest post teasers.
    create ["recent.rss"] $ do
      route idRoute
      compile $ loadAllSnapshots "posts/**" "posts"
        >>= fmap (take feedNumPosts) . recentFirst
        >>= renderAtom (feedConfig "All posts")
              (teaserField "description" "posts"
               <> bodyField "description"
               <> defaultContext)
        >>= removeIndexHtml

  where
    defaultCtx, dateCtx, teaserCtx, archiveCtx :: Context String
    defaultCtx = siteConfig <> defaultContext
    dateCtx    = dateField "date" "%B %e, %Y" <> dateField "Tdate" "%Y-%m-%d"
    teaserCtx  = teaserField "teaser" "content"
    archiveCtx = constField "title" "Archive"

------------------------------------------------------------------------------
-- utility functions

-- | Create a Context String from a list of (String, String).
toContext :: [(String, String)] -> Context String
toContext ((k, v):xs) = constField k v <> toContext xs
toContext _           = mempty

-- | Remove part of a path from a route.
routeRemove :: String -> Routes
routeRemove s = gsubRoute s (const "")

-- | Route Markdown files to an index.html file in a directory.
mdToDirRoute :: Routes
mdToDirRoute = gsubRoute ".md" (const "/index.html")

-- | Make all pre tags focusable by adding the attribute tabindex="0".
makePreFocusable :: String -> String
makePreFocusable = withTags process
  where
    process :: TS.Tag String -> TS.Tag String
    process (TS.TagOpen "pre" a) = TS.TagOpen "pre" $ ("tabindex", "0") : a
    process t                    = t

-- | Remove index.html from all links.
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml = return . fmap (withUrls process)
  where
    process :: String -> String
    process u = case splitFileName u of
      (d, "index.html") | isLocal d -> d
      _                             -> u

    isLocal :: String -> Bool
    isLocal = not . isInfixOf "://"

------------------------------------------------------------------------------
-- main

main :: IO ()
main = do
    catch (cdHomeDir "journal") cdHomeDirErr
    hakyllWith hakyllConfig siteRules
  where
    cdHomeDir :: FilePath -> IO ()
    cdHomeDir subdir = do
      homeDir <- getEnv "HOME"
      setCurrentDirectory $ homeDir `combine` subdir

    cdHomeDirErr :: IOError -> IO ()
    cdHomeDirErr err = do
      hPutStrLn stderr "error: unable to change to project directory"
      hPutStrLn stderr $ show err
      exitFailure
