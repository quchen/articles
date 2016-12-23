{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import Data.Monoid
import Hakyll



main :: IO ()
main = hakyllWith config allRules

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "gen"
    , tmpDirectory = ".hakyll/temp"
    , storeDirectory = ".hakyll/cache"
    }

allRules :: Rules ()
allRules = do
    compileTemplates
    compileStylesheets
    compileArticles
    compileIndexHtml
    compileRss

compileTemplates :: Rules ()
compileTemplates = match "templates/*" (compile templateCompiler)

compileStylesheets :: Rules ()
compileStylesheets = match "css/***.css" (do
    route idRoute
    compile copyFileCompiler )

compileArticles :: Rules ()
compileArticles = match "articles/***.md" (do
    route (setExtension "html")
    compile (pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/main.html" defaultContext
        >>= relativizeUrls ))

compileIndexHtml :: Rules ()
compileIndexHtml = create ["index.html"] (do
    route idRoute
    compile (do
        articles <- do
            myPosts <- loadAll "articles/*"
            recentFirst myPosts

        postEntryName <- do
            postItemTemplate <- loadBody "templates/postitem.html"
            applyTemplateList postItemTemplate defaultContext articles

        makeItem ""
            >>= loadAndApplyTemplate "templates/articles.html"
                    (constField "articles" postEntryName <> defaultContext)
            >>= loadAndApplyTemplate "templates/main.html"
                    (constField "title" "Quchen’s articles" <> defaultContext)
            >>= relativizeUrls ))

compileRss :: Rules ()
compileRss = create ["articles.rss"] (do
    route idRoute
    compile (do
        articles <- loadAllSnapshots "articles/*" "content"
                        >>= recentFirst
                        >>= traverse relativizeUrls
        renderRss rssConfig
                  (bodyField "description" <> defaultContext)
                  (take 10 articles) ))

rssConfig :: FeedConfiguration
rssConfig = FeedConfiguration
        { feedTitle = "Quchen's articles"
        , feedDescription = "Articles by Quchen, mostly about Haskell."
        , feedAuthorName = "David Luposchainsky aka Quchen"
        , feedRoot = "http://quchen.github.io"
        , feedAuthorEmail = "dluposchainsky at google"
        }
