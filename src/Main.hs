{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import CMark (commonmarkToNode)
import CMark qualified
import Conduit (foldC, iterMC, mapMC, runConduitRes, sourceDirectoryDeep, (.|))
import Control.Exception (throwIO)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (find)
import Data.Functor (($>))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.IO qualified as LText
import Data.Time (Day, UTCTime (UTCTime))
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath
  ( joinPath,
    makeRelative,
    replaceExtension,
    splitPath,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, docTypeHtml, preEscapedToHtml)
import Text.Hamlet (xshamlet)
import Text.Hamlet.XML (xml)
import Text.XML qualified as Xml

sourceDir :: FilePath
sourceDir = "content"

outputDir :: FilePath
outputDir = "public"

blogTitle :: Text
blogTitle = "Lae's Blog"

blogLink :: Text
blogLink = "https://lae.nz/"

blogAuthor :: Text
blogAuthor = "Lae Chen"

recentPostsLimit :: Int
recentPostsLimit = 10

data Paths = Paths
  { sourceFile :: FilePath,
    outputFile :: FilePath
  }
  deriving (Eq, Ord)

data Page = Page
  { title :: Text,
    doc :: Tree CMark.NodeType,
    paths :: Paths
  }
  deriving (Eq, Ord)

data Post = Post
  { date :: Day,
    page :: Page
  }
  deriving (Eq, Ord)

data Resource
  = Static Paths
  | PostRef Post
  | PageRef Page

data PostEntry = PostEntry
  { date :: Day,
    title :: Text,
    link :: Text
  }
  deriving (Eq, Ord)

data ProcessResult = ProcessResult
  { postEntries :: Set PostEntry,
    recentPosts :: Set Post
  }

instance Semigroup ProcessResult where
  x <> y =
    ProcessResult
      { postEntries = Set.union x.postEntries y.postEntries,
        recentPosts =
          Set.take recentPostsLimit $
            Set.union x.recentPosts y.recentPosts
      }

instance Monoid ProcessResult where
  mempty = ProcessResult mempty mempty

loadResource :: (MonadIO m, MonadFail m) => FilePath -> m Resource
loadResource sourceFile = liftIO $ do
  let outputFile = outputDir </> makeRelative sourceDir sourceFile
  let paths = Paths {sourceFile, outputFile}
  if takeExtension sourceFile == ".md"
    then loadMarkdown paths
    else pure $ Static paths
  where
    loadMarkdown paths = do
      doc <- mdFromNode . commonmarkToNode [] <$> Text.readFile sourceFile
      title <- maybe (fail $ sourceFile ++ ": no title defined") pure $ mdGetTitle doc
      let page = Page {doc, title, paths}
      if "posts" `List.isPrefixOf` makeRelative sourceDir sourceFile
        then PostRef . flip Post page <$> parseDate
        else pure $ PageRef page

    parseDate =
      maybe
        (fail $ sourceFile ++ ": no ISO-8601 date in name")
        pure
        . iso8601ParseM
        . take 10
        . takeFileName
        $ sourceFile

processResource :: (MonadIO m) => Resource -> m ProcessResult
processResource = \case
  Static paths -> processStatic paths $> mempty
  PageRef page -> processMarkdown page $> mempty
  PostRef post@Post {date} -> do
    page@Page {title, paths = Paths {outputFile}} <- processMarkdown post.page
    pure $
      ProcessResult
        { recentPosts = Set.singleton (post {page}),
          postEntries =
            Set.singleton
              ( PostEntry
                  { date,
                    title,
                    link = Text.pack $ makeRelative outputDir outputFile
                  }
              )
        }

processStatic :: (MonadIO m) => Paths -> m ()
processStatic Paths {sourceFile, outputFile} = liftIO $ do
  createDirectoryIfMissing True (takeDirectory outputFile)
  copyFile sourceFile outputFile

processMarkdown :: (MonadIO m) => Page -> m Page
processMarkdown page@Page {title, paths} = liftIO $ do
  let outputFile = replaceExtension paths.outputFile ".html"
  let doc = rewrite page.doc
  createDirectoryIfMissing True (takeDirectory outputFile)
  LText.writeFile outputFile . renderHtml $ toHtml outputFile doc
  pure $ page {doc, paths = paths {outputFile}}
  where
    toHtml output =
      htmlTemplate output title
        . preEscapedToHtml
        . CMark.nodeToHtml []
        . mdToNode

    rewrite = fmap $ \case
      CMark.LINK url t -> CMark.LINK (rewriteLink url) t
      x -> x

    rewriteLink url =
      if not ("://" `Text.isInfixOf` url)
        then maybe url (<> ".html") (Text.stripSuffix ".md" url)
        else url

htmlTemplate :: FilePath -> Text -> Html -> Html
htmlTemplate outputFile title content = docTypeHtml $ do
  [xshamlet|
    <head>
      <meta charset="utf-8">
      <meta content="width=device-width,initial-scale=1">
      <link rel="stylesheet" type="text/css" href=#{pathToRoot </> "default.css"}>
      <title>#{title}
    <body>
      <div id="nav">
        <p>
          <a href=#{pathToRoot </> "index.html"}>home
          |
          <a href=#{pathToRoot </> "index.xml"}>rss
          |
          <a href=#{pathToRoot </> "about.html"}>about
      <div id="content">#{content}
  |]
  where
    pathToRoot =
      let relPath = makeRelative outputDir outputFile
          parentCount = length (splitPath relPath) - 1
          parents = replicate parentCount ".."
       in joinPath parents

buildHomePage :: (MonadIO m) => Set PostEntry -> m ()
buildHomePage posts = liftIO $ do
  let outputFile = outputDir </> "index.html"
  LText.writeFile outputFile . renderHtml . htmlTemplate outputFile blogTitle $
    [xshamlet|
      <h1 class="title">#{blogTitle}
      <table class="posts">
        <tbody>
          $forall (PostEntry {date, title, link}) <- Set.toDescList posts
            <tr>
              <td class="date">
                <time datetime=#{iso8601Show date}>#{iso8601Show date}
              <td>
                <a href=#{link}>#{title}
    |]

-- See https://validator.w3.org/feed/docs/atom.html
-- and https://validator.w3.org/feed/#validate_by_input
buildAtomFeed :: (MonadIO m) => Set Post -> m ()
buildAtomFeed posts = liftIO $ do
  feed <- buildFeed
  Xml.writeFile Xml.def (outputDir </> "index.xml") $
    Xml.Document (Xml.Prologue [] Nothing []) feed []
  where
    buildFeed = do
      entries <- concat <$> mapM buildEntry (Set.toDescList posts)
      let updated =
            Text.pack . iso8601Show $
              UTCTime (maybe (toEnum 0) (\p -> p.date) (Set.lookupMax posts)) 0
      pure $
        Xml.Element
          "feed"
          (Map.singleton "xmlns" "http://www.w3.org/2005/Atom")
          [xml|
            <id>#{blogLink}
            <title>#{blogTitle}
            <link href=#{blogLink}>
            <link href=#{blogLink <> "index.xml"} rel="self">
            <updated>#{updated}
            <author>
              <name>#{blogAuthor}
            ^{entries}
          |]

    buildEntry (Post {date, page = Page {title, doc, paths = Paths {outputFile}}}) = do
      let link = (blogLink <>) . Text.pack $ makeRelative outputDir outputFile
      let updated = Text.pack . iso8601Show $ UTCTime date 0
      content <-
        either throwIO pure . Xml.parseText Xml.def . LText.fromStrict $
          "<div>"
            <> CMark.nodeToHtml [] (mdToNode $ rewrite outputFile doc)
            <> "</div>"
      pure
        [xml|
          <entry>
            <id>#{link}
            <link href=#{link}>
            <title>#{title}
            <updated>#{updated}
            <content type="xhtml">
              <div xmlns="http://www.w3.org/1999/xhtml">
                ^{content.documentRoot.elementNodes}
        |]

    rewrite outputFile = fmap $ \case
      CMark.LINK url title -> CMark.LINK (rewriteLink outputFile url) title
      CMark.IMAGE url title -> CMark.IMAGE (rewriteLink outputFile url) title
      x -> x

    rewriteLink outputFile url =
      if "://" `Text.isInfixOf` url
        then url
        else
          blogLink
            <> (Text.pack . takeDirectory . makeRelative outputDir $ outputFile)
            <> "/"
            <> url

mdFromNode :: CMark.Node -> Tree CMark.NodeType
mdFromNode (CMark.Node _ val children) = Tree.Node val (mdFromNode <$> children)

mdToNode :: Tree CMark.NodeType -> CMark.Node
mdToNode (Tree.Node val children) = CMark.Node Nothing val (mdToNode <$> children)

mdGetTitle :: Tree CMark.NodeType -> Maybe Text
mdGetTitle (Tree.Node (CMark.HEADING 1) children) =
  pure
    . (foldMap . foldMap) (\case CMark.TEXT t -> t; _ -> "")
    $ children
mdGetTitle (Tree.Node _ children) =
  join
    . find isJust
    . fmap mdGetTitle
    $ children

main :: IO ()
main = do
  createDirectoryIfMissing False outputDir
  result <-
    runConduitRes $
      sourceDirectoryDeep False sourceDir
        .| iterMC (liftIO . putStrLn . ("Processing " ++))
        .| mapMC loadResource
        .| mapMC processResource
        .| foldC
  buildHomePage result.postEntries
  buildAtomFeed result.recentPosts
