{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Scheduler
import           Control.Exception
import           Data.Conduit (runConduit, (.|))
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Text.HTML.Scalpel
import           Text.Regex
import qualified Data.ByteString.Char8  as C
import qualified Data.Conduit.Binary as CB

getPageURLs :: Int -> IO [URL]
getPageURLs page = do
  let url = "https://danbooru.donmai.us/posts?page=" <> show page <> "&tags=dise"
  hrefs <- scrapeURL url (attrs "href" ("a" @: ["href" @=~ mkRegex "^/posts/[0-9]+.q=dise"]))
  case hrefs of
    Nothing -> do putStrLn ("抓取第 " <> show page <> " 页信息失败")
                  return []
    Just hrefs -> return (map ("https://danbooru.donmai.us" <>) hrefs)

getImageURL :: URL -> IO (Either URL URL)
getImageURL url = do
  putStrLn ("正在抓取页面: " <> url)
  mImg <- scrapeURL url (chroot ("li" @: ["id" @= "post-info-size"]) (attr "href" "a"))
  return $ case mImg of
    Just img -> Right img
    Nothing  -> Left url

downloadSave :: URL -> FilePath -> IO ()
downloadSave url path = runResourceT $ do
  req <- parseRequest url
  runConduit $ httpSource req getResponseBody .| CB.sinkFile path

main :: IO ()
main = do
  let maxPage = 14
      nThreads = 20
  postURLs <- withScheduler (ParN nThreads) $ \scheduler -> do
    forM [1..maxPage] (scheduleWork scheduler . retry 3 . getPageURLs)
  imageURLs <- withScheduler (ParN nThreads) $ \scheduler -> do
    forM (concat postURLs) (scheduleWork scheduler . retry 3 . getImageURL)
  withScheduler_ (ParN nThreads) $ \scheduler -> do
    forM (zip [1..] imageURLs) $ \(i, img) ->
      case img of
        Left url -> do
          putStrLn $ "!! 从页面抓取图片地址失败: " <> url
        Right img -> do
          scheduleWork scheduler $ do
            putStrLn $ "正在下载第 " <> show i <> " 张图片"
            retry 3 (downloadSave img (show i <> ".jpg"))

retry :: Int -> IO a -> IO a
retry 0 ma = ma
retry n ma = ma `catch` (\e -> putStrLn (show (e :: HttpException)) >> retry (n-1) ma)

