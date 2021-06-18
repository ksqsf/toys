{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception.Extra
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Scheduler
import           Data.Conduit (runConduit, (.|))
import           Data.IORef
import           Network.HTTP.Simple
import           Text.HTML.Scalpel
import           Text.Regex
import qualified Data.Conduit.Binary as CB

getPageURLs :: Int -> IO [URL]
getPageURLs page = do
  let url = "https://danbooru.donmai.us/posts?page=" <> show page <> "&tags=dise"
  hrefs <- scrapeURL url (attrs "href" ("a" @: ["href" @=~ mkRegex "^/posts/[0-9]+.q=dise"]))
  case hrefs of
    Nothing -> do putStrLn ("抓取第 " <> show page <> " 页信息失败")
                  return []
    Just hrefs -> return (map ("https://danbooru.donmai.us" <>) hrefs)

getImageURL :: URL -> IO (Maybe URL)
getImageURL url = do
  putStrLn ("正在抓取页面: " <> url)
  scrapeURL url (chroot ("li" @: ["id" @= "post-info-size"]) (attr "href" "a"))

downloadSave :: URL -> FilePath -> IO ()
downloadSave url path = runResourceT $ do
  req <- parseRequest url
  runConduit $ httpSource req getResponseBody .| CB.sinkFile path

main :: IO ()
main = do
  let maxPage = 14
      nThreads = 25
  cnt <- newIORef 1
  withScheduler_ (ParN nThreads) $ \scheduler -> do
    forM_ [1..maxPage] $ \page -> scheduleWork scheduler $ do
      postURLs <- retry 3 (getPageURLs page)
      forM_ postURLs $ \postURL -> scheduleWork scheduler $ do
        imageURL <- retry 3 (getImageURL postURL)
        case imageURL of
          Nothing -> putStrLn $ "!! 从页面抓取图片地址失败: " <> postURL
          Just img -> scheduleWork scheduler $ do
            i <- atomicModifyIORef' cnt (\x -> (x+1, x))
            putStrLn $ "正在下载第 " <> show i <> " 张图片"
            retry 3 $ downloadSave img (show i <> ".jpg")
