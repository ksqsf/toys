{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Network.HTTP.Simple
import           Text.HTML.Scalpel
import           Text.Regex
import qualified Control.Monad.Parallel as Par
import qualified Data.ByteString.Char8  as C

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
downloadSave url path = do
  req  <- parseRequest url
  resp <- httpBS req
  C.writeFile path (getResponseBody resp)

main :: IO ()
main = do
  let maxPage = 14
  postURLs <- foldMap getPageURLs [1..maxPage]
  imageURLs <- mapMB 10 getImageURL postURLs
  forMB 10 (zip [1..] imageURLs) $ \(i, img) -> do
    case img of
      Left url -> do
        putStrLn $ "!! 从页面抓取图片地址失败: " <> url
      Right img -> do
        putStrLn $ "正在下载第 " <> show i <> " 张图片"
        downloadSave img (show i <> ".jpg")
  return ()

-- Bounded parallelism.
mapMB :: Par.MonadParallel m => Int -> (a -> m b) -> [a] -> m [b]
mapMB n f [] = return []
mapMB n f xs = do
  cohort <- Par.mapM f (take n xs)
  rest   <- mapMB n  f (drop n xs)
  return (cohort ++ rest)

forMB :: Par.MonadParallel m => Int -> [a] -> (a -> m b) -> m [b]
forMB n = flip (mapMB n)
