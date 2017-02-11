{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import Helper

import           Web.Scotty as S
import qualified Network.Wai as Wai

import           Data.Monoid ((<>))
import           Data.Text.Internal.Lazy
import qualified Data.Text.Lazy as DTL
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BLI
import           Control.Applicative     ((<$>))

import Control.Monad.IO.Class (liftIO)

-- curl -XPOST -d '{"board":[[0,0,0,7,0,0,6,0,0],[2,0,3,9,0,0,0,8,0],[7,0,8,0,0,0,3,0,0],[0,0,0,8,3,0,0,0,0],[0,0,0,0,0,6,0,0,9],[3,0,0,0,0,0,7,0,0],[9,7,0,0,0,0,4,0,0],[5,0,0,1,0,7,0,0,0],[0,4,0,0,6,0,0,0,1]]}' http://localhost:3000/solve/number_place


startApp = do
  scotty 3000 $ do
    post "/solve/number_place" $ do
      req <- request                           -- リクエスト
      biBody <- liftIO $ Wai.requestBody $ req -- BIのBody
      bliBody <- S.body                        -- BLIのBody
      responseNP <- liftIO $ handleNP biBody bliBody -- 数独の解取得
      liftIO $ putStrLn $ show responseNP
      json responseNP -- jsonでレスポンス
    get "/number_places" $ do
      num <- liftIO getAllNp
      json num
    get "/number_place/:page" $ do
      page <- param "page"
      responseNP <- liftIO $ getSelectNp page
      json responseNP

loged :: Wai.Request -> IO()
loged request = do
  putStrLn $ "requestMethod          : " ++ show (Wai.requestMethod $ request)
  putStrLn $ "httpVersion            : " ++ show (Wai.httpVersion request)
  putStrLn $ "rawPathInfo            : " ++ show (Wai.rawPathInfo $ request)
  putStrLn $ "rawQueryString         : " ++ show (Wai.rawQueryString $ request)
  putStrLn $ "requestHeaders         : " ++ show (Wai.requestHeaders $ request)
  putStrLn $ "isSecure               : " ++ show (Wai.isSecure $ request)
  putStrLn $ "remoteHost             : " ++ show (Wai.remoteHost $ request)
  putStrLn $ "pathInfo               : " ++ show (Wai.pathInfo $ request)
  putStrLn $ "queryString            : " ++ show (Wai.queryString $ request)
  reqBody <- Wai.requestBody $ request
  putStrLn $ "requestBody            : " ++ show reqBody
  putStrLn $ "requestBodyLength      : " ++ show (Wai.requestBodyLength $ request)
  putStrLn $ "requestHeaderHost      : " ++ show (Wai.requestHeaderHost $ request)
  putStrLn $ "requestHeaderRange     : " ++ show (Wai.requestHeaderRange $ request)
  putStrLn $ "requestHeaderReferer   : " ++ show (Wai.requestHeaderReferer $ request)
  putStrLn $ "requestHeaderUserAgent : " ++ show (Wai.requestHeaderUserAgent $ request) ++ "\n\n"
