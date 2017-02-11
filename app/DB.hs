{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module DB
    (
    NumberPlace (..),
    doMigration,
    insertNP,
    selectAllNP,
    selectNP
    ) where
import qualified Data.Aeson   as AE
import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (runResourceT, ResourceT)
import           Database.Persist
import           Database.Persist.MySQL
-- (ConnectInfo (..),
--                                                Connection (..),
--                                                SqlBackend (..),
--                                                defaultConnectInfo, runMigration,
--                                                runSqlConn, withMySQLConn)
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics

type Board = [[Int]]
type Boards = [[[Int]]]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
NumberPlace json
    board    Board
    boards   Boards
    deriving Eq Show Generic
|]

doMigration :: IO ()
doMigration = runNoLoggingT $ runResourceT $ withMySQLConn getConnection $ runReaderT $ runMigration migrateAll

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB = runNoLoggingT . runResourceT . withMySQLConn getConnection . runSqlConn

insertNP :: [[Int]] -> [[[Int]]] -> IO()
insertNP board boards = runDB . insert_ $ NumberPlace board boards

selectAllNP :: IO [NumberPlace]
selectAllNP = do
    list <- runDB $ selectList [] []
    -- let npList = map (\(Entity _ u) -> u) list
    return $ map (\(Entity _ u) -> u) list

selectNP :: Int -> Int -> IO [NumberPlace]
selectNP from to = do
    list <- runDB $ selectList [] []
    let npList = map (\(Entity _ u) -> u) list
    return $ drop from $ take (to + 1) npList

getConnection :: ConnectInfo
getConnection = defaultConnectInfo {
    connectUser = "dbuser",
    connectPassword = "db",
    connectDatabase = "puzzle_db"
}
