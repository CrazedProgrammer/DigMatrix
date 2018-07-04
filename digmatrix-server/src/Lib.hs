{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai.Handler.Warp (run)
import Servant
import Blocks
import Control.Monad.IO.Class
import Linear.V3
import Control.Exception.Base
import System.Environment (getArgs)

startApp :: IO ()
startApp =
  do
    args <- getArgs
    let listenPort = read (head args) :: Int
    putStrLn ("Running webserver on port " ++ (show listenPort))
    run listenPort app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

type API = "all-blocks" :> Get '[JSON] [Block]
      :<|> "n-blocks"   :> Get '[JSON] Int
      :<|> "scan-blocks" :> ReqBody '[JSON] [Scan] :> Post '[JSON] [Block]
      :<|> "new-blocks" :> ReqBody '[JSON] [Block] :> Post '[JSON] ()

server :: Server API
server = allBlocks
    :<|> nBlocks
    :<|> sBlocks
    :<|> newBlocks

  where
    allBlocks =
      do
        blocks <- getBlocks
        pure blocks
    nBlocks =
      do
        blocks <- getBlocks
        pure (length blocks)
    sBlocks scans =
      do
        blocks <- getBlocks
        pure (scanBlocks scans blocks)
    newBlocks blocks =
      do
        oldBlocks <- getBlocks
        liftIO (evaluate (length oldBlocks))
        saveBlocks (addBlocks oldBlocks blocks)
        pure ()
