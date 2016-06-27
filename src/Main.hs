{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server

import Data.List                ( intercalate )
import qualified Data.Map.Strict as Map
import Data.Maybe               ( fromMaybe )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment       ( getArgs, getProgName )
import System.Exit              ( exitFailure )

app = serve tputAPI . server

main = do
    args <- getArgs
    progName <- getProgName

    if null args
        then do
            putStrLn $ intercalate " " [ "usage:", progName, "UPLOAD_DIR" ]
            exitFailure
        else do
            let basedir = head args
            putStrLn $ concat [ "Running on port 8080 from ", basedir, "..." ]
            let settings = setPort 8080 $ setHost "127.0.0.1" defaultSettings
            conf <- makeDefaultServerConf
            runSettings settings $ app conf { basedir = basedir }
