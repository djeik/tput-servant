{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server
( ServerConf(..)
, defaultServerConf
, server
, tputAPI
)
where

import           Prelude                    hiding ( readFile, writeFile )
import           Control.Applicative
import           Data.List                  ( sort )
import           Control.Monad.IO.Class
import           Control.Exception          ( catch, SomeException )
import           Data.List                  ( sort )
import qualified Data.Text                  as T
import           Data.Text.IO               ( readFile, writeFile )
import           Servant
import           System.FilePath.Posix      ( (</>) )
import           System.Directory           ( getDirectoryContents )

type TPutAPI = "files"
                   :> Capture "path" FilePath
                   :> Get '[PlainText] T.Text
          :<|> "files"
                   :> Capture "path" FilePath
                   :> ReqBody '[PlainText] T.Text
                   :> Post '[PlainText] ()
          :<|> "list"
                   :> Get '[PlainText] T.Text

data ServerConf = ServerConf { basedir :: FilePath
                             }

defaultServerConf = ServerConf { basedir = "data"
                               }

tputAPI :: Proxy TPutAPI
tputAPI = Proxy

server :: ServerConf -> Server TPutAPI
server conf = down (basedir conf)
         :<|> up (basedir conf)
         :<|> ls (basedir conf)

down :: FilePath -> FilePath -> Handler T.Text
down db path = do
    text <- liftIO $ (readFile (db </> path) >>= return . Just) `catch` fail
    case text of
        Nothing -> left err404
        Just t -> return t
    where fail :: SomeException -> IO (Maybe T.Text)
          fail = const (return Nothing)

up :: FilePath -> FilePath -> T.Text -> Handler ()
up db path content = liftIO $ writeFile (db </> path) content

ls :: FilePath -> Handler T.Text
ls db = do
    files <- liftIO $ sort . filter (\(c:_) -> c /= '.') <$> getDirectoryContents db
    pure $ T.pack $ unlines files
