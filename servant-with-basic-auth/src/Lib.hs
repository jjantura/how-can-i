{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Lib
    ( startApp
    ) where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Either
import           Data.Either.Combinators
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Protolude
import           Servant
import           Servant.Client
import           Servant.Server

-- model
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
-- API

data Credentials = Credentials  { login :: !Text, password :: !Text }

data Ctx = Ctx {
  creds :: Credentials
}

type AuthScheme = BasicAuth "userauth" Credentials

type API = AuthScheme :> UsersApi

type UsersApi = "users" :> Get '[JSON] [User]

data Error = SampleError deriving (Generic)

type Endpoint = ExceptT Error (ReaderT Ctx IO)

type BasicAuthCtx = Context '[BasicAuthCheck Credentials]

authCtx :: Ctx -> BasicAuthCtx
authCtx ctx = checkBasicAuth ctx :. EmptyContext

checkBasicAuth :: Ctx -> BasicAuthCheck Credentials
checkBasicAuth Ctx{creds} =
  BasicAuthCheck $ \(BasicAuthData authUsername authPassword) -> do
    let login' =  decodeUtf8 authUsername
        password' = decodeUtf8 authPassword
    if login' == login creds && password' == password creds then pure $ Authorized $ Credentials login' password' else pure Unauthorized

startApp :: IO ()
startApp = do
  let creds = Credentials "Alladin" "open sesame"
      ctx = Ctx creds
  app <- mkWaiApp ctx
  run 8080 app

mkWaiApp :: MonadIO m => Ctx -> m Application
mkWaiApp ctx = pure $ serveWithContext api (authCtx ctx) $ server ctx

api :: Proxy API
api = Proxy

server :: Ctx -> Server API
server ctx _ = server' (Proxy @UsersApi) users
  where
    server' :: HasServer api '[BasicAuthCtx] => Proxy api -> ServerT api Endpoint -> Server api
    server' proxy = hoistServerWithContext proxy (Proxy @'[BasicAuthCtx]) (runEndpoint ctx)

runEndpoint :: Ctx -> Endpoint a -> Handler a
runEndpoint ctx = Handler . mapExceptT (fmap (mapLeft mapError) . flip runReaderT ctx)

mapError :: Error -> ServerError
mapError _ = err400 {errBody = "some error"}

users :: Endpoint [User]
users = pure [ User 1 "Isaac" "Newton", User 2 "Albert" "Einstein" ]
