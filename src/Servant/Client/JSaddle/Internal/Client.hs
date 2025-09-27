{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Client.JSaddle.Internal.Client where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
                 (evaluate)
import           Control.Monad
                 (unless)
import           Control.Monad.Codensity
                 (Codensity(..))
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Reader
                 (asks, runReaderT)
import           Control.Monad.Trans.Except
                 (runExceptT)
import           Data.Bifunctor
                 (bimap)
import           Data.ByteString.Builder
                 (toLazyByteString)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Maybe
                 (fromMaybe)
import           Data.Proxy
                 (Proxy (..))
import qualified GHCJS.DOM
import qualified GHCJS.DOM.Location                as Location
import           GHCJS.DOM.Types
                 (DOM, DOMContext, askDOM, runDOM)
import qualified GHCJS.DOM.Types                   as JS
import qualified GHCJS.DOM.Window                  as Window
import qualified Language.Javascript.JSaddle       as JSaddle
import           Network.HTTP.Types
                 (Status, statusCode)

import           Servant.Client.Core
import           Servant.Client.JSaddle.Internal.Fetch
import           Servant.Client.JSaddle.Internal.Types

-- | Default 'ClientEnv'
mkClientEnv :: BaseUrl -> ClientEnv
mkClientEnv burl = ClientEnv burl ((\(JS.Object o) -> JS.RequestInit o) <$> JSaddle.obj)

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

instance RunClient ClientM where
  throwClientError = throwError
#if MIN_VERSION_servant_client_core(0,18,1)
  runRequestAcceptStatus acceptStatuses r = do
    d <- ClientM askDOM
    performRequest (fromMaybe [] acceptStatuses) d r
#else
  runRequest r = do
    d <- ClientM askDOM
    performRequest [] d r
#endif

instance RunStreamingClient ClientM where
  withStreamingRequest req k = do
    d <- ClientM askDOM
    performWithStreamingRequest d req k

runClientM :: ClientM a -> ClientEnv -> DOM (Either ClientError a)
runClientM cm env = withClientM cm env (liftIO . evaluate)

runClientM' :: ClientM a -> DOM (Either ClientError a)
runClientM' m = do
    burl <- getDefaultBaseUrl
    runClientM m (mkClientEnv burl)

withClientM :: ClientM a -> ClientEnv -> (Either ClientError a -> DOM b) -> DOM b
withClientM cm env k =
  let Codensity f = runExceptT $ flip runReaderT env $ fromClientM cm
  in f k

getDefaultBaseUrl :: DOM BaseUrl
getDefaultBaseUrl = do
    win <- GHCJS.DOM.currentWindow >>= \mw -> case mw of
      Just x -> pure x
      Nothing -> fail "Can not determine default base url without window."
    curLoc <- Window.getLocation win

    protocolStr  <- Location.getProtocol curLoc
    portStr      <- Location.getPort     curLoc
    hostname     <- Location.getHostname curLoc

    let protocol
          | (protocolStr :: JS.JSString) == "https:"
                        = Https
          | otherwise   = Http

        port :: Int
        port | null portStr = case protocol of
                 Http  ->  80
                 Https -> 443
             | otherwise = read portStr

    pure (BaseUrl protocol hostname port "")

performRequest :: [Status] -> DOMContext -> Request -> ClientM Response
performRequest acceptStatuses domc req = do
  burl <- asks baseUrl
  rinit <- asks requestInit >>= flip runDOM domc
  performFetch req burl rinit `runDOM` domc >>=
    either throwClientError
    (\fetch -> do
        resp <- toResponse domc fetch

        let status = statusCode (responseStatusCode resp)
        unless ((status >= 200 && status < 300) || status `elem` (statusCode <$> acceptStatuses)) $
          throwClientError $ mkFailureResponse burl req resp

        pure resp
    )

performWithStreamingRequest :: DOMContext -> Request -> (StreamingResponse -> IO a) -> ClientM a
performWithStreamingRequest domc req k = do
  burl <- asks baseUrl
  rinit <- asks requestInit >>= flip runDOM domc
  performStreamingRequest domc req burl rinit k

mkFailureResponse :: BaseUrl -> Request -> ResponseF BSL.ByteString -> ClientError
mkFailureResponse burl request =
    FailureResponse (bimap (const ()) f request)
  where
    f b = (burl, BSL.toStrict $ toLazyByteString b)
