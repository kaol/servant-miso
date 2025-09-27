{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Servant.Client.JSaddle.Internal.Fetch where

import           Prelude ()
import           Prelude.Compat                        hiding ((!!))

import           Control.Exception
                 (toException, throwIO)
import           Control.Monad
                 ((<=<), void)
import           Control.Monad.Catch
                 (try)
import           Control.Monad.Codensity
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Morph
import           Data.ByteString.Builder
                 (toLazyByteString)
import           Data.Bifunctor
                 (bimap)
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as L
import           Data.CaseInsensitive
                 (mk, original)
import           Data.Foldable
import           Data.Maybe
                 (fromMaybe)
import qualified Data.Sequence                         as Seq
import           Data.Text.Encoding
import qualified JavaScript.TypedArray.ArrayBuffer     as AB
import qualified GHCJS.Buffer
import           GHCJS.DOM.Types
import           Network.HTTP.Media
import           Network.HTTP.Types
                 (http11, mkStatus, renderQuery)
import           Language.Javascript.JSaddle

import           Servant.Client.Core                   hiding (Request, Response)
import qualified Servant.Client.Core                   as Servant
import           Servant.Client.JSaddle.Internal.Types
import qualified Servant.Types.SourceT as S

performFetch :: Servant.Request -> BaseUrl -> RequestInit -> JSM (Either ClientError Response)
performFetch req burl rinit = do
  let url = toUrl burl req
  -- TODO use headers from rinit and amend them instead, if available
  headers <- new (jsg ("Headers" :: JSString)) ()

  forM_ (toList $ requestAccept req) $ \mediaType ->
    headers # ("append" :: JSString) $
    ( "Accept" :: JSString
    , decodeUtf8Lenient $ renderHeader mediaType
    )

  forM_ (toList $ requestHeaders req) $ \(key, value) ->
    headers # ("append" :: JSString) $
    ( decodeUtf8Lenient $ original key
    , decodeUtf8Lenient value
    )

  let setTextBody x =
        rinit <# ("body" :: JSString) $ decodeUtf8Lenient x
      setContentType t = void $ headers # ("append" :: JSString) $
        ( "Content-Type" :: JSString
        , decodeUtf8Lenient $ renderHeader t
        )
          
      setBody x t = do
        case (mainType t, subType t) of
          ("application", "json") -> setTextBody x
          ("text", _) -> setTextBody x
          ("image", "svg+xml") -> setTextBody x
          _ -> do
            x' <- bsToJSVal $ L.fromStrict x
            rinit <# ("body" :: JSString) $ x'

      stream writer s = do
        let u = S.unSourceT s
            consume S.Stop = pure $ Left Nothing
            consume (S.Error e) = pure $ Left $ Just e
            consume (S.Skip step) = consume step
            consume (S.Yield x step) = pure $ Right (x, step)
            consume (S.Effect a) = a >>= consume
        res <- liftIO $ u consume
        case res of
          Left Nothing -> () <$ (writer # ("stop" :: JSString) $ ())
          Left (Just e) -> () <$ (writer # ("abort" :: JSString) $ e)
          Right (x, step) -> do
            x' <- bsToJSVal x
            _ <- readPromise =<< (writer # ("write" :: JSString) $ x')
            stream writer $ S.fromStepT step

  case requestBody req of
    Nothing -> pure ()
    Just (RequestBodyLBS "", _) -> pure ()
    Just (RequestBodyLBS x, t) ->
      setContentType t >> setBody (L.toStrict x) t
    Just (RequestBodyBS "", _) -> pure ()
    Just (RequestBodyBS x, t) ->
      setContentType t >> setBody x t
    Just (RequestBodySource x, t) -> do
      writer <- do
        setContentType t
        transformStream <- new (jsg ("TransformStream" :: JSString)) ()
        readable <- transformStream ! ("readable" :: JSString)
        rinit <# ("body" :: JSString) $ readable
        writable <- transformStream ! ("writable" :: JSString)
        writable # ("getWriter" :: JSString) $ ()
      stream writer x
      pure ()

  rinit <# ("headers" :: JSString) $ headers
  rinit <# ("method" :: JSString) $ decodeUtf8Lenient $ requestMethod req

  bimap (ConnectionError . toException @PromiseRejected) Response <$>
    try (readPromise =<< jsg2 ("fetch" :: JSString) url rinit)

    where
      bsToJSVal x = do
        (x',_,_) <- ghcjsPure (GHCJS.Buffer.fromByteString $ L.toStrict x)
        pToJSVal <$> (ghcjsPure . GHCJS.Buffer.getArrayBuffer =<< GHCJS.Buffer.thaw x')

toUrl :: BaseUrl -> Servant.Request -> JSString
toUrl burl request =
  let pathS = toJSString $ decodeUtf8Lenient $ L.toStrict $ toLazyByteString $
              requestPath request
      queryS =
          toJSString $ decodeUtf8Lenient $
          renderQuery True $
          toList $
          requestQueryString request
  in toJSString (showBaseUrl burl) <> pathS <> queryS :: JSString

toResponseGeneric :: (MakeObject t, ToJSVal t) => t -> JSM a -> JSM (Either ClientError (ResponseF a))
toResponseGeneric response getBody = do
  status <- read @Int . fromJSString <$> (valToStr =<< (response ! ("status" :: JSString)))
  case status of
    0 -> pure $ Left $ ConnectionError $ toException JSaddleConnectionError
    _ -> do
      statusText <- BS.pack . fromJSString <$>
        (valToStr =<< response ! ("statusText" :: JSString))
      headersField <- response ! ("headers" :: JSString)
      iterator <- headersField # ("entries" :: JSString) $ ()
      headers <- getHeaders iterator
      body <- getBody
      pure $ Right $ Servant.Response
        { responseStatusCode = mkStatus status statusText
        , responseBody = body
        , responseHeaders = Seq.fromList headers
        , responseHttpVersion = http11
        }
  where
    getHeaders iterator = do
      res <- iterator # ("next" :: JSString) $ ()
      done <- fromMaybe False <$> (nullableToMaybe =<< res ! ("done" :: JSString))
      if done then return [] else do
        x <- res ! ("value" :: JSString)
        k <- BS.pack . fromJSString <$> (valToStr =<< x !! 0)
        v <- BS.pack . fromJSString <$> (valToStr =<< x !! 1)
        ((mk k,v):) <$> getHeaders iterator
  
toResponse :: DOMContext -> Response -> ClientM Servant.Response
toResponse domc response = do
  either throwError pure =<<
    (flip runDOM domc $ toResponseGeneric response $
     (pFromJSVal <$> (response # ("arrayBuffer" :: JSString) $ ())) >>=
     (fmap pFromJSVal . readPromise) >>=
     AB.unsafeFreeze >>=
     ghcjsPure . GHCJS.Buffer.createFromArrayBuffer >>=
     fmap L.fromStrict . ghcjsPure . (GHCJS.Buffer.toByteString 0 Nothing))

performStreamingRequest :: DOMContext -> Servant.Request -> BaseUrl -> RequestInit -> (StreamingResponse -> IO a) -> ClientM a
performStreamingRequest domc req burl rinit k = do
  ClientM $ lift $ lift $ Codensity $ \k1 -> bracket (performFetch req burl rinit)
    (either (const $ pure ()) (fmap (const ()) . closeFetch)) $
    either (liftIO . throwIO) $ \response ->
    bracket (response ! ("body" :: JSString) >>= \body -> body # ("getReader" :: JSString) $ ())
    (\reader -> reader # ("releaseLock" :: JSString) $ ()) $ \reader -> do
    let steps = flip runDOM domc $ do
          chunk <- readPromise =<< (reader # ("read" :: JSString) $ ())
          done <- fromMaybe False <$> (nullableToMaybe =<< (chunk ! ("done" :: JSString)))
          if done
            then pure $ S.Stop
            else do
            value <- chunk ! ("value" :: JSString) >>=
              (! ("buffer" :: JSString)) >>=
              AB.unsafeFreeze . pFromJSVal >>=
              ghcjsPure . GHCJS.Buffer.createFromArrayBuffer >>=
              ghcjsPure . (GHCJS.Buffer.toByteString 0 Nothing)
            pure $ S.Yield value $ S.Effect $ liftIO steps

    either (liftIO . throwIO) (k1 <=< liftIO . k) =<<
      toResponseGeneric response (pure $ S.SourceT (=<< steps))
  where
    closeFetch response = do
      body <- response ! ("body" :: JSString)
      body # ("cancel" :: JSString) $ ()
