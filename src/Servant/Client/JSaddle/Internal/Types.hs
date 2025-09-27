{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.JSaddle.Internal.Types where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Codensity
import           Control.Exception
                 (Exception)
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Reader
                 (MonadReader, ReaderT)
import           Control.Monad.Trans.Except
                 (ExceptT)
import           Data.Functor.Alt
                 (Alt (..))
import           GHC.Generics
import           GHCJS.DOM.Types

import           Servant.Client.Core

-- Note: assuming encoding UTF-8
data ClientEnv
   = ClientEnv
   { baseUrl :: BaseUrl
   , requestInit :: JSM RequestInit
   }

data JSaddleConnectionError = JSaddleConnectionError
  deriving (Eq, Show)

instance Exception JSaddleConnectionError

instance Show ClientEnv where
  showsPrec prec (ClientEnv burl _) =
    showParen (prec >= 11)
      ( showString "ClientEnv {"
      . showString "baseUrl = "
      . showsPrec 0 burl
      . showString ", requestInit = <JSVal>"
      . showString "}"
      )

newtype ClientM a = ClientM
  { fromClientM :: ReaderT ClientEnv (ExceptT ClientError (Codensity DOM)) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ClientError)

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` const b
