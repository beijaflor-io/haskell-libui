{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.LibUI.MonadUI where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.String
import           Foreign                  hiding (void)
import qualified Foreign
import           Foreign.C
import           System.IO                (fixIO)

import           Graphics.LibUI.FFI

data UI a = UI { runUI :: IO (a, [CUIControl])
               }

instance Functor UI where
    f `fmap` ui = UI $ runUI ui >>= \(a, c) -> return (f a, c)

instance Monoid a => Monoid (UI a) where
    mempty = UI (return (mempty, []))
    ui1 `mappend` ui2 = UI $ do
        (a, cui1) <- runUI ui1
        (b, cui2) <- runUI ui2
        return (a `mappend` b, cui1 ++ cui2)

instance MonadFix UI where
    mfix f = UI $ do
        x <- fixIO (fmap fst . runUI . f)
        return (x, [])

instance Applicative UI where
    pure = return
    (<*>) = ap

instance Monad UI where
    return x = UI (return (x, []))
    ui >>= a = UI $ do
        (r, cui1) <- runUI ui
        (r', cui2) <- runUI $ a r
        return (r', cui1 ++ cui2)

instance MonadIO UI where
    liftIO action = UI $ do
        a <- action
        return (a, [])
