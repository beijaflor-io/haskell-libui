{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
module Graphics.LibUI
    ( module Graphics.LibUI.FFI
    , module Graphics.LibUI.MonadUI
    , module Graphics.LibUI.Types

    , MonadIO (..)
    , Default (..)
    )
  where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Default
import           Data.String
import           Foreign                  hiding (void)
import qualified Foreign                  as Foreign
import           Foreign.C

import           Graphics.LibUI.FFI
import           Graphics.LibUI.MonadUI
import           Graphics.LibUI.Types

import Control.Monad.Free
import Control.Monad.Free.TH

$(makeFree ''UIControl)

type Grammar child result = Free (UIControl child) result

ui :: Grammar CUIControl ()
ui = do
    uIControlWindow def
    uIControlButton def

runUILibUI :: Grammar CUIControl CUIControl -> IO CUIControl
runUILibUI = iterM $ \ctrl ->
    case ctrl of
        UIControlWindow c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIWindow)
        UIControlButton c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIButton)
        UIControlBox c  more ->
            toCUIControl <$> (toCUIIO c :: IO CUIBox)
        UIControlCheckbox c more ->
            toCUIControl <$> (toCUIIO c :: IO CUICheckbox)
        UIControlEntry c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIEntry)
        UIControlLabel c more ->
            toCUIControl <$> (toCUIIO c :: IO CUILabel)
        UIControlTab c more ->
            toCUIControl <$> (toCUIIO c :: IO CUITabs)
        UIControlGroup c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIGroup)
        UIControlSpinbox c more ->
            toCUIControl <$> (toCUIIO c :: IO CUISpinbox)
        UIControlSlider c more ->
            toCUIControl <$> (toCUIIO c :: IO CUISlider)
        UIControlProgressBar c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIProgressBar)
        UIControlSeparator c more ->
            toCUIControl <$> (toCUIIO c :: IO CUISeparator)
        UIControlCombobox c more ->
            toCUIControl <$> (toCUIIO c :: IO CUICombobox)
        UIControlEditableCombobox c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIEditableCombobox)
        UIControlRadioButtons c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIRadioButtons)
        UIControlMultlineEntry c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIMultilineEntry)
        UIControlMenuItem c more ->
            error "Undefined behavior"
            -- toCUIControl <$> (toCUIIO c :: IO CUIMenuItem)
        UIControlMenu c more ->
            toCUIControl <$> (toCUIIO c :: IO CUIMenu)
