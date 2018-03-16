{- Copyright 2013-2017 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Utils.Conduit
    ( ByteLine(..)
    , linesC
    ) where

import qualified Data.ByteString as B

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import           Data.Conduit ((=$=))

-- | This just signals that a "line" is expected.
newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }
                deriving (Show)

linesC :: (Monad m) => C.Conduit B.ByteString m ByteLine
linesC = CB.lines =$= CL.map ByteLine
{-# INLINE linesC #-}

