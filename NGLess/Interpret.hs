{- Copyright 2015-2017 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}
module Interpret
    ( interpret
    ) where
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as B

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|))


import Control.Monad.IO.Class   (liftIO)

import           Control.Monad.Trans.Resource
type NGLessIO = ResourceT IO


data NGLInterpretEnv = NGLInterpretEnv ()
type InterpretationEnvIO = StateT  NGLInterpretEnv NGLessIO

interpret :: [(Int,String)] -> NGLessIO ()
interpret es = do
    evalStateT (interpretIO es) (NGLInterpretEnv ())

interpretIO :: [(Int, String)] -> InterpretationEnvIO ()
interpretIO es = forM_ es $ \(_,e) -> do
    interpretTop e

interpretTop :: String -> InterpretationEnvIO ()
interpretTop "count" = void $ lift (performCount "testing.sam")
interpretTop _ = error "Top level statement is NOP"

enumerateC :: (Monad m) => C.Conduit a m (Int, a)
enumerateC = loop 0
    where
        loop !n = C.await >>= \case
                                Nothing -> return ()
                                Just v -> do
                                    C.yield (n, v)
                                    loop (n+1)

countC = loop (0 :: Int)
    where
        loop !n = C.await >>= \case
                            Nothing -> return n
                            Just{} -> loop (n+1)

parseHeader :: C.Sink B.ByteString NGLessIO Int
parseHeader =
        CL.filter (B.isPrefixOf "@SQ\tSN:")
                    .| enumerateC
                    .| countC

isHeader :: B.ByteString -> Bool
isHeader s = not (B.null s) && (B.head s == 64) -- 64 is '@'

performCount :: FilePath  -> NGLessIO FilePath
performCount istream = do
    C.runConduit $
        CC.sourceFile istream
            .| CB.lines
            .| do
                c0 <- CC.takeWhile isHeader .| parseHeader
                liftIO $ print c0
                c <- countC
                liftIO $ print c
                return ()
    return "test.txt"


