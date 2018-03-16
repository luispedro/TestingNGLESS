{-# LANGUAGE FlexibleContexts, CPP #-}
module Count
    ( performCount
    ) where

import qualified Data.ByteString as B

import qualified Data.Vector as V

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|), (=$=))


import Control.Monad.IO.Class   (liftIO)
import Data.IORef

import NGLess

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
                    .| do
                            c <- liftIO $ newIORef (0 :: Int)
                            CL.map (B.length . snd)
                                .| CL.mapM_ (\ix -> liftIO $ modifyIORef' c (+ ix))
                            liftIO $ readIORef c

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


