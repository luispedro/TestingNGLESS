{-# LANGUAGE FlexibleContexts, CPP #-}
module Count
    ( executeCount
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

data Annotator = SeqNameAnnotator (Maybe Int)


executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount arg@(NGOMappedReadSet _ istream) args = do
    performCount istream
    return arg
executeCount err _ = error ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)

-- | Equivalent to Python's enumerate
enumerateC :: (Monad m) => C.Conduit a m (Int, a)
enumerateC = loop 0
    where
        loop !n = C.await >>= \case
                                Nothing -> return ()
                                Just v -> do
                                    C.yield (n, v)
                                    loop (n+1)


annSamHeaderParser :: C.Sink B.ByteString NGLessIO Int
annSamHeaderParser = lineGroups =$= annSamHeaderParser1
    where
        annSamHeaderParser1 = do
            c <- liftIO $ newIORef (0 :: Int)
            CL.map (V.map B.length . snd)
                .| CL.mapM_ (\v -> liftIO $
                                    V.forM_ v $ \ix -> modifyIORef' c (+ ix))
            liftIO $ readIORef c
        lineGroups = CL.filter (B.isPrefixOf "@SQ\tSN:")
                    .| CC.conduitVector 32768
                    .| enumerateC

isSamHeaderString :: B.ByteString -> Bool
isSamHeaderString s = not (B.null s) && (B.head s == 64) -- 64 is '@'

performCount :: FilePath  -> NGLessIO FilePath
performCount istream = do
    C.runConduit $
        CC.sourceFile istream
            .| CB.lines
            .| do
                c0 <- CC.takeWhile isSamHeaderString .| annSamHeaderParser
                liftIO $ print c0
                c <- countC
                liftIO $ print c
                return ()
    return "test.txt"

countC = loop (0 :: Int)
    where
        loop !n = C.await >>= \case
                            Nothing -> return n
                            Just{} -> loop (n+1)

