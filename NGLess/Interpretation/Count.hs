{- Copyright 2015-2018 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, CPP #-}
module Interpretation.Count
    ( executeCount
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Data.Vector as V

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|), (=$=))


import Control.Monad.IO.Class   (liftIO)
import GHC.Conc                 (getNumCapabilities)
import Control.DeepSeq          (NFData(..))
import Data.IORef

import Data.Sam (isSamHeaderString, readSamGroupsC')
import NGLess

import Utils.Conduit

{- Implementation of count()
 -
 - The main function is performCount which loops over mapped read groups
 -. annotating them with an Annotator.
 -}


data MMMethod = MMCountAll | MM1OverN | MMDist1 | MMUniqueOnly
    deriving (Eq)

data NMode = NMRaw | NMNormed | NMScaled | NMFpkm
    deriving (Eq)


minDouble :: Double
minDouble = (2.0 :: Double) ^^ fst (floatRange (1.0 :: Double))

data CountOpts =
    CountOpts
    { optFeatures :: [B.ByteString] -- ^ list of features to condider
    , optMinCount :: !Double
    , optMMMethod :: !MMMethod
    , optDelim :: !B.ByteString
    , optNormMode :: !NMode
    , optIncludeMinus1 :: !Bool
    }

data AnnotationMode = AnnotateSeqName | AnnotateGFF FilePath | AnnotateFunctionalMap FilePath
    deriving (Eq, Show)

data Annotator = SeqNameAnnotator (Maybe Int)
instance NFData Annotator where
    rnf (SeqNameAnnotator m) = rnf m


executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOMappedReadSet _ istream _) args = do
    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOString f) -> return [f]
        _ -> error "??"
    let opts = CountOpts
            { optFeatures = map (B8.pack . T.unpack) fs
            , optMinCount = minDouble
            , optMMMethod = MMDist1
            , optDelim = "\t"
            , optNormMode = NMRaw
            , optIncludeMinus1 = True
            }
    NGOCounts . File <$> performCount istream (SeqNameAnnotator Nothing) opts
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


annSamHeaderParser :: Int -> Annotator -> CountOpts -> C.Sink ByteLine NGLessIO Annotator
annSamHeaderParser mapthreads anns _ = lineGroups =$= annSamHeaderParser1 anns
    where
        annSamHeaderParser1 (SeqNameAnnotator Nothing) = do
            c <- liftIO $ newIORef (0 :: Int)
            CL.map (\(!_, v) -> V.imap (\ix ell -> (B.length $ unwrapByteLine ell)) v)
                .| CL.mapM_ (\v -> liftIO $
                                    V.forM_ v $ \ix -> modifyIORef' c (+ ix))
            c' <- liftIO $ readIORef c
            return $! SeqNameAnnotator (Just c')
        annSamHeaderParser1 ann = CC.sinkNull >> return ann
        lineGroups = CL.filter (B.isPrefixOf "@SQ\tSN:" . unwrapByteLine)
                    .| CC.conduitVector 32768
                    .| enumerateC



performCount :: FileOrStream ->  Annotator -> CountOpts -> NGLessIO FilePath
performCount istream annotators0 opts = do
    numCapabilities <- liftIO getNumCapabilities
    let mapthreads = max 1 (numCapabilities - 1)
        (samfp, samStream) = asSamStream istream
    C.runConduit $
        samStream
            .| do
                ann <-
                    CC.takeWhile (isSamHeaderString . unwrapByteLine)
                        .| annSamHeaderParser mapthreads annotators0 opts
                toDistribute <-
                    readSamGroupsC' mapthreads True
                        .| countC
                return ()
    return "test.txt"


countC = loop (0 :: Int)
    where
        loop !n = C.await >>= \case
                            Nothing -> return n
                            Just{} -> loop (n+1)

