{- Copyright 2015-2018 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, CPP #-}
module Interpretation.Count
    ( executeCount
    ) where
import Control.Monad.Primitive

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VA

import qualified Data.Set as S

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|), (=$=))


import Control.Monad.IO.Class   (liftIO)
import GHC.Conc                 (getNumCapabilities)
import Control.DeepSeq          (NFData(..))
import Data.Maybe

import Data.Convertible         (convert)

import Data.Sam (SamLine(..), isSamHeaderString, readSamGroupsC')
import NGLess

import Utils.Conduit
import qualified Utils.IntGroups as IG
import qualified Interpretation.Count.RefSeqInfoVector as RSV

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

data Annotator = SeqNameAnnotator (Maybe RSV.RefSeqInfoVector) -- ^ Just annotate by sequence names
instance NFData Annotator where
    rnf (SeqNameAnnotator m) = rnf m

annotateReadGroup :: CountOpts -> Annotator -> [SamLine] -> Either String [Int]
annotateReadGroup _ ann samlines = add1 . listNub <$> case ann of
        SeqNameAnnotator Nothing -> error "Incomplete annotator used"
        SeqNameAnnotator (Just szmap) -> mapMaybeM (getID szmap) samlines
    where
        -- this is because "unmatched" is -1
        add1 [] = [0]
        add1 vs = (+ 1) <$> vs
        getID :: RSV.RefSeqInfoVector -> SamLine -> Either String (Maybe Int)
        getID _ _ = Right Nothing


annEnumerate :: Annotator -> [(B.ByteString, Int)]
annEnumerate (SeqNameAnnotator Nothing)   = error "Using unfinished annotator"
annEnumerate (SeqNameAnnotator (Just ix)) = ("-1",0):enumerateRSVector ix
enumerateRSVector rfv = [(RSV.retrieveName rfv i, i + 1) | i <- [0.. RSV.length rfv - 1]]

-- Number of elements
annSize :: Annotator -> Int
annSize (SeqNameAnnotator (Just rfv)) = RSV.length rfv + 1
annSize ann = length (annEnumerate ann)


{- We define the type AnnotationIntersectionMode mainly to facilitate tests,
 - which depend on being able to write code such as
 -
 -      annotationRule IntersectUnion
 -}
data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq)



executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOMappedReadSet rname istream _) args = do
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
    NGOCounts . File <$> performCount istream rname (SeqNameAnnotator Nothing) opts
executeCount err _ = error ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)

-- First pass over the data
performCount1Pass :: MMMethod
                        -> VUM.IOVector Double -- ^ counts vector. Will be modified
                        -> C.Sink (VU.Vector Int, IG.IntGroups) NGLessIO [IG.IntGroups]
performCount1Pass MMUniqueOnly mcounts = do
    C.awaitForever $ \(singles, _) -> liftIO (incrementAll mcounts singles)
    return []
performCount1Pass MMCountAll mcounts = do
    C.awaitForever $ \(singles, mms) -> liftIO $ do
        incrementAll mcounts singles
        IG.forM_ mms (incrementAllV mcounts)
    return []
performCount1Pass MM1OverN mcounts = do
    C.awaitForever $ \(singles, mms) -> liftIO $ do
        incrementAll mcounts singles
        IG.forM_ mms (increment1OverN mcounts)
    return []
performCount1Pass MMDist1 mcounts = loop []
    where
        loop :: [IG.IntGroups] -> C.Sink (VU.Vector Int, IG.IntGroups) NGLessIO [IG.IntGroups]
        loop acc = C.await >>= \case
            Nothing -> return acc
            Just (singles, mms) ->  do
                    liftIO $ incrementAll mcounts singles
                    loop $ if not (IG.null mms)
                                then mms:acc
                                else acc

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
annSamHeaderParser mapthreads anns _ = lineGroups =$= (annSamHeaderParser1 anns)
    where
        annSamHeaderParser1 (SeqNameAnnotator Nothing) = do
            rfvm <- liftIO RSV.newRefSeqInfoVector
            asyncMapEitherC mapthreads (\(!vi, v) -> V.imapM (\ix ell -> seqNameSize (vi*32768+ix, ell)) v)
                .| CL.mapM_ (\v -> liftIO $
                                    V.forM_ v $ \(RSV.RefSeqInfo n val) ->
                                        RSV.insert rfvm n val)
            vsorted <- liftIO $ do
                RSV.sort rfvm
                RSV.unsafeFreeze rfvm
            return $! SeqNameAnnotator (Just vsorted)
        annSamHeaderParser1 ann = CC.sinkNull >> return ann
        lineGroups = CL.filter (B.isPrefixOf "@SQ\tSN:" . unwrapByteLine)
                    .| CC.conduitVector 32768
                    .| enumerateC

        seqNameSize :: (Int, ByteLine) -> Either String RSV.RefSeqInfo
        seqNameSize (n, ByteLine h) = case B8.split '\t' h of
                [_,seqname,sizestr] -> case B8.readInt (B.drop 3 sizestr) of
                    Just (size, _) -> return $! RSV.RefSeqInfo (B.drop 3 seqname) (convert size)
                    Nothing -> Left ("Could not parse sequence length in header (line: " ++ show n ++ ")")
                _ -> Left ("SAM file does not contain the right number of tokens (line: " ++ show n ++ ")")


listNub :: (Ord a) => [a] -> [a]
listNub = S.toList . S.fromList


-- Takes a vector of [Int] and splits into singletons (which can be represented
-- as `VU.Vector Int` and the rest (represented as `IG.IntGroups`)
splitSingletons :: MMMethod -> V.Vector [Int] -> (VU.Vector Int, IG.IntGroups)
splitSingletons method values = (singles, mms)
    where
        singles = VU.create $ do
            v <- VU.unsafeThaw $ VU.unfoldr getsingle1 0
            -- We want to maximize the work performed in this function as it is
            -- being done in a worker thread:
            -- sorting is completely unnecessary for correctness, but improves
            -- cache performance as close-by indices will be accessed together
            -- when this data is processed in the main thread.
            VA.sort v

            return v
        getsingle1 :: Int -> Maybe (Int, Int)
        getsingle1 ix = do
            vs <- values V.!? ix
            case vs of
                [v] -> return (v, ix + 1)
                _ -> getsingle1 (ix + 1)
        mms -- if we are only using unique hits, then we do not need to care about non-singletons
            | method == MMUniqueOnly = IG.empty
            | otherwise = IG.fromList (filter larger1 (V.toList values))
        larger1 []  = False
        larger1 [_] = False
        larger1 _   = True


takeWhileC :: Monad m => (a -> Bool) -> C.Conduit a m a
takeWhileC f = loop
    where
        loop = C.await >>= maybe (return ()) (\v ->
                    if f v
                        then C.yield v >> loop
                        else C.leftover v)

performCount :: FileOrStream -> T.Text -> Annotator -> CountOpts -> NGLessIO FilePath
performCount istream gname annotators0 opts = do
    numCapabilities <- liftIO getNumCapabilities
    let mapthreads = max 1 (numCapabilities - 1)
        method = optMMMethod opts
        delim = optDelim opts
        (samfp, samStream) = asSamStream istream
    (_, mcounts, annotators) <- C.runConduit $
        samStream
            .| do
                ann <-
                    takeWhileC (isSamHeaderString . unwrapByteLine)
                        .| annSamHeaderParser mapthreads annotators0 opts
                let n_entries = annSize ann
                mcounts <- liftIO $ VUM.replicate n_entries (0.0 :: Double)
                toDistribute <-
                    readSamGroupsC' mapthreads True
                        .| asyncMapEitherC mapthreads (\samgroup -> do
                                                            annotated <- V.mapM (annotateReadGroup opts ann) samgroup
                                                            return $ splitSingletons method annotated)
                        .| performCount1Pass method mcounts
                return (toDistribute, mcounts, ann)
    return "test.txt"


unsafeIncrement :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> m ()
unsafeIncrement v i = unsafeIncrement' v i 1

unsafeIncrement' :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> a -> m ()
unsafeIncrement' v i inc = VUM.unsafeModify v (+ inc) i

incrementAll :: VUM.IOVector Double -> VU.Vector Int -> IO ()
incrementAll counts vis = VU.forM_ vis $ \vi -> unsafeIncrement counts vi

incrementAllV :: VUM.IOVector Double -> VU.Vector Int -> IO ()
incrementAllV counts vis = VU.forM_ vis $ \vi -> unsafeIncrement counts vi

increment1OverN :: VUM.IOVector Double -> VU.Vector Int -> IO ()
increment1OverN counts vis = VU.forM_ vis $ \vi -> unsafeIncrement' counts vi oneOverN
    where
        oneOverN :: Double
        oneOverN = 1.0 / convert (VU.length vis)

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs
