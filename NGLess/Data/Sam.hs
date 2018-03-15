{- Copyright 2014-2017 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Data.Sam
    ( SamLine(..)
    , SamGroup
    , readSamGroupsC
    , readSamGroupsC'
    , readSamLine
    , isSamHeaderString

    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit ((=$=), (.|))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Strict.Tuple (Pair(..))
import Data.Bits (testBit)
import Control.DeepSeq
import Control.Monad.Base

import Data.Function (on)
import Control.Monad.Except
import Utils.Conduit


type SamGroup = [SamLine]

data SamLine = SamLine
            { samQName :: !B.ByteString
            , samFlag :: {-# UNPACK #-} !Int
            , samRName :: {-# UNPACK #-} !B.ByteString
            , samPos :: {-# UNPACK #-} !Int
            , samMapq :: {-# UNPACK #-} !Int
            , samCigar :: {-# UNPACK #-} !B.ByteString
            , samRNext :: {-# UNPACK #-} !B.ByteString
            , samPNext :: {-# UNPACK #-} !Int
            , samTLen :: {-# UNPACK #-} !Int
            , samSeq :: {-# UNPACK #-} !B.ByteString
            , samQual :: {-# UNPACK #-} !B.ByteString
            , samExtra :: {-# UNPACK #-} !B.ByteString
            } | SamHeader !B.ByteString
             deriving (Eq, Show, Ord)


instance NFData SamLine where
    rnf SamLine{} = ()
    rnf (SamHeader !_) = ()


isFirstInPair :: SamLine -> Bool
isFirstInPair = (`testBit` 6) . samFlag

isSamHeaderString :: B.ByteString -> Bool
isSamHeaderString s = not (B.null s) && (B.head s == 64) -- 64 is '@'

newtype SimpleParser a = SimpleParser { runSimpleParser :: B.ByteString -> Maybe (Pair a B.ByteString) }
instance Functor SimpleParser where
    fmap f p = SimpleParser $ \b -> do
                                (v :!: rest) <- runSimpleParser p b
                                return $! (f v :!: rest)

instance Applicative SimpleParser where
    pure v = SimpleParser (\b -> Just (v :!: b))
    f <*> g = SimpleParser (\b -> do
                                (f' :!: rest) <- runSimpleParser f b
                                (g' :!: rest') <- runSimpleParser g rest
                                return $! (f' g' :!: rest'))

readSamLine :: B.ByteString -> Either String SamLine
readSamLine line
    | B.null line = Left "Unexpected empty line"
    | B8.head line == '@' = return (SamHeader line)
    | otherwise = case runSimpleParser samP line of
        Just (v :!: _) -> return v
        Nothing -> Left ("Could not parse sam line "++show line)

tabDelim :: SimpleParser B.ByteString
tabDelim = SimpleParser $ \input -> do
    ix <- B8.elemIndex '\t' input
    return $! (B.take ix input :!: B.drop (ix+1) input)

tabDelimOpts :: SimpleParser B.ByteString
tabDelimOpts = SimpleParser $ \input -> do
    case B8.elemIndex '\t' input of
         Just (ix) -> return $! (B.take ix input :!: B.drop (ix+1) input)
         Nothing -> return $! (B.empty :!: input)

readIntTab = SimpleParser $ \b -> do
        (v,rest) <- B8.readInt b
        return $! (v :!: B.tail rest)
restParser = SimpleParser $ \b -> Just (b :!: B.empty)
samP = SamLine
    <$> tabDelim
    <*> readIntTab
    <*> tabDelim
    <*> readIntTab
    <*> readIntTab
    <*> tabDelim
    <*> tabDelim
    <*> readIntTab
    <*> readIntTab
    <*> tabDelim
    <*> tabDelimOpts
    <*> restParser

{--
Op     Description
M alignment match (can be a sequence match or mismatch).
I insertion to the reference
D deletion from the reference.
N skipped region from the reference.
S soft clipping (clipped sequences present inSEQ)
H hard clipping (clipped sequences NOT present inSEQ)
P padding (silent deletion from padded reference).
= sequence match.
X sequence mismatch.
--}

-- | take in *ByteLines* and transform them into groups of SamLines all refering to the same read
--
-- Header lines are silently ignored
readSamGroupsC :: (MonadError String m) => C.Conduit ByteLine m [SamLine]
readSamGroupsC = readSamLineOrDie =$= CL.groupBy groupLine
    where
        readSamLineOrDie = C.awaitForever $ \(ByteLine line) ->
            case readSamLine line of
                Left err -> throwError err
                Right parsed@SamLine{} -> C.yield parsed
                _ -> return ()
        groupLine = (==) `on` samQName



-- | a chunked variation of readSamGroupsC which uses multiple threads
--
-- When respectPairs is False, then the two mates of the same fragment will be
-- considered grouped in different blocks
readSamGroupsC' :: forall m . (MonadError String m, MonadBase IO m, MonadIO m) => Int -> Bool -> C.Conduit ByteLine m (V.Vector [SamLine])
readSamGroupsC' mapthreads respectPairs = do
        CC.dropWhile (isSamHeaderString . unwrapByteLine)
        CC.conduitVector 4096
            .| asyncMapEitherC mapthreads (liftM groupByName . V.mapM (readSamLine . unwrapByteLine))
            -- the groups may not be aligned on the group boundary, thus we need to fix them
            .| fixSamGroups
    where
        samQNameMate :: SamLine -> (B.ByteString, Bool)
        samQNameMate s
            | respectPairs = (samQName s, True)
            | otherwise = (samQName s, isFirstInPair s)
        groupByName :: V.Vector SamLine -> V.Vector [SamLine]
        groupByName vs = V.unfoldr groupByName' (0, error "null value", [])
            where
                groupByName' :: (Int, (B.ByteString, Bool), [SamLine]) -> Maybe ([SamLine], (Int, (B.ByteString, Bool), [SamLine]))
                groupByName' (ix, name, acc)
                        | ix == V.length vs = if null acc
                                                then Nothing
                                                else Just (reverse acc, (ix, error "null value", []))
                        | null acc = groupByName' (ix + 1, cur_tag, [cur])
                        | cur_tag == name = groupByName' (ix + 1, name, cur:acc)
                        | otherwise = Just (reverse acc, (ix, error "null value", []))
                    where
                        cur = vs V.! ix
                        cur_tag = samQNameMate cur
        fixSamGroups :: C.Conduit (V.Vector [SamLine]) m (V.Vector [SamLine])
        fixSamGroups = awaitJust fixSamGroups'
        fixSamGroups' :: V.Vector [SamLine] -> C.Conduit (V.Vector [SamLine]) m (V.Vector [SamLine])
        fixSamGroups' prev = C.await >>= \case
            Nothing -> C.yield prev
            Just cur -> case (V.last prev, V.head cur) of
                    ((lastprev:_),(curfirst:_))
                        | samQNameMate lastprev /= samQNameMate curfirst -> do
                            -- lucky case, the groups align with the vector boundary
                            C.yield prev
                            fixSamGroups' cur
                        | otherwise -> do
                            C.yield (V.slice 0 (V.length prev - 1) prev)
                            cur' <- liftIO $ injectLast (V.last prev) cur
                            fixSamGroups' cur'
                    _ -> error "This should have been an impossible situation in `fixSamGroups`"
        injectLast :: [SamLine] -> V.Vector [SamLine] -> IO (V.Vector [SamLine])
        injectLast prev gs = do
            gs' <- V.unsafeThaw gs
            VM.modify gs' (prev ++ ) 0
            V.unsafeFreeze gs'

