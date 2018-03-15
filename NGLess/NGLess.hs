{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}
module NGLess
    ( NGLessIO(..)
    , KwArgsValues
    , error
    , testNGLessIO
    , runNGLess
    , Expression(..)
    , Variable(..)
    , Block(..)
    , FuncName(..)
    , MethodName(..)
    , NGLType(..)
    , NGLessObject(..)
    , FileOrStream(..)
    , asSamStream
    , samBamConduit
    ) where

import Data.Sam

import           Data.Conduit ((=$=))
import qualified Data.Conduit as C

import Utils.Conduit

import qualified Data.ByteString as B
import qualified Data.Conduit.Binary as CB
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Except

import qualified Data.Text as T

import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Control
import           Control.Monad.Base

-- This file should be a leaf in the import graph (i.e., not import any other NGLess modules)


newtype NGLessIO a = NGLessIO { unwrapNGLessIO :: ExceptT String (ResourceT IO) a }
                        deriving (Functor, Applicative, Monad, MonadIO,
                        MonadError String, MonadResource, MonadThrow,
                        MonadBase IO)


newtype NGLessIOStM a = NGLessIOStM { unwrapNGLessIOStM :: StM (ExceptT String (ResourceT IO)) a }

instance MonadBaseControl IO NGLessIO where
    type StM NGLessIO a = NGLessIOStM a
    liftBaseWith f =  NGLessIO $ liftBaseWith (\q -> f (fmap NGLessIOStM . q . unwrapNGLessIO))
    restoreM = NGLessIO . restoreM . unwrapNGLessIOStM


runNGLess :: (MonadError String m) => Either String a -> m a
runNGLess (Left err) = error err
runNGLess (Right v) = return v

testNGLessIO :: NGLessIO a -> IO a
testNGLessIO (NGLessIO act) = do
        perr <- (runResourceT . runExceptT) act
        return (showError perr)
    where
        showError (Right a) = a
        showError (Left e) = error (show e)

type KwArgsValues = [(T.Text, NGLessObject)]


newtype Variable = Variable T.Text
    deriving (Eq, Ord, Show)

newtype FuncName = FuncName { unwrapFuncName :: T.Text }
    deriving (Eq, Ord)

instance Show FuncName where
    show (FuncName f) = T.unpack f

newtype MethodName = MethodName { unwrapMethodName :: T.Text }
    deriving (Eq, Ord, Show)
-- | a block is
--  f(a) using |inputvariables|:
--      expression
data Block = Block
                { blockVariable :: [Variable] -- ^ input arguments
                , blockBody :: Expression -- ^ block body, will likely be Sequence
                }
    deriving (Eq, Ord, Show)

data NGLType =
        NGLString
        | NGLInteger
        | NGLDouble
        | NGLBool
        | NGLSymbol
        | NGLFilename
        | NGLRead
        | NGLReadSet
        | NGLMappedRead
        | NGLMappedReadSet
        | NGLSequenceSet
        | NGLCounts
        | NGLVoid
        | NGLAny
        | NGList !NGLType
    deriving (Eq, Ord, Show)

data NGLessObject =
        NGOString !T.Text
        | NGOBool !Bool
        | NGOInteger !Integer
        | NGODouble !Double
        | NGOSymbol !T.Text
        | NGOFilename !FilePath
        | NGOSequenceSet FileOrStream
        | NGOMappedReadSet
                    { nglgroupName :: T.Text
                    , nglSamFile :: FileOrStream
                    , nglReference :: Maybe T.Text
                    }
        | NGOMappedRead [SamLine]
        | NGOCounts FileOrStream
        | NGOVoid
        | NGOList [NGLessObject]
        | NGOExpression Expression
    deriving (Eq, Show)


-- | 'Expression' is the main type for holding the AST.

data Expression =
        Lookup (Maybe NGLType) Variable -- ^ This looks up the variable name
        | ConstStr T.Text -- ^ constant string
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName Expression [(Variable, Expression)] (Maybe Block)
        | MethodCall MethodName Expression (Maybe Expression) [(Variable, Expression)] -- ^ expr.method(expre)
        | Sequence [Expression]
    deriving (Eq, Ord)


instance Show Expression where
    show (Lookup (Just t) (Variable v)) = "Lookup '"++T.unpack v++"' as "++show t
    show (Lookup Nothing (Variable v)) = "Lookup '"++T.unpack v++"' (type unknown)"
    show (ConstStr t) = show t
    show (Assignment (Variable v) a) = T.unpack v++" = "++show a
    show (FunctionCall fname a args block) = show fname ++ "(" ++ show a ++ showArgs args ++ ")"
                                    ++ (case block of
                                        Nothing -> ""
                                        Just b -> "using {"++show b ++ "}")
    show (MethodCall mname self a args) = "(" ++ show self ++ ")." ++ show mname ++ "( " ++ show a ++ showArgs args ++ " )"
    show (Sequence e) = "Sequence " ++ show e

showArgs [] = ""
showArgs ((Variable v, e):args) = "; "++T.unpack v++"="++show e++showArgs args

-- | reads a SAM (possibly compressed) or BAM file (in the latter case by using
-- 'samtools view' under the hood)
samBamConduit :: FilePath -> C.Source NGLessIO B.ByteString
samBamConduit samfp = CB.sourceFile samfp


data FileOrStream = File FilePath | Stream FilePath (C.Source NGLessIO ByteLine)

instance Show FileOrStream where
    show (File fp) = "File " ++ fp
    show (Stream _ _) = "<STREAM>"

instance Eq FileOrStream where
    (File fp) == (File fp') = fp == fp'
    _ == _ = False


asSamStream (File fname) = (fname, samBamConduit fname =$= linesC)
asSamStream (Stream fname istream) = (fname, istream)

