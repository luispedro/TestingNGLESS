{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}
module NGLess
    ( NGLessIO(..)
    , KwArgsValues
    , testNGLessIO
    , runNGLess
    , Expression(..)
    , Variable(..)
    , FuncName(..)
    , NGLessObject(..)
    , ByteLine(..)
    , linesC
    ) where

import           Data.Conduit ((=$=))
import qualified Data.Conduit as C


import qualified Data.ByteString as B
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
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


data NGLessObject =
        NGOString !T.Text
        | NGOBool !Bool
        | NGOInteger !Integer
        | NGODouble !Double
        | NGOSymbol !T.Text
        | NGOFilename !FilePath
        | NGOMappedReadSet
                    { nglgroupName :: T.Text
                    , nglSamFile :: FilePath
                    , nglReference :: Maybe T.Text
                    }
        | NGOVoid
    deriving (Eq, Show)


-- | 'Expression' is the main type for holding the AST.

data Expression =
        Lookup Variable -- ^ This looks up the variable name
        | ConstStr T.Text -- ^ constant string
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName Expression [(Variable, Expression)]
    deriving (Eq, Ord)


-- | This just signals that a "line" is expected.
newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }
                deriving (Show)

linesC :: (Monad m) => C.Conduit B.ByteString m ByteLine
linesC = CB.lines =$= CL.map ByteLine
{-# INLINE linesC #-}

