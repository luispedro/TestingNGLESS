{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}
module NGLess
    ( NGLessIO(..)
    , KwArgsValues
    , runNGLess
    , Expression(..)
    , Variable(..)
    , FuncName(..)
    , NGLessObject(..)
    ) where


import qualified Data.ByteString as B
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


data Expression =
        Lookup Variable -- ^ This looks up the variable name
        | ConstStr T.Text -- ^ constant string
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName Expression [(Variable, Expression)]
    deriving (Eq, Ord)


