{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}
module NGLess
    ( NGLessIO(..)
    ) where


import           Control.Monad.Except

import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Control
import           Control.Monad.Base

-- This file should be a leaf in the import graph (i.e., not import any other NGLess modules)


newtype NGLessIO a = NGLessIO { unwrapNGLessIO :: (ResourceT IO a) }
                        deriving (Functor, Applicative, Monad, MonadIO,
                        MonadResource, MonadThrow,
                        MonadBase IO)


newtype NGLessIOStM a = NGLessIOStM { unwrapNGLessIOStM :: StM (ResourceT IO) a }

instance MonadBaseControl IO NGLessIO where
    type StM NGLessIO a = NGLessIOStM a
    liftBaseWith f =  NGLessIO $ liftBaseWith (\q -> f (fmap NGLessIOStM . q . unwrapNGLessIO))
    restoreM = NGLessIO . restoreM . unwrapNGLessIOStM

