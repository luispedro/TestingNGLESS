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

import NGLess
import Count


data NGLInterpretEnv = NGLInterpretEnv ()
type InterpretationEnvIO = StateT  NGLInterpretEnv NGLessIO

interpret :: [(Int,Expression)] -> NGLessIO ()
interpret es = do
    evalStateT (interpretIO es) (NGLInterpretEnv ())

interpretIO :: [(Int, Expression)] -> InterpretationEnvIO ()
interpretIO es = forM_ es $ \(_,e) -> do
    interpretTop e

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (FunctionCall f _ _) = void $ lift (performCount "testing.sam")
interpretTop _ = error "Top level statement is NOP"
