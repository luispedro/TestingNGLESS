{- Copyright 2015-2017 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}
module Interpret
    ( interpret
    ) where
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map

import qualified Data.Text as T


import NGLess

import Interpretation.Count


type SimpleVariableMap = Map.Map T.Text NGLessObject
data VariableMap = VariableMapGlobal SimpleVariableMap

variableMapLookup v (VariableMapGlobal sm) = Map.lookup v sm

data NGLInterpretEnv = NGLInterpretEnv
    { ieVariableEnv :: VariableMap
    }

-- Monad 1: IO + read-write environment
type InterpretationEnvIO = StateT  NGLInterpretEnv NGLessIO
-- Monad 2: pure read-only environment
type InterpretationROEnv = ReaderT NGLInterpretEnv (Either String)

runNGLessIO :: NGLessIO a -> InterpretationEnvIO a
runNGLessIO = lift

runInROEnvIO :: InterpretationROEnv a -> InterpretationEnvIO a
runInROEnvIO act = do
    env <- get
    runNGLess $ runReaderT act env

lookupVariable :: T.Text -> InterpretationROEnv (Maybe NGLessObject)
lookupVariable !k = variableMapLookup k . ieVariableEnv <$> ask

setVariableValue :: T.Text -> NGLessObject -> InterpretationEnvIO ()
setVariableValue !k !v = modify $ \(NGLInterpretEnv (VariableMapGlobal vm)) -> (NGLInterpretEnv (VariableMapGlobal (Map.insert k v vm)))



interpret :: [(Int,Expression)] -> NGLessIO ()
interpret es = do
    evalStateT (interpretIO es) (NGLInterpretEnv $ VariableMapGlobal Map.empty)

interpretIO :: [(Int, Expression)] -> InterpretationEnvIO ()
interpretIO es = forM_ es $ \(ln,e) -> do
    interpretTop e

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (Assignment (Variable var) val) = interpretTopValue val >>= setVariableValue var
interpretTop (FunctionCall f e args b) = void $ interpretFunction f e args b
interpretTop _ = error "Top level statement is NOP"

interpretTopValue :: Expression -> InterpretationEnvIO NGLessObject
interpretTopValue (FunctionCall f e args b) = interpretFunction f e args b
interpretTopValue e = runInROEnvIO (interpretExpr e)

interpretExpr :: Expression -> InterpretationROEnv NGLessObject
interpretExpr (Lookup _ (Variable v)) = lookupVariable v >>= \case
        Nothing -> error ("Could not lookup variable `"++show v++"`")
        Just r' -> return r'
interpretExpr (ConstStr s) = return $ NGOString s
interpretExpr not_expr = error ("Expected an expression, received " ++ show not_expr ++ " (in interpretExpr)")

interpretFunction :: FuncName -> Expression -> [(Variable, Expression)] -> Maybe Block -> InterpretationEnvIO NGLessObject
interpretFunction f expr args block = do
    expr' <- interpretTopValue expr
    args' <- interpretArguments args
    interpretFunction' f expr' args' block

interpretFunction' :: FuncName -> NGLessObject -> KwArgsValues -> Maybe Block -> InterpretationEnvIO NGLessObject
interpretFunction' (FuncName "samfile")   expr args Nothing = executeSamfile expr args
interpretFunction' (FuncName "count")     expr args Nothing = runNGLessIO (executeCount expr args)
interpretFunction' f _ _ _ = error . concat $ ["Interpretation of ", show f, " is not implemented"]

executeSamfile (NGOString fname) _ = do
    let fname' = T.unpack fname
    return $ NGOMappedReadSet "testing" (File fname') Nothing
executeSamfile e args = error ("executeSamfile " ++ show e ++ " " ++ show args)

interpretArguments :: [(Variable, Expression)] -> InterpretationEnvIO [(T.Text, NGLessObject)]
interpretArguments args =
    forM args $ \(Variable v, e) -> do
        e' <- interpretTopValue e
        return (v, e')
