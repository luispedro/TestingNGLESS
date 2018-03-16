module Main
    ( main
    ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource

import NGLess
import Interpret

runNGLessIO :: NGLessIO a -> IO ()
runNGLessIO (NGLessIO act) = runResourceT (runExceptT act) >> return ()

main =
    runNGLessIO $
        interpret [(1, (FunctionCall (FuncName "count") (ConstStr "testing.sam") []))]


