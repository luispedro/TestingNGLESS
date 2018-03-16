module Main
    ( main
    ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource

import NGLess
import Interpret

runNGLessIO :: NGLessIO a -> IO ()
runNGLessIO (NGLessIO act) = runResourceT (runExceptT act) >> return ()

main = do
    runNGLessIO $
        interpret [(0, (Assignment (Variable "t") (FunctionCall (FuncName "samfile") (ConstStr "testing.sam") []))),
                (1, (FunctionCall (FuncName "count") (Lookup (Variable "t")) [(Variable "features", ConstStr "seqname")]))]


