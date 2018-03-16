module Main
    ( main
    ) where

import Control.Monad.Trans.Resource

import Interpret

main = runResourceT $ interpret [(0, "count")]


