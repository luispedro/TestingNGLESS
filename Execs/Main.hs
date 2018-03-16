module Main
    ( main
    ) where

import Control.Monad.Trans.Resource
import Interpret

main = runResourceT $ interpret [()]


