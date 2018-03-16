module Interpret
    ( interpret
    ) where
import qualified Data.ByteString as B

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Conduit ((.|))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Trans.Resource

type NGLessIO = ResourceT IO
type InterpretationEnvIO = StateT  () NGLessIO

interpret :: [()] -> NGLessIO ()
interpret es = evalStateT (interpretIO es) ()

interpretIO :: [()] -> InterpretationEnvIO ()
interpretIO es = forM_ es interpretTop

interpretTop :: () -> InterpretationEnvIO ()
interpretTop () = void $ lift (parseFile "testing.sam")

countC = loop (0 :: Int)
    where
        loop !n = C.await >>= \case
                            Nothing -> return n
                            Just{} -> loop (n+1)
-- The following variation has bad memory behaviour WITH & WITHOUT -O2:
--      loop !n = C.await >>= maybe (return n) (const $ loop (n+1))

isHeader :: B.ByteString -> Bool
isHeader s = not (B.null s) && (B.head s == 64) -- 64 is '@'

parseFile :: FilePath  -> NGLessIO FilePath
parseFile istream = do
    C.runConduit $
        CC.sourceFile istream
            .| CB.lines
            .| do
                c0 <- CC.takeWhile isHeader .| countC
                liftIO $ print c0
                c <- countC
                liftIO $ print c
                return ()
    return "test.txt"


