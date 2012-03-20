import System 
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Regex
import Data.Maybe
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.List
import System.IO (hGetContents, hPutStrLn, hClose)
import System.Process (runInteractiveProcess, waitForProcess)
import Directory
import Debug.Trace

main = do
  args <- getArgs
  if length args == 0 then
    putStrLn "usage: (this) photoDir"
  else do
    let photoDir = head args
    let cmd = "sqlite3 happiage.sqlite3"
    --putStrLn "migrating images..." 
    photos <- getDirectoryContents photoDir
    --mapM_ print photos
    --(sin, sout, stderr, procHandle) <- runInteractiveProcess cmd [] Nothing Nothing
    mapM_ (\img -> putStrLn $ mkCmd img) $ filter isImage photos
    where
      isImage a = a /= "." && a/= ".."
      mkCmd imageName = "insert into picture (user, title, path, deleted) values(1,\""++imageName++"\", \"static/photo/"++imageName++"\", 0);"
