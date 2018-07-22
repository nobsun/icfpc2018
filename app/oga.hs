import System.Environment

import TraceEncoder (writeTraceFile)
import NaiveBot (getAssembleTrace', getDisassembleTrace')
import Model (Model(..), readModel)
import State (Trace, Command(..))

main :: IO ()
main = do
  args <- getArgs
  mapM_ f args
  where
    f :: String -> IO ()
    f mdl = do
      model <- readModel mdl
      let trs = (method mdl) model
--      putStrLn ("resolution: " ++ show (mdResolution model))
--      mapM_ print trs
      writeTraceFile (mkNbtName mdl) (concat trs)

mkNbtName :: String -> String
mkNbtName str =
  name ++ ".nbt"
  where
    name = takeWhile (/='_') str

method :: String -> Model -> [Trace]
method fn =
  case take 2 fn of
    "FA" -> getAssembleTrace'
    "FD" -> getDisassembleTrace'
    _    -> const [[Halt]]

