module Common
( myTrace
, myTraceLn
, inputLines
) where


import           Debug.Trace   (trace)
import qualified Data.Text     as T


myTrace :: Show a => String -> a -> a
myTrace msg val =
    trace ((msg ++) . show $ val ) val


myTraceLn :: Show a => String -> a -> a
myTraceLn msg val =
    trace (msg ++ (show val) ++ "\n") val


inputLines ::  String -> [String]
inputLines = map trim . lines


trim :: String -> String
trim = T.unpack . T.strip . T.pack