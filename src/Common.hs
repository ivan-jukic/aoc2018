module Common
( myTrace
, myTraceLn
) where


import           Debug.Trace   (trace)


myTrace :: Show a => String -> a -> a
myTrace msg val =
    trace ((msg ++) . show $ val ) val


myTraceLn :: Show a => String -> a -> a
myTraceLn msg val =
    trace (msg ++ (show val) ++ "\n") val
