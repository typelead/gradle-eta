import Lib

data PrintStream = PrintStream @java.io.PrintStream

foreign import java unsafe "@static @field java.lang.System.out" stdout :: PrintStream

foreign import java unsafe println :: PrintStream -> String -> IO ()

myHello :: String
#if HELLO == 1
myHello = myHello1
#else
myHello = myHello2
#endif

main :: IO ()
main = println stdout $ myHello ++ " World!"
