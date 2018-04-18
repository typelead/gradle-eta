data PrintStream = PrintStream @java.io.PrintStream

foreign import java unsafe "@static @field java.lang.System.out" stdout :: PrintStream

foreign import java unsafe println :: PrintStream -> String -> IO ()

myHello :: String
#if HELLO == 1
myHello = "Hello"
#else
myHello = "Bonjour"
#endif

main :: IO ()
main = println stdout $ myHello ++ " World!"
