data PrintStream = PrintStream @java.io.PrintStream

foreign import java unsafe "@static @field java.lang.System.out" stdout :: PrintStream

foreign import java unsafe println :: PrintStream -> String -> IO ()

main :: IO ()
main = println stdout $ "Hello World!"
