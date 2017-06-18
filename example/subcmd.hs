{-# LANGUAGE DataKinds #-}

import           Control.Monad.Trans
import           Options.Declarative
import           System.IO

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Flag "" '["decolate"] "" "decolate message" Bool
      -> Arg "NAME" String
      -> Arg "suffix" (Def "uh" String)
      -> Cmd "Greeting command" ()
greet msg deco name suf = do
    let f x | get deco = "*** " ++ x ++ " ***"
            | otherwise = x
    liftIO $ putStrLn $ f $ get msg ++ ", " ++ get name ++ ", " ++ get suf ++ "!"

connect :: Flag "h" '["host"] "HOST" "host name"   (Def "localhost" String)
        -> Flag "p" '["port"] "PORT" "port number" (Def "8080"      Int   )
        -> Cmd "Connect command" ()
connect host port = do
    let addr = get host ++ ":" ++ show (get port)
    liftIO $ putStrLn $ "connect to " ++ addr

getOptExample
    :: Flag "o" '["output"] "FILE" "output FILE" (Def "stdout" String)
    -> Flag "c" '[] "FILE" "input FILE" (Def "stdin" String)
    -> Flag "L" '["libdir"] "DIR" "library directory" String
    -> Cmd "GetOpt example" ()
getOptExample output input libdir =
    liftIO $ print (get output, get input, get libdir)

getHead :: Flag "n" '[] "NUM" "number of lines" (Def "10" Int)
        -> Arg "FILE" (Def "stdin" (IO Handle))
        -> Cmd "head command" ()
getHead n handle = do
    handle' <- liftIO $ get handle
    contents <- liftIO $ hGetContents handle'
    liftIO $ putStrLn $ unlines $ take (get n) $ lines contents


main :: IO ()
main = run_ $
    Group "Test program for sub commands"
    [ subCmd "greet"   greet
    , subCmd "connect" connect
    , subCmd "getopt"  getOptExample
    , subCmd "head"   getHead
    ]
