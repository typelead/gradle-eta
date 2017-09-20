{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Lib

import Control.Monad
import qualified Data.Text as T
import Java
import Unsafe.Coerce

main :: IO ()
main = testMain
  [ jclass (getClass' :: JClass Test1)
  , jclass (getClass' :: JClass Test2)
  ]

data {-# CLASS "Test1 extends junit.framework.TestCase" #-} Test1 = Test1 (Object# Test1)
  deriving Class

testMessage1 :: Java Test1 ()
testMessage1 = assertEquals (toJString "Hello from Gradle Eta!") (toJString (Lib.message))

foreign export java "testMessage1" testMessage1 :: Java Test1 ()

data {-# CLASS "Test2 extends junit.framework.TestCase" #-} Test2 = Test2 (Object# Test2)
  deriving Class

testMessage2 :: Java Test2 ()
testMessage2 = assertEquals (toJava $ T.pack "Baz" :: JString) (toJString "Baz")

foreign export java "testMessage2" testMessage2 :: Java Test2 ()

-------------------------
-- Test Infrastructure --
-------------------------

foreign import java unsafe "getClass" unsafeGetClass
  :: Java a Bool

testMain :: [JClassAny] -> IO ()
testMain classes = do
  result <- runTests
  putStrLn $ "Ran " ++ show (getRunCount result) ++ " JUnit tests"
  when (not $ wasSuccessful result) $ do
    mapM_
      (putStrLn . fromJava . toString)
      (fromJava $ getFailures result :: [Failure])
    putStrLn $ "Failed with " ++ show (getFailureCount result) ++ " failures"
    java $ exit 1
  where
  runTests :: IO Result
  runTests = java $ do
    classes <- arrayFromList
      (map (\(JClassAny cls) -> unsafeCoerce cls) classes :: [JClass Object])
    junitCoreRun classes

getClass' :: Class a => JClass a
getClass' = getClass undefined

data JClassAny = forall a. Class a => JClassAny (JClass a)

jclass :: Class a => JClass a -> JClassAny
jclass = JClassAny

data {-# CLASS "java.lang.Class[]" #-} JClassArray a
  = JClassArray (Object# (JClassArray a))
  deriving Class

instance Class a => JArray (JClass a) (JClassArray a)

foreign import java unsafe "@static junit.framework.TestCase.assertEquals" assertEquals
  :: (a <: Object, b <: Object)
  => a -> b -> Java c ()

data {-# CLASS "org.junit.runner.Result" #-} Result = Result (Object# Result)
  deriving Class

foreign import java unsafe "wasSuccessful" wasSuccessful
  :: Result -> Bool

foreign import java unsafe "getFailures" getFailures
  :: Result -> List Failure

foreign import java unsafe "getFailureCount" getFailureCount
  :: Result -> Int

foreign import java unsafe "getRunCount" getRunCount
  :: Result -> Int

data {-# CLASS "org.junit.runner.notification.Failure" #-} Failure
  = Failure (Object# Failure)
  deriving Class

foreign import java unsafe "@static org.junit.runner.JUnitCore.runClasses" junitCoreRun
  :: JClassArray a -> Java b Result

foreign import java unsafe "@static java.lang.System.exit" exit
  :: Int -> Java a ()
