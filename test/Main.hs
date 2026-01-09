module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (SomeException, try)
import           Control.Monad
import           System.IO (hPutStrLn, hClose)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified System.Directory as Dir
import qualified System.Process as Proc

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "ParseError1" $ runTest "ParseError1"
  ]

testModulePath :: String -> FilePath
testModulePath name = "test-modules/" <> name

runTest :: String -> Assertion
runTest name = do
  let qfFile = testModulePath (name ++ ".qf")
  -- Remove any existing quickfix file
  qfExists <- Dir.doesFileExist qfFile
  when qfExists $ Dir.removeFile qfFile

  -- Use cabal repl to keep GHC alive long enough for background thread
  (Just stdin, _, _, h) <- Proc.createProcess
    (Proc.proc "cabal" ["repl", "test-modules:" ++ name])
      { Proc.std_in = Proc.CreatePipe
      , Proc.std_out = Proc.CreatePipe
      , Proc.std_err = Proc.CreatePipe
      }

  -- Wait for compilation and background thread to write (200ms delay in plugin)
  threadDelay 20_000

  -- Quit gracefully, ignoring errors if pipe is already closed
  void $ try @SomeException $ hPutStrLn stdin ":quit"
  void $ try @SomeException $ hClose stdin
  void $ Proc.waitForProcess h

  -- Check that quickfix file was created and has expected contents
  actualContents <- readFile qfFile
  expectedContents <- readFile $ qfFile ++ ".expected"
  assertEqual "Expected quickfix output" expectedContents actualContents

  -- Clean up
  Dir.removeFile qfFile
