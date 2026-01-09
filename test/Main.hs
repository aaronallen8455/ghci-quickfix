module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.List (isInfixOf)
import qualified System.Directory as Dir
import           System.IO
import qualified System.Process as Proc
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "ParseError1" $ runTest "ParseError1"
  , testCase "TypeErr1" $ runTest "TypeErr1"
  , testCase "UnusedVar" $ runTest "UnusedVar"
  , testCase "IncompletePat" $ runTest "IncompletePat"
  , testCase "MultiMod" $ runTest "MultiMod"
  , testCase "WarningFix" runTestWarningFix
  ]

testModulePath :: String -> FilePath
testModulePath name = "test-modules/" <> name

-- Wait for GHCi prompt by reading output until we see "ghci>"
waitForPrompt :: Handle -> IO ()
waitForPrompt hOut = go ""
  where
    go acc = do
      ready <- hReady hOut
      if ready
        then do
          c <- hGetChar hOut
          let acc' = take 10 (c : acc)  -- Keep last 10 chars
          if "ghci>" `isInfixOf` reverse acc'
            then threadDelay 300_000  -- Brief delay for background thread
            else go acc'
        else do
          threadDelay 10_000  -- 10ms before checking again
          go acc

runTest :: String -> Assertion
runTest name = do
  let qfFile = testModulePath (name ++ ".qf")
  -- Remove any existing quickfix file
  qfExists <- Dir.doesFileExist qfFile
  when qfExists $ Dir.removeFile qfFile

  -- Use cabal repl to keep GHC alive long enough for background thread
  (Just stdin, Just stdout, _, h) <- Proc.createProcess
    (Proc.proc "cabal" ["repl", "test-modules:" ++ name])
      { Proc.std_in = Proc.CreatePipe
      , Proc.std_out = Proc.CreatePipe
      , Proc.std_err = Proc.CreatePipe
      }
  hSetBuffering stdin NoBuffering

  -- Wait for GHCi prompt
  waitForPrompt stdout

  -- Quit gracefully, ignoring errors if pipe is already closed
  void $ hPutStrLn stdin ":quit"
  void $ hClose stdin
  void $ Proc.waitForProcess h

  -- Check that quickfix file was created and has expected contents
  actualContents <- readFile qfFile
  expectedContents <- readFile $ qfFile ++ ".expected"
  assertEqual "Expected quickfix output" expectedContents actualContents

  -- Clean up
  Dir.removeFile qfFile

runTestWarningFix :: Assertion
runTestWarningFix = do
  let qfFile = testModulePath "WarningFix.qf"
      mod2File = testModulePath "WarningFix2.hs"
      mod2Broken = testModulePath "WarningFix2.hs.broken"
      mod2Fixed = testModulePath "WarningFix2.hs.fixed"

  -- Remove any existing quickfix file
  qfExists <- Dir.doesFileExist qfFile
  when qfExists $ Dir.removeFile qfFile

  -- Start with broken version (no type signature, has warning)
  Dir.copyFile mod2Broken mod2File

  -- Use cabal repl
  (Just stdin, Just stdout, _, h) <- Proc.createProcess
    (Proc.proc "cabal" ["repl", "test-modules:WarningFix"])
      { Proc.std_in = Proc.CreatePipe
      , Proc.std_out = Proc.CreatePipe
      , Proc.std_err = Proc.CreatePipe
      }
  hSetBuffering stdin NoBuffering

  -- Wait for GHCi prompt after initial compilation
  waitForPrompt stdout

  -- Check that quickfix file was created
  qfExists1 <- Dir.doesFileExist qfFile
  assertBool "Quickfix file should exist after initial compilation" qfExists1

  -- Check that quickfix file has error and warning
  actualContents1 <- readFile qfFile
  assertBool "Should have error and warning initially"
    (("error:" `isInfixOf` actualContents1) && ("warning:" `isInfixOf` actualContents1))

  -- Now fix the warning by copying fixed version
  Dir.copyFile mod2Fixed mod2File

  -- Reload in the repl
  void $ try @SomeException $ hPutStrLn stdin ":reload"

  -- Wait for GHCi prompt after reload
  waitForPrompt stdout

  -- Check that quickfix file now only has error, no warning
  actualContents2 <- readFile qfFile
  expectedContents <- readFile $ qfFile ++ ".expected"
  assertEqual "Expected only error after fix" expectedContents actualContents2

  -- Quit gracefully
  void $ hPutStrLn stdin ":quit"
  void $ hClose stdin
  void $ Proc.waitForProcess h

  -- Clean up
  Dir.removeFile qfFile
  Dir.removeFile mod2File
