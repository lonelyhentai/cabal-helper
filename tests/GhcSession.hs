{-# LANGUAGE TupleSections, ScopedTypeVariables, CPP #-}
module Main where

import GHC
#if __GLASGOW_HASKELL__ <= 706
import GhcMonad
#endif
import GHC.Paths (libdir)
import DynFlags

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Version
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>))
import System.Directory
import System.IO
import System.IO.Temp
import System.Process (readProcess)

import Distribution.Helper

import CabalHelper.Shared.Common


main :: IO ()
main = do
  args <- getArgs
  topdir <- getCurrentDirectory
  -- res <- mapM (setup topdir test) $ case args of
  --   [] -> [ ("tests/exelib"   , parseVer "1.10")
  --         , ("tests/exeintlib", parseVer "2.0")
  --         , ("tests/fliblib"  , parseVer "2.0")
  --         , ("tests/bkpregex" , parseVer "2.0")
  --         ]
  --   xs -> map (,parseVer "0") xs
  let
    testCases = case args of
      [] -> [ ("tests/exelib"   , parseVer "1.10")
            , ("tests/exeintlib", parseVer "2.0")
            , ("tests/fliblib"  , parseVer "2.0")
            , ("tests/bkpregex" , parseVer "2.0")
            ]
      xs -> map (,parseVer "0") xs

  -- resOld <- mapM (setup CabalOldBuild topdir test) testCases
  let resOld = []
  resNew <- mapM (setup CabalNewBuild topdir test) testCases

  if any (==False) $ concat (resOld ++ resNew)
    then exitFailure
    else exitSuccess

data CabalBuild = CabalOldBuild | CabalNewBuild
                deriving (Eq,Show)

cabalInstallVersion :: IO Version
cabalInstallVersion =
    parseVer . trim <$> readProcess "cabal" ["--numeric-version"] ""

cabalInstallBuiltinCabalVersion :: IO Version
cabalInstallBuiltinCabalVersion =
    parseVer . trim <$> readProcess "cabal"
        ["act-as-setup", "--", "--numeric-version"] ""

setup :: CabalBuild -> FilePath
      -> (CabalBuild -> FilePath -> IO [Bool]) -> (FilePath, Version) -> IO [Bool]
setup cb topdir act (srcdir, min_cabal_ver) = do
    ci_ver <- cabalInstallVersion
    c_ver <- cabalInstallBuiltinCabalVersion
    let mreason
          | (ci_ver < parseVer "1.24") =
            Just $ "cabal-install-" ++ showVersion ci_ver ++ " is too old"
          | c_ver < min_cabal_ver =
            Just $ "Cabal-" ++ showVersion c_ver
                   ++ " < " ++ showVersion min_cabal_ver
          | otherwise =
            Nothing

    case mreason of
      Just reason -> do
        putStrLn $ "Skipping test '" ++ srcdir ++ "' because " ++ reason ++ "."
        return []
      Nothing -> do
        putStrLn $ "Running test '" ++ srcdir ++ "' ------------------------------"
        withSystemTempDirectory "cabal-helper.ghc-session.test" $ \dir -> do
          setCurrentDirectory $ topdir </> srcdir
          run "cabal" [ "sdist", "--output-dir", dir ]

          setCurrentDirectory dir
          case cb of
            CabalOldBuild -> run "cabal" [ "configure" ]
            CabalNewBuild -> run "cabal" [ "new-configure" ]

          act cb dir

run :: String -> [String] -> IO ()
run x xs = do
  print $ x:xs
  o <- readProcess x xs ""
  putStrLn o
  return ()

test :: CabalBuild -> FilePath -> IO [Bool]
test cb dir = do
    let qe = case cb of
          CabalOldBuild -> mkQueryEnv dir (dir </> "dist")
          CabalNewBuild -> mkQueryEnv dir (dir </> "dist-newstyle")
    cs <- runQuery qe $ components $ (,,,) <$> entrypoints <.> ghcOptions <.> needsBuildOutput
    forM cs $ \(ep, opts, nb, cn) -> do

        putStrLn $ "\n" ++ show cn ++ ":::: " ++ show nb

        when (nb == ProduceBuildOutput) $ do
          case cb of
            CabalOldBuild -> run "cabal" [ "build" ]
            CabalNewBuild -> run "cabal" [ "new-build" ]

        let opts' = "-Werror" : opts

        let sopts = intercalate " " $ map formatArg $ "\nghc" : opts'
        putStrLn $ "\n" ++ show cn ++ ": " ++ sopts
        hFlush stdout
        compileModule nb ep opts'
  where
    formatArg x
        | "-" `isPrefixOf` x = "\n  "++x
        | otherwise          = x


compileModule :: NeedsBuildOutput -> ChEntrypoint -> [String] -> IO Bool
compileModule nb ep opts = do

    putStrLn $ "compiling:" ++ show ep ++ " (" ++ show nb ++ ")"

    E.handle (\(ec :: ExitCode) -> print ec >> return False) $ do

#if __GLASGOW_HASKELL__ <= 704
    defaultErrorHandler defaultLogAction $ do
#else
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
#endif

    runGhc (Just libdir) $ do

    handleSourceError (\e -> GHC.printException e >> return False) $ do

    let target = case nb of
          ProduceBuildOutput -> HscNothing -- AZ: what should this be?
          NoBuildOutput      -> HscInterpreted

    dflags0 <- getSessionDynFlags
    let dflags1 = dflags0 {
        ghcMode   = CompManager
      , ghcLink   = LinkInMemory
      , hscTarget = target
      , optLevel  = 0
      }

    (dflags2, _, _) <- parseDynamicFlags dflags1 (map noLoc opts)
    _ <- setSessionDynFlags dflags2

    ts <- mapM (\t -> guessTarget t Nothing) $
         case ep of
           ChLibEntrypoint ms ms' ss -> map unChModuleName $ ms ++ ms' ++ ss
           ChExeEntrypoint m'  ms    ->
             let

               -- The options first clear out includes, then put in the build dir. We want the
               -- first one after that, so "regex-example" in the following case
               --
               -- ,"-i"
               -- ,"-idist/build/regex-example"
               -- ,"-iregex-example"
               firstInclude = drop 2 $ head $ drop 2 $ filter (isPrefixOf "-i") opts
               m = firstInclude </> m'
             in [m] ++ map unChModuleName ms
           ChSetupEntrypoint         -> ["Setup.hs"]

    let ts' = case nb of
                NoBuildOutput -> map (\t -> t { targetAllowObjCode = False }) ts
                ProduceBuildOutput -> ts

    setTargets ts'
    _ <- load LoadAllTargets

#if __GLASGOW_HASKELL__ >= 706
    when (nb == NoBuildOutput) $ do
      setContext $ case ep of
        ChLibEntrypoint ms ms' ss ->
            map (IIModule . mkModuleName . unChModuleName) $ ms ++ ms' ++ ss
        ChExeEntrypoint _  ms  ->
            map (IIModule . mkModuleName . unChModuleName) $ ChModuleName "Main" : ms
        ChSetupEntrypoint      ->
            map (IIModule . mkModuleName) ["Main"]
#endif

    liftIO'CH $ print ExitSuccess
    return True

unChModuleName :: ChModuleName -> String
unChModuleName (ChModuleName  mn) = mn

liftIO'CH :: IO a -> Ghc a
#if __GLASGOW_HASKELL__ <= 706
liftIO'CH = GhcMonad.liftIO
#else
liftIO'CH = liftIO
#endif
