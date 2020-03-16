{-# LANGUAGE CPP #-}
import Distribution.Simple
#if MIN_VERSION_Cabal(1,25,0)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
#else
import Distribution.Simple.BuildPaths (autogenModulesDir)
#endif
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.PackageDescription (PackageDescription, emptyHookedBuildInfo)
import Distribution.Simple.LocalBuildInfo (withLibLBI)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (readProcess)


main = defaultMainWithHooks $ simpleUserHooks { buildHook = myBuildHook }

myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook packageDescription buildInfo userHooks buildFlags =
#if MIN_VERSION_Cabal(1,25,0)
  withLibLBI packageDescription buildInfo $ \_ libCfg -> do
    createDirectoryIfMissing True (autogenComponentModulesDir buildInfo libCfg)
    writeCustomFile (autogenComponentModulesDir buildInfo libCfg </> "Build_elm_instrument.hs")
    buildHook simpleUserHooks packageDescription buildInfo userHooks buildFlags
#else
  do
    createDirectoryIfMissing True (autogenModulesDir buildInfo)
    writeCustomFile (autogenModulesDir buildInfo </> "Build_elm_instrument.hs")
    buildHook simpleUserHooks packageDescription buildInfo userHooks buildFlags
#endif


writeCustomFile :: FilePath -> IO ()
writeCustomFile filepath = do
  putStrLn $ "Generating " ++ filepath ++ "..."

  desc <- readProcess "git" ["describe", "--abbrev=8", "--always"] ""
  now <- readProcess "date" ["+%s"] ""

  writeFile filepath $ unlines
      [ "module Build_elm_instrument where"
      , ""
      , "gitDescribe :: String"
      , "gitDescribe = " ++ show (init desc)
      ]
