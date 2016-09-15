{-# LANGUAGE LambdaCase
           , TupleSections
           , NamedFieldPuns
           #-}
module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.IO
import qualified Data.Map as Map
import Data.Maybe
import Data.Function (on)
import Data.List
import Data.List.Split
import Control.Monad.State
import Control.Reference as Ref

import GHC
import SrcLoc
import HscTypes as GHC
import Module as GHC
import GHC.Paths ( libdir )
import FastString (unpackFS)

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.GetModules

main :: IO ()
main = refactorSession =<< getArgs

refactorSession :: [String] -> IO ()
refactorSession args = runGhc (Just libdir) $ 
  do initGhcFlags
     workingDirs <- useFlags args
     useDirs workingDirs
     if null workingDirs then liftIO $ putStrLn "Usage: ht-refact [ghc-flags] package-roots"
                         else do initSt <- initializeSession workingDirs
                                 flip evalStateT initSt $ runSession
     
  where initializeSession :: [FilePath] -> Ghc RefactorSessionState
        initializeSession workingDirs = do
          emptyPackages <- liftIO $ getAllModules workingDirs
          liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
          baseDF <- getSessionDynFlags
          forM_ emptyPackages $ \(mc@ModuleCollection{mcFlagSetup, mcModules}) -> 
            do setSessionDynFlags =<< liftIO (mcFlagSetup baseDF)
               forM_ (Map.keys mcModules) $ \mod -> 
                  addTarget (Target (TargetModule (mkModuleName (sfkModuleName mod))) True Nothing)
               load LoadAllTargets
          allMods <- getModuleGraph
          loadedModules <- loadModules emptyPackages allMods
          liftIO $ putStrLn "All modules loaded. Use 'SelectModule module-name' to select a module"
          liftIO $ hSetBuffering stdout NoBuffering
          return $ RefactorSessionState loadedModules Nothing False

        loadModules :: [ModuleCollection SourceFileKey ()] -> [ModSummary] -> Ghc [ModuleCollection AbsoluteSourceKey (UnnamedModule IdDom)]
        loadModules mcols summaries = 
          do modules <- forM summaries $ \ms -> 
                          do res <- parseTyped ms
                             liftIO $ putStrLn ("Loaded module: " ++ (GHC.moduleNameString $ moduleName $ ms_mod ms))
                             return res
             let roots = map getRootDir summaries 
                 modNames = map (GHC.moduleNameString . moduleName . ms_mod) summaries
                 boots = map (\ms -> case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot) summaries
                 newCollModKeys = map findCollAndKey (zip4 roots modNames boots modules)
                 newCollMods = map (map snd) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) newCollModKeys
             
             return $ zipWith (\mc nc -> mc { mcModules = Map.fromList nc }) mcols newCollMods
          where findCollAndKey :: (FilePath, String, IsBoot, a) -> (Int, (AbsoluteSourceKey, a))
                findCollAndKey (wd, modName, boot, mod) 
                  = ( fromJust $ findIndex (\col -> isJust $ find (\k -> sfkModuleName k == modName) $ Map.keys (mcModules col)) mcols
                    , (mkAbsSrcKey wd modName boot, mod) )

        runSession :: RefactorSession Ghc ()
        runSession = do 
          actualMod <- gets (^. actualMod)
          liftIO $ putStr (maybe "no-module-selected" askModule actualMod ++ ">")
          cmd <- liftIO $ getLine 
          sessionComm <- readSessionCommand cmd
          performSessionCommand sessionComm
          doExit <- gets (^. exiting)
          when (not doExit) runSession

getRootDir :: ModSummary -> FilePath
getRootDir ms = getRoot $ fromJust (ml_hs_file loc)
  where loc = ms_location ms
        getRoot path = iterate (</> "..") (takeDirectory path) !! depth
        depth = length $ filter (=='.') $ GHC.moduleNameString $ moduleName (ms_mod ms)

data RefactorSessionCommand 
  = LoadModule String
  | Exit
  | RefactorCommand RefactorCommand

readSessionCommand :: MonadIO m => String -> RefactorSession m RefactorSessionCommand
readSessionCommand cmd = case splitOn " " cmd of 
    ["SelectModule", mod] -> return $ LoadModule mod
    ["Exit"] -> return Exit
    _ -> do actualMod <- gets (^. actualMod)
            case actualMod of Just ask -> return $ RefactorCommand $ readCommand (askFile ask) cmd
                              Nothing -> error "Set the actual module first"

performSessionCommand :: RefactorSessionCommand -> RefactorSession Ghc ()
performSessionCommand (LoadModule mod) 
  = do -- TODO: make it possible to select a file by file path
       ask <- find (\ask -> askModule ask == mod && sfkIsBoot (askSrcKey ask) == NormalHs) <$> gets allAbsKeys
       if isJust ask then modify $ actualMod .= ask
                     else liftIO $ putStrLn ("Cannot find module: " ++ mod)
performSessionCommand Exit = modify $ exiting .= True
performSessionCommand (RefactorCommand cmd) 
  = do -- TODO: check if actual module is selected
       st@(RefactorSessionState { _modColls = mods, _actualMod = Just act }) <- get
       let Just collInd = findIndex (\coll -> act `Map.member` mcModules coll) mods
           Just refactoredModule = Map.lookup act $ mcModules $ mods !! collInd
           -- make sure that the filename is the same
           fileName = case (getRange (refactoredModule ^. annotation&sourceInfo)) of RealSrcSpan sp -> srcSpanFile sp
           updateFileName fn loc = mkRealSrcLoc fn (srcLocLine loc) (srcLocCol loc)
           cmd' = cmd { commandPos = mkRealSrcSpan (updateFileName fileName $ realSrcSpanStart $ commandPos cmd) (updateFileName fileName $ realSrcSpanEnd $ commandPos cmd) }
       
       res <- lift $ performCommand cmd' (askModule act, refactoredModule) (allModulesLoaded st)
       case res of Left err -> liftIO $ putStrLn err
                   Right resMods -> do 
                     -- update the contents of the files, check how these files can be recompiled
                     mss <- forM resMods $ \case 
                       ContentChanged (n,m) -> do
                         ms <- lookupModuleSummary m
                         let ask = askFromSummary ms
                             Just collInd = findIndex (\coll -> ask `Map.member` mcModules coll) mods
                         liftIO $ withBinaryFile (askFile ask) WriteMode (`hPutStr` prettyPrint m)
                         return $ Just (collInd, (ask, ms))
                       ModuleRemoved mod -> do
                         -- TODO: we need more information to identify the exact module !!!
                         --ms <- lookupModuleSummary m
                         --let ask = askFromSummary ms
                         --liftIO $ removeFile (askFile ask)
                         --modify $ modColls .- map (\coll -> coll { mcModules = Map.filter ask $ Map.delete ask $ mcModules coll } )
                         return Nothing
                     -- fetch the compilation flags for modules
                     let modGroups = map (map snd) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ catMaybes mss
                         modsWithFlags = zip modGroups (map mcFlagSetup mods)
                     -- compile each module with the correct flags
                     baseFlags <- lift getSessionDynFlags
                     forM_ modsWithFlags $ \(mods, flagSetup) -> lift $ do
                       setSessionDynFlags =<< liftIO (flagSetup baseFlags)
                       forM_ mods $ \(_, ms) -> load (LoadUpTo (moduleName $ ms_mod ms))
                     -- transform the compiled modules to our representation and store them in the session
                     forM_ (catMaybes mss) $ \(collInd, (ask, ms)) -> do
                         -- TODO: add target if module is added as a change
                         newm <- lift $ parseTyped ms
                         modify $ modColls & Ref.element collInd .- \mc -> mc { mcModules = Map.insert ask newm $ mcModules mc }
                         liftIO $ putStrLn ("Re-loaded module: " ++ askModule ask)

askFromSummary :: ModSummary -> AbsoluteSourceKey
askFromSummary ms = mkAbsSrcKey (getRootDir ms) (GHC.moduleNameString $ moduleName $ ms_mod ms) 
                                (case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot)

allAbsKeys :: RefactorSessionState -> [AbsoluteSourceKey]
allAbsKeys = concatMap (Map.keys . mcModules) . (^. modColls)

allModulesLoaded :: RefactorSessionState -> [ModuleDom IdDom]
allModulesLoaded = concatMap (map (\(ask,mod) -> (askModule ask, mod)) . Map.assocs . mcModules) . (^. modColls)