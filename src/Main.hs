{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Main where

import Data.Maybe ( fromMaybe )
import Control.Monad ( unless, forM_ )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Data.Version ( showVersion )
import Paths_agda_deps ( version )

import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Abstract.Name ( qnameToConcrete, QName )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend ( Backend_boot(..), Backend(..), Backend'_boot(..), Backend'(..), Recompile(..), IsMain )

import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
  ( TCM, withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma )

import Agda.Utils.Graph.AdjacencyMap.Unidirectional
  ( Graph, Edge )
import qualified Agda.Utils.Graph.AdjacencyMap.Unidirectional as Graph

import Agda.Main ( runAgda )

main = runAgda [Backend backend]

data Options = Options { optOutDir :: Maybe FilePath }

instance NFData Options where
  rnf _ = ()

outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts{ optOutDir = Just dir }

defaultOptions :: Options
defaultOptions = Options{ optOutDir = Nothing }

type ModuleEnv = ()
type ModuleRes = ()
type CompiledDef = Definition

backend :: Backend' Options Options ModuleEnv ModuleRes ADDef
backend = Backend'
  { backendName           = "agda-deps"
  , backendVersion        = Just (showVersion version)
  , options               = defaultOptions
  , commandLineFlags      =
      [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
        "Write output files to DIR. (default: project root)"
      ]
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = moduleSetup
  , postModule            = postModuleAD
  , compileDef            = compileDefAD
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }

moduleSetup :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
            -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ m _ = do
  setScope . iInsideScope =<< curIF
  return $ Recompile ()

data ADDef = ADDef {
    _name :: QName   -- Name of the definition
  , _deps :: [QName] -- its dependencies (named free variables)
  }

computeADDef :: Definition -> ADDef
computeADDef def@Defn{..} = ADDef { _name = defName, _deps = namesIn defType ++ namesIn theDef }

computeDepGraph :: [ADDef] -> Graph QName ()
computeDepGraph = Graph.fromEdges . concatMap defToEdges
  where
    defToEdges :: ADDef -> [Edge QName ()]
    defToEdges ADDef{..} = map (\t -> Graph.Edge _name t ()) _deps

compileDefAD :: Options -> ModuleEnv -> IsMain -> Definition -> TCM ADDef
compileDefAD opts env _ def = return (computeADDef def)

postModuleAD :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName -> [ADDef] -> TCM ()
postModuleAD opts env _ tlmn defs =
  let depGraph = computeDepGraph defs
  in liftIO $ forM_ (Graph.edges depGraph) $ do
                \e -> print $ prettyShow e
