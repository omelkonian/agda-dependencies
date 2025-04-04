{-# LANGUAGE LambdaCase, RecordWildCards, PatternSynonyms, DoAndIfThenElse #-}
module Main where

import Control.Monad ( unless, forM_, filterM )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import Data.Maybe ( fromMaybe, catMaybes, isJust, mapMaybe )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S

import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Data.Version ( showVersion )
import Paths_agda_deps ( version )

import Agda.Utils.Lens ( (^.) )
import Agda.Utils.Graph.AdjacencyMap.Unidirectional ( Graph, Edge )
import qualified Agda.Utils.Graph.AdjacencyMap.Unidirectional as Graph

import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Abstract.Name ( qnameToConcrete, QName )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.Syntax.Translation.InternalToAbstract ( reify )

import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Pretty ( text, prettyTCM )

import Agda.TypeChecking.Monad
  ( TCM, withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma
  , Definition(..), Defn(..)
  , pattern Function, funWith, funExtLam, funInline, funProjection, funInline, funIsKanOp
  , pattern DataOrRecSig, pattern PrimitiveSort, pattern Primitive
  , reportSLn, VerboseLevel
  )
import Agda.TypeChecking.Monad.Debug ( reportSDoc )
import Agda.TypeChecking.Monad.Signature ( getConstInfo )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend ( Backend_boot(..), Backend(..), Backend'_boot(..), Backend'(..), Recompile(..), IsMain )

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
type ModuleRes = [Maybe ADDef]

backend :: Backend' Options Options ModuleEnv ModuleRes (Maybe ADDef)
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
  , postCompile           = postCompileAD
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

-- ** computing the dependencies

data ADDef = ADDef
  { _name :: QName   -- Name of the definition
  , _deps :: [QName] -- its dependencies (named free variables)
  } deriving (Show)

computeADDef :: Definition -> TCM ADDef
computeADDef def@Defn{..} = do
  let deps = namesIn defType ++ namesIn theDef
  deps' <- filterM (fmap not . ignoreDependency) deps
  -- liftIO $ putStrLn $ "names in " <> prettyShow defName <> ": " <> prettyShow deps'
  return $ ADDef { _name = defName, _deps = deps' }

compileDefAD :: Options -> ModuleEnv -> IsMain -> Definition -> TCM (Maybe ADDef)
compileDefAD opts env _ def = do
  -- liftIO $ putStrLn $ prettyShow def
  if ignoreDef (theDef def) then
    return Nothing
  else
    Just <$> computeADDef def

postModuleAD :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName -> [Maybe ADDef] -> TCM [Maybe ADDef]
postModuleAD opts env _ tlmn defs = return defs

-- ** constructing the graph

postCompileAD :: Options -> IsMain -> (Map TopLevelModuleName [Maybe ADDef]) -> TCM ()
postCompileAD opts _ defMap = do
  let defs = catMaybes $ concat (M.elems defMap)
  -- liftIO $ putStrLn $ "defs: " <> show defs
  let depGraph = computeDepGraph defs
  liftIO $ forM_ (Graph.edges depGraph) $ do
              \e -> print $ prettyShow e
  where
  computeDepGraph :: [ADDef] -> Graph QName ()
  computeDepGraph defs = Graph.fromEdges $ concatMap defToEdges defs
    where
    defToEdges :: ADDef -> [Edge QName ()]
    defToEdges adef = flip mapMaybe (_deps adef) $ \t ->
      if t `elem` map _name defs then
        Just (Graph.Edge (_name adef) t ())
      else
        Nothing

-- ** things to ignore

ignoreDependency :: QName -> TCM Bool
ignoreDependency qn = do
  def <- getConstInfo qn
  return $ ignoreDef (theDef def)

ignoreDef :: Defn -> Bool
ignoreDef = \case
  -- ** ignore functions that are pattern-lambdas/with-generated/Kan operations
  Function{..} | isJust funExtLam || isJust funWith || isJust funIsKanOp -> True
  -- ** ignore inlined functions (arising from instantiated modules)
  d@Function{..} | d ^. funInline -> True
  -- Primitive{..} -> True
  PrimitiveSort{..} -> True
  DataOrRecSig{..} -> True
  GeneralizableVar -> True
  -- ** consider everything else
  _ -> False


