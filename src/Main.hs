{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( unless, forM_, filterM )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import Data.Maybe ( fromMaybe, catMaybes, isJust, mapMaybe )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Foldable ( foldr1 )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Agda.Utils.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )
import Agda.Utils.Hash ( hashString )
import Data.Bifunctor (second)

import Data.Version ( showVersion )
import Paths_agda_deps ( version )

import Agda.Utils.Lens ( (^.) )

import Data.GraphViz
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph

import Agda.Syntax.Scope.Base ( Scope, allThingsInScope, NameSpace (nsInScope), _scopeInScope )
import Agda.Syntax.Scope.Monad ( getCurrentScope )

import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Abstract.Name ( ModuleName, Name , qnameToConcrete, QName )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.Syntax.Translation.InternalToAbstract ( reify )

import Agda.Syntax.Common.Pretty ( Pretty(..), prettyShow, (<+>), vcat, pshow )
import Agda.TypeChecking.Pretty ( text, prettyTCM )

import Agda.TypeChecking.Monad
  ( TCM, withCurrentModule, iInsideScope, setScope, getScope
  , CompilerPragma(..), getUniqueCompilerPragma
  , Definition(..), Defn(..)
  , pattern Function, funWith, funExtLam, funInline, funProjection, funInline, funIsKanOp
  , pattern Primitive, primClauses
  , pattern PrimitiveSort
  , pattern Axiom
  , pattern DataOrRecSig
  , reportSLn, VerboseLevel
  , liftTCM
  )
import Agda.TypeChecking.Monad.Debug ( reportSDoc )
import Agda.TypeChecking.Monad.Signature ( getConstInfo )

import Agda.Compiler.Common ( curIF, compileDir, IsMain(..) )
import Agda.Compiler.Backend ( Backend_boot(..), Backend(..), Backend'_boot(..), Backend'(..), Recompile(..), IsMain )

import Agda.Main ( runAgda )

main :: IO ()
main = runAgda [Backend backend]

data Options = Options { optOutDir :: Maybe FilePath }

instance NFData Options where
  rnf _ = ()

outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts{ optOutDir = Just dir }

defaultOptions :: Options
defaultOptions = Options{ optOutDir = Nothing }

data ModuleEnv = ModuleEnv { namesInScope :: Set QName }
type ModuleRes = [Maybe ADDef]

backend :: Backend' Options Options ModuleEnv ModuleRes (Maybe ADDef)
backend = Backend'
  { backendName           = "agda-deps"
  , backendVersion        = Just . T.pack . showVersion $ version
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
  allNamesInScope <- nsInScope . allThingsInScope <$> liftTCM getCurrentScope
  return $ Recompile (ModuleEnv allNamesInScope)

-- ** computing the dependencies

data ADDef = ADDef
  { _name  :: QName     -- name of the definition
  , _deps  :: Set QName -- its dependencies (named free variables)
  } deriving (Show)

instance Pretty ADDef where
  pretty ADDef{..} = vcat [ pshow "Name:" <+> pretty _name
                          , pshow "Deps:" <+> pretty _deps ]

computeDefAD :: Definition -> TCM ADDef
computeDefAD def@Defn{..} = do
  deps <- S.fromList <$> filterM (fmap not . ignoreDependency) (namesIn defType ++ namesIn theDef)
  -- liftIO $ putStrLn $ "names in " <> prettyShow defName <> ": " <> prettyShow deps <> ": " <> prettyShow scope
  return ADDef { _name = defName, _deps = deps }

-- maybe change agda to allow more flexibility here?
compileDefAD :: Options -> ModuleEnv -> IsMain -> Definition -> TCM (Maybe ADDef)
compileDefAD opts _ _ def = do
  -- liftIO $ putStrLn $ prettyShow def
  if ignoreDef def then
    return Nothing
  else
    do addef <- computeDefAD def
       -- liftIO $ putStrLn (prettyShow addef ++ "\n")
       return $ Just addef
-- compileDefAD opts env NotMain def = return Nothing

postModuleAD :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName -> [Maybe ADDef] -> TCM [Maybe ADDef]
postModuleAD opts env _ tlmn defs = do
  -- let allNs = namesInScope env
  --     ds = catMaybes defs
  -- -- TODO: Refactor the code below
  -- let sc = map (\d -> allNs `S.difference` _deps d) ds
  -- case sc of
  --   [] -> return []
  --   (s:ss) -> do
  --     let definedNames = S.fromList $ map _name ds
  --         unusedNames = intersections (s :| ss) `S.difference` definedNames
  --     -- liftIO . putStrLn $ show $ S.map prettyShow allNs
  --     -- liftIO . putStrLn $ "[" <> prettyShow tlmn <> "]\nAll unused names in scope:"
  --     -- liftIO . putStrLn $ prettyShow unusedNames
      return defs

hashQName :: QName -> Int
hashQName = fromIntegral . hashString . show

-- ** constructing the graph

postCompileAD :: Options -> IsMain -> Map TopLevelModuleName [Maybe ADDef] -> TCM ()
postCompileAD opts _ defMap = do
  let defMap' :: Map TopLevelModuleName [ADDef]
      defMap' = fmap catMaybes defMap

      defs = concat . M.elems $ defMap'
  liftIO $ TL.putStrLn . printDotGraph . computeDepGraph $ defs
  where
    computeDepGraph :: [ADDef] -> DotGraph Node
    computeDepGraph defs =
      let
          mkNode' :: QName -> LNode QName
          mkNode' qn = (hashQName qn, qn)

          mkNodes' :: ADDef -> [LNode QName]
          mkNodes' ADDef{..} = mkNode' _name : map mkNode' (S.toList _deps)

          lnodeList :: [LNode QName]
          lnodeList = concatMap mkNodes' defs

          mkEdges' :: ADDef -> [UEdge]
          mkEdges' ADDef{..} = map (\dep -> (hashQName _name, hashQName dep, ())) $ S.toList _deps

          ledgeList = concatMap mkEdges' defs

          graphVizParams :: GraphvizParams Node QName () ModuleName QName
          graphVizParams = defaultParams
            {
              fmtNode = \(n, l) -> [toLabel $ prettyShow (qnameName l)],
              clusterBy = \(n , l) -> C (qnameModule l) (N (n , l)),
              clusterID = Str . TL.pack . prettyShow,
              fmtCluster = \mn -> [ GraphAttrs [toLabel $ prettyShow mn]]
            }

          depGraph :: Gr QName ()
          depGraph = mkGraph lnodeList ledgeList

      in graphToDot graphVizParams depGraph

-- ** things to ignore

ignoreDependency :: QName -> TCM Bool
ignoreDependency qn = do
  def <- getConstInfo qn
  return $ ignoreDef def

ignoreDef :: Definition -> Bool
ignoreDef Defn{..} = case theDef of

  -- ** ignore functions that are pattern-lambdas/with-generated/Kan operations
  Function{..} | isJust funExtLam || isJust funWith || isJust funIsKanOp -> True
  -- ** ignore inlined functions (arising from instantiated modules)
  d@Function{..} | d ^. funInline -> True

  -- ** Primitives: ignore *primitive* functions (no clauses), not *builtin* functions.
  Primitive{..} -> null primClauses

  -- ** CHANGEME, ad-hoc treatment of Level
  Axiom{} | prettyShow defName == "Agda.Primitive.Level" -> True

  -- ** just ignore these kinds of definitions
  PrimitiveSort{..} -> True
  DataOrRecSig{..} -> True
  GeneralizableVar _ -> True

  -- ** consider everything else
  _ -> False


-- ** utils
intersections :: Ord a => NonEmpty (Set a) -> Set a
intersections = foldr1 S.intersection
