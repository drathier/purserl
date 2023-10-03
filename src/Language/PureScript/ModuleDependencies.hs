-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( DependencyDepth(..)
  , sortModules
  , ModuleGraph
  , ModuleSignature(..)
  , moduleSignature
  ) where

import Protolude hiding (head)

import Data.Array ((!))
import Data.Maybe (fromJust)
import Data.List qualified as List
import Data.Graph (SCC(..), graphFromEdges, reachable, stronglyConnComp, dfs, Vertex, edges)
import Data.Set qualified as S
import Language.PureScript.AST (Declaration(..), ErrorMessageHint(..), Module(..), SourceSpan)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), addHint, errorMessage', errorMessage'', parU)
import Language.PureScript.Names (ModuleName)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | A module signature for sorting dependencies.
data ModuleSignature = ModuleSignature
  { sigSourceSpan :: SourceSpan
  , sigModuleName :: ModuleName
  , sigImports :: [(ModuleName, SourceSpan)]
  }

data DependencyDepth = Direct | Transitive

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
sortModules
  :: forall m a
   . MonadError MultipleErrors m
  => DependencyDepth
  -> (a -> ModuleSignature)
  -> [a]
  -> m ([a], ModuleGraph)
sortModules dependencyDepth toSig ms = do
    let
      ms' = (\m -> (m, toSig m)) <$> ms
      mns = S.fromList $ map (sigModuleName . snd) ms'
    verts <- parU ms' (toGraphNode mns)
    ms'' <- parU (stronglyConnComp verts) (toModule verts)
    let (graph, fromVertex, toVertex) = graphFromEdges verts
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = case dependencyDepth of
                                         Direct -> graph ! v
                                         Transitive -> reachable graph v
                             toKey i = case fromVertex i of (_, key, _) -> key
                         return (mn, filter (/= mn) (map toKey deps))
    return (fst <$> ms'', moduleGraph)
  where
    toGraphNode :: S.Set ModuleName -> (a, ModuleSignature) -> m ((a, ModuleSignature), ModuleName, [ModuleName])
    toGraphNode mns m@(_, ModuleSignature _ mn deps) = do
      void . parU deps $ \(dep, pos) ->
        when (dep `notElem` C.primModules && S.notMember dep mns) .
          throwError
            . addHint (ErrorInModule mn)
            . errorMessage' pos
            $ ModuleNotFound dep
      pure (m, mn, map fst deps)

-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> Maybe (ModuleName, SourceSpan)
-- Regardless of whether an imported module is qualified we still need to
-- take into account its import to build an accurate list of dependencies.
usedModules (ImportDeclaration (ss, _) mn _ _) = pure (mn, ss)
usedModules _ = Nothing

-- | Convert a strongly connected component of the module graph to a module
toModule :: MonadError MultipleErrors m => [((a, ModuleSignature), ModuleName, [ModuleName])] -> SCC (a, ModuleSignature) -> m (a, ModuleSignature)
toModule _ (AcyclicSCC m) = return m
toModule verts (CyclicSCC ms) =
  case nonEmpty ms of
    Nothing ->
      internalError "toModule: empty CyclicSCC"
    Just ms'@(msRoot :| msRest) ->
      let shortestCycle n =
            let (graph, fromVertex, toVertex) = graphFromEdges verts in
            let msToVertex a = fromJust (toVertex (sigModuleName (snd a))) in
            let f z = sigModuleName <$> (\(x,_,_) -> snd x) $ fromVertex z in
            let (sccNodes :: S.Set Vertex) = S.fromList $ fromJust <$> toVertex <$> sigModuleName <$> snd <$> (msRoot : msRest) in
            let startState = Seq.fromList $ filter (\(_,_,x) -> S.member x sccNodes) $ NE.toList $ (\x -> (S.empty, [], msToVertex x)) <$> ms' in

            let firstNode = fromJust (toVertex (sigModuleName (snd n))) in
            let dfs :: S.Set Vertex -> [Vertex] -> [Vertex]
                dfs seen (a:ax) | S.member a sccNodes == False = dfs seen ax
                dfs seen (a:ax) | S.member a seen = [a]
                dfs seen (a:_) = a : dfs (S.insert a seen) (graph ! a)
            in

            let bfs :: Int -> Seq.Seq (S.Set Vertex, [Vertex], Vertex) -> [Vertex]
                bfs i _ | 1000000 < i =
                  let cycleWithTail@(cycleWithTailFirst:cycleWithTailRest) = reverse $ dfs S.empty [firstNode]
                  in cycleWithTailFirst : (reverse $ (cycleWithTailFirst : reverse (takeWhile (/= cycleWithTailFirst) cycleWithTailRest)))
                bfs i (rest Seq.:|> (aset,apath,a)) | S.member a aset = (a:apath)
                bfs i (rest Seq.:|> (aset,apath,a)) =
                  let outs = Seq.fromList $ (\x -> (S.insert a aset, a:apath, x)) <$> filter (\x -> S.member x sccNodes) (graph ! a) in
                  bfs (i + Seq.length outs) (outs <> rest)

                cycleWithTail = bfs 0 startState
            in
            NE.fromList $ map (\(x,_,_) -> x) $ map fromVertex $ reverse $
              case cycleWithTail of
                [a,b] | a == b -> [a]
                a -> a

          ms'' =
            case msRest of
              [] -> ms'
              [_] -> ms'
              _ ->
                shortestCycle msRoot
      in
      throwError
        . errorMessage'' (fmap (sigSourceSpan . snd) ms'')
        $ CycleInModules (map (sigModuleName . snd) ms'')

moduleSignature :: Module -> ModuleSignature
moduleSignature (Module ss _ mn ds _) = ModuleSignature ss mn (ordNub (mapMaybe usedModules ds))
