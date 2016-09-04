{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeFamilies, TypeInType,
             DataKinds, PolyKinds, UndecidableInstances, GADTs, RankNTypes #-}

module Graph where

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Enum



import Data.Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
            ViewL(..), ViewR(..), viewl, viewr)

$(singletons [d|
  data Tree a = Node {
          rootLabel :: a,         -- ^ label value
          subForest :: [Tree a]   -- ^ zero or more child trees
      }

  data SCC vertex = AcyclicSCC vertex     -- ^ A single vertex that is not
                                          -- in any cycle.
                  | CyclicSCC  [vertex]   -- ^ A maximal set of mutually
                                          -- reachable vertices.

  |])

$(singletons [d|

  drawTree :: Tree String -> String
  drawTree  = unlines . draw

  drawForest :: [Tree String] -> String
  drawForest  = unlines . map drawTree

  draw :: Tree String -> [String]
  draw (Node x ts0) = lines x ++ drawSubTrees ts0
    where
      drawSubTrees [] = []
      drawSubTrees [t] =
          "|" : shift "`- " "   " (draw t)
      drawSubTrees (t:ts) =
          "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

      shift first other = zipWith (++) (first : repeat other)

  flatten :: Tree a -> [a]
  flatten t = squish t []
    where squish (Node x ts) xs = x:Prelude.foldr squish xs ts

  levels :: Tree a -> [[a]]
  levels t =
      map (map rootLabel) $
          takeWhile (not . null) $
          iterate (concatMap subForest) [t]

  foldTree :: (a -> [b] -> b) -> Tree a -> b
  foldTree f = go where
      go (Node x ts) = f x (map go ts)

  unfoldTree :: (b -> (a, [b])) -> b -> Tree a
  unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

  unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
  unfoldForest f = map (unfoldTree f)


  flattenSCCs :: [SCC a] -> [a]
  flattenSCCs = concatMap flattenSCC

  flattenSCC :: SCC vertex -> [vertex]
  flattenSCC (AcyclicSCC v) = [v]
  flattenSCC (CyclicSCC vs) = vs

  stronglyConnComp
          :: Ord key
          => [(node, key, [key])]
                  -- ^ The graph: a list of nodes uniquely identified by keys,
                  -- with a list of keys of nodes this node has edges to.
                  -- The out-list may contain keys that don't correspond to
                  -- nodes of the graph; such edges are ignored.
          -> [SCC node]

  stronglyConnComp edges0
    = map get_node (stronglyConnCompR edges0)
    where
      get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
      get_node (CyclicSCC triples)     = CyclicSCC [n | (n,_,_) <- triples]

  stronglyConnCompR
          :: Ord key
          => [(node, key, [key])]
                  -- ^ The graph: a list of nodes uniquely identified by keys,
                  -- with a list of keys of nodes this node has edges to.
                  -- The out-list may contain keys that don't correspond to
                  -- nodes of the graph; such edges are ignored.
          -> [SCC (node, key, [key])]     -- ^ Topologically sorted

  stronglyConnCompR [] = []  -- added to avoid creating empty array in graphFromEdges -- SOF
  stronglyConnCompR edges0
    = map decode forest
    where
      (graph, vertex_fn,_) = graphFromEdges edges0
      forest             = scc graph
      decode (Node v []) | mentions_itself v = CyclicSCC [vertex_fn v]
                         | otherwise         = AcyclicSCC (vertex_fn v)
      decode other = CyclicSCC (dec other [])
                   where
                     dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
      mentions_itself v = v `elem` (graph ! v)


  vertices :: Array Int [Int] -> [Int]
  vertices  = indices

  edges    :: Array Int [Int] -> [(Int, Int)]
  edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

  mapT    :: (Int -> a -> b) -> Array Int a -> Array Int b
  mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]

  buildG :: (Int, Int) -> [(Int, Int)] -> Array Int [Int]
  buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 edges0

  transposeG  :: Array Int [Int] -> Array Int [Int]
  transposeG g = buildG (bounds g) (reverseE g)

  reverseE    :: Array Int [Int] -> [(Int, Int)]
  reverseE g   = [ (w, v) | (v, w) <- edges g ]

  outdegree :: Array Int [Int] -> Array Int Int
  outdegree  = mapT numEdges
               where numEdges _ ws = length ws

  indegree :: Array Int [Int] -> Array Int Int
  indegree  = outdegree . transposeG

  graphFromEdges'
          :: Ord key
          => [(node, key, [key])]
          -> (Array Int [Int], Int -> (node, key, [key]))
  graphFromEdges' x = (a,b) where
      (a,b,_) = graphFromEdges x

  graphFromEdges
          :: Ord key
          => [(node, key, [key])]
          -> (Array Int [Int], Int -> (node, key, [key]), key -> Maybe Int)
  graphFromEdges edges0
    = (graph, \v -> vertex_map ! v, key_vertex)
    where
      max_v           = length edges0 - 1
      bounds0         = (0,max_v)
      sorted_edges    = sortBy lt edges0
      edges1          = zipWith (,) [0..] sorted_edges

      graph           = array bounds0 [(,) v (mapMaybe key_vertex ks) | (,) v (_,    _, ks) <- edges1]
      key_map         = array bounds0 [(,) v k                       | (,) v (_,    k, _ ) <- edges1]
      vertex_map      = array bounds0 edges1

      (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2

      -- key_vertex :: key -> Maybe Int
      --  returns Nothing for non-interesting vertices
      key_vertex k   = findVertex 0 max_v
                     where
                       findVertex a b | a > b
                                = Nothing
                       findVertex a b = case compare k (key_map ! mid) of
                                     LT -> findVertex a (mid-1)
                                     EQ -> Just mid
                                     GT -> findVertex (mid+1) b
                                where
                                  mid = a + (b - a) `div` 2


  dff          :: Array Int [Int] -> [Tree Int]
  dff g         = dfs g (vertices g)

  dfs          :: Array Int [Int] -> [Int] -> [Tree Int]
  dfs g vs      = prune (bounds g) (map (generate g) vs)

  generate     :: Array Int [Int] -> Int -> Tree Int
  generate g v  = Node v (map (generate g) (g!v))

  prune        :: (Int, Int) -> [Tree Int] -> [Tree Int]
  prune bnds ts = run bnds (chop ts)

  chop         :: [Tree Int] -> SetM s [Tree Int]
  chop []       = return []
  chop (Node v ts : us)
                = do
                  visited <- contains v
                  if visited then
                    chop us
                   else do
                    include v
                    as <- chop ts
                    bs <- chop us
                    return (Node v as : bs)


  newtype SetM s a = SetM { runSetM :: IntSet -> (a, IntSet) }

  run          :: (Int, Int) -> SetM s a -> a
  run _ act     = fst (runSetM act Set.empty)

  contains     :: Int -> SetM s Bool
  contains v    = SetM $ \ m -> (Set.member v m, m)

  include      :: Int -> SetM s ()
  include v     = SetM $ \ m -> ((), Set.insert v m)



  preorder' :: Tree a -> [a] -> [a]
  preorder' (Node a ts) = (a :) . preorderF' ts

  preorderF' :: [Tree a] -> [a] -> [a]
  preorderF' ts = foldr (.) id $ map preorder' ts

  preorderF :: [Tree a] -> [a]
  preorderF ts = preorderF' ts []

  tabulate        :: (Int, Int) -> [Int] -> Array Int Int
  tabulate bnds vs = array bnds (zipWith (,) vs [1..])

  preArr          :: (Int, Int) -> [Tree Int] -> Array Int Int
  preArr bnds      = tabulate bnds . preorderF


  postorder :: Tree a -> [a] -> [a]
  postorder (Node a ts) = postorderF ts . (a :)

  postorderF   :: [Tree a] -> [a] -> [a]
  postorderF ts = foldr (.) id $ map postorder ts

  postOrd :: Array Int [Int] -> [Int]
  postOrd g = postorderF (dff g) []

  topSort      :: Array Int [Int] -> [Int]
  topSort       = reverse . postOrd


  components   :: Array Int [Int] -> [Tree Int]
  components    = dff . undirected

  undirected   :: Array Int [Int] -> Array Int [Int]
  undirected g  = buildG (bounds g) (edges g ++ reverseE g)


  scc  :: Array Int [Int] -> [Tree Int]
  scc g = dfs g (reverse (postOrd (transposeG g)))

  reachable    :: Array Int [Int] -> Int -> [Int]
  reachable g v = preorderF (dfs g [v])

  path         :: Array Int [Int] -> Int -> Int -> Bool
  path g v w    = w `elem` (reachable g v)


  bcc :: Array Int [Int] -> [Tree [Int]]
  bcc g = (concat . map bicomps . map (do_label g dnum)) forest
   where forest = dff g
         dnum   = preArr (bounds g) forest

  do_label :: Array Int [Int] -> Array Int Int -> Tree Int -> Tree (Int,Int,Int)
  do_label g dnum (Node v ts) = Node (v,dnum!v,lv) us
   where us = map (do_label g dnum) ts
         lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v]
                       ++ [lu | Node (_,_,lu) _ <- us])

  bicomps :: Tree (Int,Int,Int) -> [Tree [Int]]
  bicomps (Node (v,_,_) ts)
        = [ Node (v:vs) us | (_,Node vs us) <- map collect ts]

  collect :: Tree (Int,Int,Int) -> (Int, Tree [Int])
  collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
   where collected = map collect ts
         vs = concat [ ws | (lw, Node ws _) <- collected, lw<dv]
         cs = concat [ if lw<dv then us else [Node (v:ws) us]
                          | (lw, Node ws us) <- collected ]

   |])
