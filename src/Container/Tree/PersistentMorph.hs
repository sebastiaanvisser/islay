module Container.Tree.PersistentMorph where

import Annotation.Persistent (P)
import Data.Binary
import Generics.Regular.Functions.Seq
import Generics.Fixpoint
import Heap.Heap
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Morph as M
import qualified Generics.Morphism.Apo as Apo
import qualified Generics.Morphism.Para as Para

type Map k v = FixA2 P (F.Tree k v)

insert :: (Ord k, Binary k, Binary v) => k -> v -> Map k v -> HeapW (Map k v)
insert k v = fmap out . Apo.endoMA (M.insert k v) . In

size :: (Num n, Binary k, Binary v, DeepSeq n) => Map k v -> HeapR n
size = Para.paraMA' M.size . In

depth :: (Ord n, Num n, Binary k, Binary v, DeepSeq n) => Map k v -> HeapR n
depth = Para.paraMA' M.depth . In

lookup :: (Show v, Ord k, Binary k, Binary v, DeepSeq v) => k -> Map k v -> HeapR (Maybe v)
lookup k = Para.paraMA' (M.lookup k) . In

