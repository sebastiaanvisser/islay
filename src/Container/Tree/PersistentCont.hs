module Container.Tree.PersistentCont where

import Annotation.Persistent (P)
import Control.Applicative
import Data.Binary
import Data.Maybe
import Generics.Cont
import Generics.Regular.Base ()
import Generics.Fixpoint
import Heap.Heap hiding (size)
import Prelude hiding (lookup, null)
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Cont     as C

type Map k v = P (F.Tree k v) (FixA P (F.Tree k v))

empty :: (Binary k, Binary v) => HeapW (Map k v)
empty = mkAnnI C.empty

singleton :: (Binary k, Binary v) => k -> v -> HeapW (Map k v)
singleton k v = mkAnnI (C.singleton k v)

triplet :: (Binary k, Binary v) => k -> v -> k -> v -> k -> v -> HeapW (Map k v)
triplet a0 b0 a1 b1 a2 b2 = mkAnnI (C.triplet a0 b0 a1 b1 a2 b2)

lookup :: (Ord k, Binary k, Binary v) => k -> Map k v -> HeapR (Maybe v)
lookup k = mkAnnO (C.lookup k)

(!) :: (Ord k, Binary k, Binary a) => Map k a -> k -> HeapR a
(!) t k = fromMaybe (error "element not in the map") <$> lookup k t

member :: (Ord k, Binary k, Binary a) => k -> Map k a -> HeapR Bool
member k t = isJust <$> lookup k t

notMember :: (Ord k, Binary k, Binary a) => k -> Map k a -> HeapR Bool
notMember k t = not . isJust <$> lookup k t

size :: (Num c, Binary k, Binary v) => Map k v -> HeapR c
size = mkAnnO C.size

null :: (Binary k, Binary v) => Map k v -> HeapR Bool
null t = (== (0 :: Integer)) <$> size t

depth :: (Ord c, Num c, Binary k, Binary v) => Map k v -> HeapR c
depth = mkAnnO C.depth

alter :: (Ord k, Binary k, Binary v) => (Maybe v -> Maybe v) -> k -> Map k v -> HeapW (Map k v)
alter f k = mkAnnIO (C.alter f k)

insert :: (Ord k, Binary k, Binary v) => k -> v -> Map k v -> HeapW (Map k v)
insert k v = mkAnnIO (C.alter (const (Just v)) k)

delete :: (Ord k, Binary k, Binary v) => k -> Map k v -> HeapW (Map k v)
delete k = mkAnnIO (C.alter (const Nothing) k)

adjust :: (Ord k, Binary k, Binary v) => (v -> v) -> k -> Map k v -> HeapW (Map k v)
adjust f k = mkAnnIO (C.alter (fmap f) k)

