module Matrix where

import Coordinate

import Data.Bool (bool)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Matrix = IntMap (Set (Int,Int))

makeMatrix :: [Coord] -> Matrix
makeMatrix cs = IntMap.fromListWith Set.union [(y, Set.singleton (x,z)) | Coord (x,y,z) <- cs]

matrixCoords :: Matrix -> [Coord]
matrixCoords m = [Coord (x,y,z) | (y, xzs) <- IntMap.toList m, (x,z) <- Set.toList xzs]

isGrounded :: Matrix -> Bool
isGrounded m = go (IntMap.toAscList m) (const True)
  where
    go :: [(Int, Set (Int,Int))] -> ((Int,Int) -> Bool) -> Bool
    go [] _prev = True
    go ((_y,xzs) : ls) prev = and table && go ls (\xz -> Map.lookup xz table == Just True)
      where
        table :: Map (Int,Int) Bool
        table = converge f tbl0
          where
            tbl0 :: Map (Int,Int) Bool
            tbl0 = Map.fromList [(xz, prev xz) | xz@(x,z) <- Set.toAscList xzs]
            f :: Map (Int,Int) Bool -> Map (Int,Int) Bool
            f tbl = Map.mapWithKey (\(x,z) g -> g || or [Map.lookup (x',z') tbl == Just True | (x',z') <- [(x-1,z),(x+1,z),(x,z-1),(x,z+1)]]) tbl
        converge f x
          | fx == x = x
          | otherwise = converge f fx
          where
            fx = f x

data Voxel = Void | Full deriving (Show, Eq)

voxel :: Matrix -> Coord -> Voxel
voxel m (Coord (x,y,z)) = maybe Void (bool Void Full . Set.member (x, z)) $ IntMap.lookup y m

isFull :: Matrix -> Coord -> Bool
isFull m c = voxel m c == Full

isVoid :: Matrix -> Coord -> Bool
isVoid m c = voxel m c == Void
