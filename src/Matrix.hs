module Matrix where

import Coordinate

import Data.Bool (bool)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- isGrounded の実装で層別に処理できるかと思って、この表現にしたけれど、
-- それは出来なかったので、今の所、この表現にしている意味はない。
type Matrix = IntMap (Set (Int,Int))

makeMatrix :: [Coord] -> Matrix
makeMatrix cs = IntMap.fromListWith Set.union [(y, Set.singleton (x,z)) | Coord (x,y,z) <- cs]

matrixCoords :: Matrix -> [Coord]
matrixCoords m = [Coord (x,y,z) | (y, xzs) <- IntMap.toList m, (x,z) <- Set.toList xzs]

isGrounded :: Matrix -> Bool
isGrounded m =
  case converge f (g0,u0) of
    (g,u) -> Set.null u
  where
    g0 = Set.map (\(x,z) -> Coord (x,0,z)) $ IntMap.findWithDefault Set.empty 0 m  -- grounded   集合初期値: Coord Set of y == 0
    u0 = Set.fromList (matrixCoords m) Set.\\ g0                                   -- ungrounded 集合初期値: Coord Set of y /= 0

    f (g,u) = (g `Set.union` s1, u Set.\\ s1)
      where
        s0 = Set.fromList
             [ Coord c
             | Coord (x,y,z) <- Set.toList g
             , c <- [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
             -- Coord の要素に対して範囲 [0, R - 1] を見ていなくて危険に見えるが、
             -- 以下の intersection で、問題のない Coord のみが取り出されるので問題なし
             ]
        s1 = Set.intersection s0 u

    -- grounded 集合と ungrounded 集合のペアが変化しなくなるまで繰り返し
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
