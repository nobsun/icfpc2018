{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Coordinate where

import Control.Applicative (pure, (*>), Alternative)
import Control.Monad (guard)
import Data.Bool (bool)
import Data.Function (on)
import Data.Tuple.Extra (fst3, snd3, thd3)

type R = Int

isValidR :: R -> Bool
isValidR = (&&) . (0 <) <*> (<= 250)

newtype Coord = Coord (Int, Int, Int) deriving (Eq, Ord, Show)
unCoord :: Coord -> (Int,Int,Int)
unCoord (Coord c) = c

type CDiff = (Int, Int, Int)
type LD = CDiff
type SLD = CDiff
type LLD = CDiff
type ND = CDiff

coord :: Alternative f
      => Int -> (Int, Int, Int) -> f Coord
coord r (x, y, z) =
  guard (0 <= x && x <= r - 1) *>
  guard (0 <= y && y <= r - 1) *>
  guard (0 <= z && z <= r - 1) *>
  pure (Coord (x, y, z))

add :: Coord -> CDiff -> Coord
add (Coord (x,y,z)) (dx,dy,dz) = Coord (x+dx,y+dy,z+dz)

sub :: Coord -> Coord -> CDiff
sub (Coord (x,y,z)) (Coord (x',y',z')) = (x-x',y-y',z-z')

-- Manhattan length (or L1 norm)
mlen :: CDiff -> Int
mlen (dx, dy, dz) = abs dx + abs dy + abs dz

-- Chessboard length (or Chebyshev distance or L∞ norm)
clen :: CDiff -> Int
clen (dx, dy, dz) = max (max (abs dx) (abs dy)) (abs dz)

adjacent :: Coord -> Coord -> Bool
adjacent c c' = mlen (sub c c') == 1

pAND :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
pAND p q x = p x && q x

-- predicate to check difference is `linear coordinate difference` (notated ld)
ld :: CDiff -> Bool
ld (dx,dy,dz) = length (filter (/= 0) [dx,dy,dz]) == 1

-- predicate to check difference is `short linear coordinate difference` (notated sld)
sld :: CDiff -> Bool
sld = ld `pAND` ((<= 5) . mlen)

-- predicate to check difference is `long linear coordinate difference` (notated lld)
lld :: CDiff -> Bool
lld = ld `pAND` ((<= 15) . mlen)

-- predicate to check difference is `near coordinate difference` (notated nd)
nd :: CDiff -> Bool
nd d = 0 < ml && ml <= 2 && cl == 1
  where
    ml = mlen d
    cl = clen d

-- | may not normalized region type
type Region_ = (Coord, Coord)

-- | normalized region type
newtype Region = Region (Coord, Coord) deriving (Eq, Ord, Show)

unRegion :: Region -> Region_
unRegion (Region p) = p

-- | make region with normalizing
region :: Coord -> Coord -> Region
region (Coord (x1,y1,z1)) (Coord (x2,y2,z2)) =
  Region (Coord (min x1 x2, min y1 y2, min z1 z2), Coord (max x1 x2, max y1 y2, max z1 z2))

memOfRegion :: Coord -> Region -> Bool
memOfRegion (Coord (x,y,z)) (Region (Coord (x1,y1,z1), Coord (x2,y2,z2)))
  = and [ x1 <= x && x <= x2
        , y1 <= y && x <= y2
        , z1 <= z && z <= z2
        ]

memOfRegion_ :: Coord -> Region_ -> Bool
memOfRegion_ p = memOfRegion p . uncurry region

normRegion_ :: Region_ -> Region_
normRegion_ = unRegion . uncurry region

membersOfRegion :: Region -> [Coord]
membersOfRegion (Region (Coord (x1,y1,z1), Coord (x2,y2,z2))) = do
  x <- [x1 .. x2]
  y <- [y1 .. y2]
  z <- [z1 .. z2]
  return $ Coord (x,y,z)

membersOfRegion_ :: Region_ -> [Coord]
membersOfRegion_ = membersOfRegion . uncurry region

type Dimension = Int

dim :: Region -> Dimension
dim (Region (Coord c1, Coord c2)) = dim' fst3 + dim' snd3 + dim' thd3
  where
    dim' acc = bool 1 0 $ ((==) `on` acc) c1 c2

data Shape = Point | Line | Plane | Box deriving (Show, Eq, Ord, Enum)

shape :: Region -> Shape
shape = toEnum . dim
