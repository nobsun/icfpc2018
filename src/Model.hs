{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Model where

import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy as BL

import Coordinate
import Matrix

data Model = Model
  { mdResolution :: !Int
  , mdMatrix     :: Matrix
  }
  deriving (Eq,Ord,Show)

parseModel :: BL.ByteString -> Model
parseModel s
  | BL.length body /= fromIntegral n = error "invalid length"
  | otherwise = Model (fromIntegral r) mat
  where
    r' = BL.head s
    r, r3, n :: Int
    r = fromIntegral r'
    r3 = fromIntegral r ^ (3::Int)
    n = (r3 + 7) `div` 8
    body = BL.tail s
    mat = makeMatrix $ do
      (i,w) <- zip [0..] (BL.unpack body)
      j <- [0..7]
      let b = testBit w j
          xyz = i*8 + j
          (x,yz) = xyz `divMod` (r*r)
          (y,z) = yz `divMod` r
      guard $ xyz < r3
      guard b
      return $ Coord (x,y,z)

readModel :: FilePath -> IO Model
readModel fpath = liftM parseModel $ BL.readFile fpath

-- | empty model with same resolution
emptyR :: Model -> Model
emptyR m = m { mdMatrix = makeMatrix [] }
