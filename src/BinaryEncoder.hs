module BinaryEncoder where

import Data.Bits
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (mconcat)
import Data.Word

import Coordinate
import State


bit00011111 :: Word8
bit00011111 = 31

low5 :: Word8 -> Word8
low5 n = n .&. bit00011111

toWord :: Int -> Word8
toWord n = fromIntegral (n `mod` 256)

encodeLLD :: LLD -> (Word8,Word8)
encodeLLD (dx,0,0) = (1, low5 ((toWord dx)+15))
encodeLLD (0,dy,0) = (2, low5 ((toWord dy)+15))
encodeLLD (0,0,dz) = (3, low5 ((toWord dz)+15))

encodeSLD :: SLD -> (Word8,Word8)
encodeSLD (dx,0,0) = (1, low5 ((toWord dx)+5))
encodeSLD (0,dy,0) = (2, low5 ((toWord dy)+5))
encodeSLD (0,0,dz) = (3, low5 ((toWord dz)+5))

encodeND :: ND -> Word8
encodeND (dx,dy,dz) = ((toWord dx)+1)*9 + ((toWord dy)+1)*3 + ((toWord dz)+1)

encodeFD :: FD -> [Word8]
encodeFD (dx,dy,dz) =
  [ toWord (dx+30)
  , toWord (dy+30)
  , toWord (dz+30)
  ]

encode :: Command -> [Word8]
encode Halt = [255]

encode Wait = [254]

encode Flip = [253]

encode (SMove lld) =
  [ 0 .|. (shiftL da 4) .|. bit 2
  , di
  ]
  where
    (da,di) = encodeLLD lld

encode (LMove sld1 sld2) =
  [ 0 .|. (shiftL da2 6) .|. (shiftL da1 4) .|. (shift 3 2)
  , 0 .|. (shiftL di2 4) .|. di1
  ]
  where
    (da1,di1) = encodeSLD sld1
    (da2,di2) = encodeSLD sld2

encode (FusionP nd) =
  [ 0 .|. (shiftL d 3) .|. 7]
  where
    d = encodeND nd

encode (FusionS nd) =
  [ 0 .|. (shiftL d 3) .|. 6]
  where
    d = encodeND nd

encode (Fission nd m) =
  [ 0 .|. (shiftL d 3) .|. 5
  , toWord m
  ]
  where
    d = encodeND nd

encode (Fill nd) =
  [ 0 .|. (shiftL d 3) .|. 3]
  where
    d = encodeND nd

encode (Void nd) =
  [ 0 .|. (shiftL d 2) .|. 3]
  where
    d = encodeND nd

encode (GFill nd fd) =
  [0 .|. (shiftL d 1) .|. 3] ++ encodeFD fd
  where
    d = encodeND nd

encode (GVoid nd fd) =
  [0 .|. (shiftL d 0) .|. 3] ++ encodeFD fd
  where
    d = encodeND nd

encodeTrace :: Trace -> BL.ByteString
encodeTrace cs =
  BB.toLazyByteString (mconcat (map BB.word8 (concatMap encode cs)))

writeTraceFile :: FilePath -> Trace -> IO ()
writeTraceFile path cs =
  BL.writeFile path (encodeTrace cs)
