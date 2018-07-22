{-# LANGUAGE BinaryLiterals #-}

import Test.Hspec

import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Word

import Coordinate
import Matrix
import State
import TraceDecoder
import TraceEncoder

makeMatrix' :: [(Int, Int, Int)] -> Matrix
makeMatrix' = makeMatrix . map Coord

small =  [(x,y,z)|x<-[-5..5],y<-[-5..5],z<-[-5..5]]
large =  [(x,y,z)|x<-[-15..15],y<-[-15..15],z<-[-15..15]]

coordinateSpec :: Spec
coordinateSpec = do
  describe "ld/nd/fd" $ do
    it "sld if mlen(ld) ≤ 5 then exactly 30 slds." $
      length (filter sld small) `shouldBe` 30

    it "lld if mlen(ld) ≤ 15 then exactly 90 llds." $
      length (filter lld  large) `shouldBe` 90

    it "nd if mlen(d) ≤ 2 then exactly 18 nds." $
      length [Coord (x,y,z)|x<-[-2..2],y<-[-2..2],z<-[-2..2], 0 < mlen (x,y,z) && mlen (x,y,z) <= 2 && clen (x,y,z) == 1] `shouldBe` 18

    it "fd if clen(d) ≤ 30 then exactly 226980 fds." $
      length [Coord (x,y,z)|x<-[-30..30],y<-[-30..30],z<-[-30..30], 0 < clen (x,y,z) && clen (x,y,z) <= 30] `shouldBe` 226980

  describe "dim" $ do
    it "region ((1,1,1),(1,1,1)) is dimension 0." $
      dim (region (Coord (1,1,1)) (Coord (1,1,1))) `shouldBe` 0
    it "region ((1,1,1),(0,1,1)) is dimension 1." $
      dim (region (Coord (1,1,1)) (Coord (0,1,1))) `shouldBe` 1
    it "region ((1,0,1),(0,1,1)) is dimension 2." $
      dim (region (Coord (1,0,1)) (Coord (0,1,1))) `shouldBe` 2
    it "region ((1,0,1),(0,1,0)) is dimension 3." $
      dim (region (Coord (1,0,1)) (Coord (0,1,0))) `shouldBe` 3

  describe "shape" $ do
    it "region ((1,1,1),(1,1,1)) is Point." $
      shape (region (Coord (1,1,1)) (Coord (1,1,1))) `shouldBe` Point
    it "region ((0,1,1),(1,1,1)) is Line." $
      shape (region (Coord (0,1,1)) (Coord (1,1,1))) `shouldBe` Line
    it "region ((0,1,0),(1,1,1)) is Line." $
      shape (region (Coord (0,1,0)) (Coord (1,1,1))) `shouldBe` Plane
    it "region ((0,1,0),(1,0,1)) is Box." $
      shape (region (Coord (0,1,0)) (Coord (1,0,1))) `shouldBe` Box

matrixSpec :: Spec
matrixSpec = do
  let mx = makeMatrix' [(0,0,0),(0,1,0),(0,0,2),(0,3,0),(1,1,2),(1,0,3)]
  describe "matrix" $ do
    it "voxel (0,1,0) is Full." $
      voxel mx (Coord (0,1,0)) `shouldBe` Full
    it "voxel (0,0,1) is Empty." $
      voxel mx (Coord (0,0,1)) `shouldBe` Empty
    it "Matrix is constructed by just only Full voxels." $
       makeMatrix (filter (isFull mx) $ map Coord small) `shouldBe` mx
  describe "isGrounded" $ do
    let pt0 = makeMatrix' []
        pt1 = makeMatrix' [(10,0,20)]
        pt2 = makeMatrix' [(1,0,2), (3,0,4)]
        line0 = makeMatrix' [(0,0,0),(1,0,0),(2,0,0),(3,0,0)]
        line1 = makeMatrix' [(0,0,0),(0,1,0),(1,1,0),(2,1,0)]
        line2 = makeMatrix' [(0,0,0),(0,1,0),(0,2,0),(0,2,1),(0,2,2),(0,1,2)]
        line3 = makeMatrix' [(0,0,0),(0,1,0),(0,2,0),(0,2,1),(0,2,2),(0,1,2),(0,0,2)]
        out0 = makeMatrix' [(0,1,0)]
        out1 = makeMatrix' [(1,1,1)]
        out2 = makeMatrix' [(10,0,20),(1,1,1)]
        out3 = makeMatrix' [(0,1,0),(0,2,0),(1,2,0),(2,2,0)]
    it "empty is grounded." $
      isGrounded pt0 `shouldBe` True
    it "point (10,0,20) is grounded." $
      isGrounded pt1 `shouldBe` True
    it "2 splitted points (1,0,2) and (3,0,4) is grounded." $
      isGrounded pt2 `shouldBe` True
    it "linear line is grounded." $
      isGrounded line0 `shouldBe` True
    it "zigzag line is grounded." $
      isGrounded line1 `shouldBe` True
    it "gamma line is grounded." $
      isGrounded line2 `shouldBe` True
    it "bridge line is grounded." $
      isGrounded line3 `shouldBe` True
    it "point (0,1,0) is NOT grounded." $
      isGrounded out0 `shouldBe` False
    it "point (1,1,1) is NOT grounded." $
      isGrounded out1 `shouldBe` False
    it "one point is grounded but the another point is NOT grounded." $
      isGrounded out2 `shouldBe` False
    it "grounded line lifted is NOT grounded." $
      isGrounded out3 `shouldBe` False

  describe "fill/void" $ do
    let m = makeMatrix' [(0,0,0),(0,1,0),(1,0,0)]
    it "fill (0,0,0) make no change" $
      fill (Coord (0,0,0)) m `shouldBe` m
    it "fill (0,0,1) add new set" $
      fill (Coord (0,0,1)) m `shouldBe` IntMap.fromList [(0, Set.fromList [(0,0),(0,1),(1,0)]),(1, Set.fromList [(0,0)])]
    it "fill (1,2,1) add new map key" $
      fill (Coord (1,2,1)) m `shouldBe` IntMap.fromList [(0, Set.fromList [(0,0),(1,0)]),(1, Set.fromList [(0,0)]),(2, Set.fromList [(1,1)])]
    it "void (0,0,0)" $
      void (Coord (0,0,0)) m `shouldBe` IntMap.fromList [(0, Set.fromList [(1,0)]), (1, Set.fromList [(0,0)])]
    it "void (0,1,0)" $
      void (Coord (0,1,0)) m `shouldBe` IntMap.fromList [(0, Set.fromList [(0,0),(1,0)])]

showWord :: Word8 -> String
showWord w =
  [if testBit w i then '1' else '0' | i<-[7,6..0]]

traceEncoderSpec :: Spec
traceEncoderSpec = do
  describe "Halt" $ do
    it "Halt is encoded as [11111111]8." $
      unwords (map showWord (encodeCommand' Halt)) `shouldBe` "11111111"

  describe "Wait" $ do
    it "Wait is encoded as [11111110]8." $
      unwords (map showWord (encodeCommand' Wait)) `shouldBe` "11111110"

  describe "Flip" $ do
    it "Flip is encoded as [11111101]8." $
      unwords (map showWord (encodeCommand' Flip)) `shouldBe` "11111101"

  describe "SMove" $ do
    it "SMove <12,0,0> is encoded as [00010100] [00011011]." $
      unwords (map showWord (encodeCommand' (SMove (12,0,0)))) `shouldBe` "00010100 00011011"

    it "SMove <0,0,-4> is encoded as [00110100] [00001011]." $
      unwords (map showWord (encodeCommand' (SMove (0,0,-4)))) `shouldBe` "00110100 00001011"

  describe "LMove" $ do
    it "LMove <3,0,0> <0,-5,0> is encoded as [10011100] [00001000]." $
      unwords (map showWord (encodeCommand' (LMove (3,0,0) (0,-5,0)))) `shouldBe` "10011100 00001000"

    it "LMove <0,-2,0> <0,0,2> is encoded as [11101100] [01110011]." $
      unwords (map showWord (encodeCommand' (LMove (0,-2,0) (0,0,2)))) `shouldBe` "11101100 01110011"

  describe "FusionP" $ do
    it "FusionP <-1,1,0> is encoded as [00111111]." $
      unwords (map showWord (encodeCommand' (FusionP (-1,1,0)))) `shouldBe` "00111111"

  describe "FusionS" $ do
    it "FusionS <1,-1,0> is encoded as [10011110]." $
      unwords (map showWord (encodeCommand' (FusionS (1,-1,0)))) `shouldBe` "10011110"

  describe "Fission" $ do
    it "Fission <0,0,1> 5 is encoded as [01110101] [00000101]" $
      unwords (map showWord (encodeCommand' (Fission (0,0,1) 5))) `shouldBe` "01110101 00000101"

  describe "Fill" $ do
    it "Fill <0,-1,0> is encoded as [01010011]" $
      unwords (map showWord (encodeCommand' (Fill (0,-1,0)))) `shouldBe` "01010011"

  describe "Void" $ do
    it "Void <1,0,1> is encoded as [10111010]" $
      unwords (map showWord (encodeCommand' (Void (1,0,1)))) `shouldBe` "10111010"

  describe "GFill" $ do
    it "GFill <0,-1,0> <10,-15,20> is encoded as [01010001] [00101000] [00001111] [00110010]" $
      unwords (map showWord (encodeCommand' (GFill (0,-1,0) (10,-15,20)))) `shouldBe` "01010001 00101000 00001111 00110010"

  describe "GVoid" $ do
    it "GVoid <1,0,0> <5,5,-5> is encoded as [10110000] [00100011] [00100011] [00011001]" $
      unwords (map showWord (encodeCommand' (GVoid (1,0,0) (5,5,-5)))) `shouldBe` "10110000 00100011 00100011 00011001"


traceFileSpec :: Spec
traceFileSpec = do
  describe "Trace Files" $ do
    it "Halt" $
      unwords (map showWord (BL.unpack (encodeTrace [Halt]))) `shouldBe` "11111111"

    it "Wait Halt" $
      unwords (map showWord (BL.unpack (encodeTrace [Wait,Halt]))) `shouldBe` "11111110 11111111"

    it "Flip Flip Wait Halt" $
      unwords (map showWord (BL.unpack (encodeTrace [Flip,Flip,Wait,Halt]))) `shouldBe` "11111101 11111101 11111110 11111111"

traceDecoderSpec :: Spec
traceDecoderSpec = do
  describe "Halt" $ do
    it "Halt is encoded as [11111111]8." $
      decodeTrace (BL.pack [0b11111111]) `shouldBe` Right [Halt]

  describe "Wait" $ do
    it "Wait is encoded as [11111110]8." $
      decodeTrace (BL.pack [0b11111110]) `shouldBe` Right [Wait]

  describe "Flip" $ do
    it "Flip is encoded as [11111101]8." $
      decodeTrace (BL.pack [0b11111101]) `shouldBe` Right [Flip]

  describe "SMove" $ do
    it "SMove <12,0,0> is encoded as [00010100] [00011011]." $
      decodeTrace (BL.pack [0b00010100, 0b00011011]) `shouldBe` Right [SMove (12,0,0)]

    it "SMove <0,0,-4> is encoded as [00110100] [00001011]." $
      decodeTrace (BL.pack [0b00110100, 0b00001011]) `shouldBe` Right [SMove (0,0,-4)]

  describe "LMove" $ do
    it "LMove <3,0,0> <0,-5,0> is encoded as [10011100] [00001000]." $
      decodeTrace (BL.pack [0b10011100, 0b00001000]) `shouldBe` Right [LMove (3,0,0) (0,-5,0)]

    it "LMove <0,-2,0> <0,0,2> is encoded as [11101100] [01110011]." $
      decodeTrace (BL.pack [0b11101100, 0b01110011]) `shouldBe` Right [LMove (0,-2,0) (0,0,2)]
             
  describe "FusionP" $ do
    it "FusionP <-1,1,0> is encoded as [00111111]." $
      decodeTrace (BL.pack [0b00111111]) `shouldBe` Right [FusionP (-1,1,0)]

  describe "FusionS" $ do
    it "FusionS <1,-1,0> is encoded as [10011110]." $
      decodeTrace (BL.pack [0b10011110]) `shouldBe` Right [FusionS (1,-1,0)]

  describe "Fission" $ do
    it "Fission <0,0,1> 5 is encoded as [01110101] [00000101]" $
      decodeTrace (BL.pack [0b01110101, 0b00000101]) `shouldBe` Right [Fission (0,0,1) 5]

  describe "Fill" $ do
    it "Fill <0,-1,0> is encoded as [01010011]" $
      decodeTrace (BL.pack [0b01010011]) `shouldBe` Right [Fill (0,-1,0)]

  describe "Void" $ do
    it "Void <1,0,1> is encoded as [10111010]" $
      decodeTrace (BL.pack [0b10111010]) `shouldBe` Right [Void (1,0,1)]

  describe "GFill" $ do
    it "GFill <0,-1,0> <10,-15,20> is encoded as [01010001] [00101000] [00001111] [00110010]." $
      decodeTrace (BL.pack [0b01010001, 0b00101000, 0b00001111, 0b00110010]) `shouldBe` Right [GFill (0,-1,0) (10,-15,20)]

  describe "GVoid" $ do
    it "GVoid <1,0,0> <5,5,-5> is encoded as [10110000] [00100011] [00100011] [00011001]." $
      decodeTrace (BL.pack [0b10110000, 0b00100011, 0b00100011, 0b00011001]) `shouldBe` Right [GVoid (1,0,0) (5,5,-5)]

main :: IO ()
main = hspec $ do
  coordinateSpec
  matrixSpec
  traceEncoderSpec
  traceFileSpec
  traceDecoderSpec
