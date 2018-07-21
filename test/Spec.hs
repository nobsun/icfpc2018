import Test.Hspec

import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Word

import BinaryEncoder
import Coordinate
import Matrix
import State

makeMatrix' :: [(Int, Int, Int)] -> Matrix
makeMatrix' = makeMatrix . map Coord

small =  [(x,y,z)|x<-[-5..5],y<-[-5..5],z<-[-5..5]]
large =  [(x,y,z)|x<-[-15..15],y<-[-15..15],z<-[-15..15]]

coordinateSpec :: Spec
coordinateSpec = do
  describe "ld and nd" $ do
    it "sld if mlen(ld) ≤ 5 then exactly 30 slds." $
      length (filter sld small) `shouldBe` 30

    it "lld if mlen(ld) ≤ 15 then exactly 90 llds." $
      length (filter lld  large) `shouldBe` 90

    it "nd if mlen(d) <= 2 then exactly 18 nds." $
      length [Coord (x,y,z)|x<-[-2..2],y<-[-2..2],z<-[-2..2], 0 < mlen (x,y,z) && mlen (x,y,z) <= 2 && clen (x,y,z) == 1] `shouldBe` 18

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
    it "voxel (0,0,1) is Void." $
      voxel mx (Coord (0,0,1)) `shouldBe` Void
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


showWord :: Word8 -> String
showWord w =
  [if testBit w i then '1' else '0' | i<-[7,6..0]]

binaryEncoderSpec :: Spec
binaryEncoderSpec = do
  describe "Halt" $ do
    it "Halt is encoded as [11111111]8." $
      unwords (map showWord (encode Halt)) `shouldBe` "11111111"

  describe "Wait" $ do
    it "Wait is encoded as [11111110]8." $
      unwords (map showWord (encode Wait)) `shouldBe` "11111110"

  describe "Flip" $ do
    it "Flip is encoded as [11111101]8." $
      unwords (map showWord (encode Flip)) `shouldBe` "11111101"

  describe "SMove" $ do
    it "SMove <12,0,0> is encoded as [00010100] [00011011]." $
      unwords (map showWord (encode (SMove (12,0,0)))) `shouldBe` "00010100 00011011"

    it "SMove <0,0,-4> is encoded as [00110100] [00001011]." $
      unwords (map showWord (encode (SMove (0,0,-4)))) `shouldBe` "00110100 00001011"

  describe "LMove" $ do
    it "LMove <3,0,0> <0,-5,0> is encoded as [10011100] [00001000]." $
      unwords (map showWord (encode (LMove (3,0,0) (0,-5,0)))) `shouldBe` "10011100 00001000"

    it "LMove <0,-2,0> <0,0,2> is encoded to [11101100] [01110011]." $
      unwords (map showWord (encode (LMove (0,-2,0) (0,0,2)))) `shouldBe` "11101100 01110011"

  describe "FusionP" $ do
    it "FusionP <-1,1,0> is encoded as [00111111]." $
      unwords (map showWord (encode (FusionP (-1,1,0)))) `shouldBe` "00111111"

  describe "FusionS" $ do
    it "FusionS <1,-1,0> is encoded as [10011110]." $
      unwords (map showWord (encode (FusionS (1,-1,0)))) `shouldBe` "10011110"

  describe "Fission" $ do
    it "Fission <0,0,1> 5 is encoded as [01110101] [00000101]" $
      unwords (map showWord (encode (Fission (0,0,1) 5))) `shouldBe` "01110101 00000101"

  describe "Fill" $ do
    it "Fill <0,-1,0> is encoded as [01010011]" $
      unwords (map showWord (encode (Fill (0,-1,0)))) `shouldBe` "01010011"


traceFileSpec :: Spec
traceFileSpec = do
  describe "Trace Files" $ do
    it "Halt" $
      unwords (map showWord (BL.unpack (encodeTrace [Halt]))) `shouldBe` "11111111"

    it "Wait Halt" $
      unwords (map showWord (BL.unpack (encodeTrace [Wait,Halt]))) `shouldBe` "11111110 11111111"

    it "Flip Flip Wait Halt" $
      unwords (map showWord (BL.unpack (encodeTrace [Flip,Flip,Wait,Halt]))) `shouldBe` "11111101 11111101 11111110 11111111"


main :: IO ()
main = hspec $ do
  coordinateSpec
  matrixSpec
  binaryEncoderSpec
  traceFileSpec
