import Test.Hspec

import Coordinate
import Matrix

smallCD :: [CDiff]
smallCoords :: [Coord]
(smallCD, smallCoords) = (small, map Coord small)
  where
    small =  [(x,y,z)|x<-[-5..5],y<-[-5..5],z<-[-5..5]]
largeCD :: [CDiff]
largeCD =  [(x,y,z)|x<-[-15..15],y<-[-15..15],z<-[-15..15]]

coordinateSpec :: Spec
coordinateSpec = do
  describe "ld and nd" $ do
    it "sld if mlen(ld) ≤ 5 then exactly 30 slds." $
      length (filter sld smallCD) `shouldBe` 30
      
    it "lld if mlen(ld) ≤ 15 then exactly 90 llds." $
      length (filter lld  largeCD) `shouldBe` 90

    it "nd if mlen(d) <= 2 then exactly 18 nds." $
      length [Coord (x,y,z)|x<-[-2..2],y<-[-2..2],z<-[-2..2], 0 < mlen (x,y,z) && mlen (x,y,z) <= 2 && clen (x,y,z) == 1] `shouldBe` 18

  describe "dim" $ do
    it "region ((1,1,1),(1,1,1)) is dimension 0." $
      dim (Coord (1,1,1), Coord (1,1,1)) `shouldBe` 0
    it "region ((1,1,1),(0,1,1)) is dimension 1." $
      dim (Coord (1,1,1), Coord (0,1,1)) `shouldBe` 1
    it "region ((1,0,1),(0,1,1)) is dimension 2." $
      dim (Coord (1,0,1), Coord (0,1,1)) `shouldBe` 2
    it "region ((1,0,1),(0,1,0)) is dimension 3." $
      dim (Coord (1,0,1), Coord (0,1,0)) `shouldBe` 3

  describe "shape" $ do
    it "region ((1,1,1),(1,1,1)) is Point." $
      shape (Coord (1,1,1), Coord (1,1,1)) `shouldBe` Point
    it "region ((0,1,1),(1,1,1)) is Line." $
      shape (Coord (0,1,1), Coord (1,1,1)) `shouldBe` Line
    it "region ((0,1,0),(1,1,1)) is Line." $
      shape (Coord (0,1,0), Coord (1,1,1)) `shouldBe` Plane
    it "region ((0,1,0),(1,0,1)) is Box." $
      shape (Coord (0,1,0), Coord (1,0,1)) `shouldBe` Box

matrixSpec :: Spec
matrixSpec = do
  let mx = makeMatrix $ map Coord [(0,0,0),(0,1,0),(0,0,2),(0,3,0),(1,1,2),(1,0,3)]
  describe "matrix" $ do
    it "voxel (0,1,0) is Full." $
      voxel mx (Coord (0,1,0)) `shouldBe` Full
    it "voxel (0,0,1) is Void." $
      voxel mx (Coord (0,0,1)) `shouldBe` Void
    it "Matrix is constructed by just only Full voxels." $
       makeMatrix (filter (isFull mx) smallCoords) `shouldBe` mx
  describe "isGrounded" $ do
    let pt0 = makeMatrix []
        pt1 = makeMatrix [Coord (10,0,20)]
        line0 = makeMatrix $ map Coord [(0,0,0),(1,0,0),(2,0,0),(3,0,0)]
        line1 = makeMatrix $ map Coord [(0,0,0),(0,1,0),(1,1,0),(2,1,0)]
        out0 = makeMatrix [Coord (0,1,0)]
    it "empty is grounded" $
      isGrounded pt0 `shouldBe` True
    it "point (10,0,20) is grounded" $
      isGrounded pt1 `shouldBe` True
    it "linear line is grounded" $
      isGrounded line0 `shouldBe` True
    it "zigzag line is grounded" $
      isGrounded line1 `shouldBe` True
    it "point (0,1,0) is NOT grounded" $
      isGrounded out0 `shouldBe` False

main :: IO ()
main = hspec $ do
  coordinateSpec
  matrixSpec
