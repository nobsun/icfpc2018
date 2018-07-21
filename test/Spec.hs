import Test.Hspec
import Coordinate

coordinateSpec :: Spec
coordinateSpec = do
  describe "ld" $ do
    it "sld if mlen(ld) ≤ 5 then exactly 30 slds." $
      length (filter sld  [(x,y,z)|x<-[-5..5],y<-[-5..5],z<-[-5..5]]) `shouldBe` 30
      
    it "lld if mlen(ld) ≤ 15 then exactly 90 llds." $
      length (filter lld  [(x,y,z)|x<-[-15..15],y<-[-15..15],z<-[-15..15]]) `shouldBe` 90

    it "nd if mlen(d) <= 2 then exactly 18 nds." $
      length  [(x,y,z)|x<-[-2..2],y<-[-2..2],z<-[-2..2], 0 < mlen (x,y,z) && mlen (x,y,z) <= 2 && clen (x,y,z) == 1] `shouldBe` 18

main :: IO ()
main = hspec $ do
  coordinateSpec
