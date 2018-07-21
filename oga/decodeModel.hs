import Data.Bits
import qualified Data.ByteString as B
import Data.Word

-- decoding Model files
main = do
  m <- readModel "LA011_tgt.mdl"
  print (isFull m (7,15,7))


data Model = Model Int B.ByteString

readModel :: FilePath -> IO Model
readModel path = do
  str <- B.readFile path
  let (r,bs) = (B.head str, B.tail str)
  return (Model (fromIntegral r) bs)


isFull :: Model -> (Int,Int,Int) -> Bool
isFull m@(Model r bs) ix@(x,y,z) =
  isInbound m ix && testBit (B.index bs iq) ir
  where
    iq, ir :: Int
    (iq,ir) = (x*r*r + y*r + z)`divMod`8

isInbound :: Model -> (Int,Int,Int) -> Bool
isInbound (Model r bs) (x,y,z)
  =  0<=x && x<r && 0<=y && y<r && 0<=z && z<r
