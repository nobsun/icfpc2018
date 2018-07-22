{-# OPTIONS -Wall #-}
{-# LANGUAGE BinaryLiterals #-}

module TraceDecoder where

import Control.Applicative (pure, many)
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Serialize.Get (Get, getWord8, runGet)

import Coordinate (SLD, LLD, ND, FD)
import State (Command (..), Trace)

sld :: Word8 -> Word8 -> Get SLD
sld a i
  | a == 0b01  =  pure $ (fromIntegral i - 5, 0, 0)
  | a == 0b10  =  pure $ (0, fromIntegral i - 5, 0)
  | a == 0b11  =  pure $ (0, 0, fromIntegral i - 5)
  | otherwise  =  fail $ "sld: a = " ++ show a

lld :: Word8 -> Word8 -> Get LLD
lld a i
  | a == 0b01  =  pure $ (fromIntegral i - 15, 0, 0)
  | a == 0b10  =  pure $ (0, fromIntegral i - 15, 0)
  | a == 0b11  =  pure $ (0, 0, fromIntegral i - 15)
  | otherwise  =  fail $ "lld: a = " ++ show a

nd :: Word8 -> Get ND
nd w = pure (fromIntegral dx' - 1, fromIntegral dy' - 1, fromIntegral dz' - 1)
  where
    (dx',tmp) = w `divMod` 9
    (dy',dz') = tmp `divMod` 3

fd :: (Word8, Word8, Word8) -> Get FD
fd (dx,dy,dz) = pure (fromIntegral dx - 30, fromIntegral dy -30, fromIntegral dz - 30)

command :: Get Command
command = do
  w <- getWord8
  case w of
    0b11111111 -> return Halt
    0b11111110 -> return Wait
    0b11111101 -> return Flip
    _ | w .&. 0b1111 == 0b0100 -> do
          let a = (0b00110000 .&. w) `shiftR` 4
          i <- getWord8
          liftM SMove $ lld a i
      | w .&. 0b1111 == 0b1100 -> do
          let a2 = (0b11000000 .&. w) `shiftR` 6
              a1 = (0b00110000 .&. w) `shiftR` 4
          w2 <- getWord8
          let i2 = (0b11110000 .&. w2) `shiftR` 4
              i1 = 0b00001111 .&. w2
          sld2 <- sld a2 i2
          sld1 <- sld a1 i1
          return $ LMove sld1 sld2
      | w .&. 0b111 == 0b111 -> do
          nd' <- nd $ w `shiftR` 3
          return $ FusionP nd'
      | w .&. 0b111 == 0b110 -> do
          nd' <- nd $ w `shiftR` 3
          return $ FusionS nd'
      | w .&. 0b111 == 0b101 -> do
          nd' <- nd $ w `shiftR` 3
          m <- getWord8
          return $ Fission nd' (fromIntegral m)
      | w .&. 0b111 == 0b011 -> do
          nd' <- nd $ w `shiftR` 3
          return $ Fill nd'
      | w .&. 0b111 == 0b010 -> do
          nd' <- nd $ w `shiftR` 3
          return $ Void nd'
      | w .&. 0b111 == 0b001 -> do
          nd' <- nd $ w `shiftR` 3
          dx <- getWord8
          dy <- getWord8
          dz <- getWord8
          fd' <- fd $ (dx,dy,dz)
          return $ GFill nd' fd'

trace :: Get Trace
trace = many command


readTraceFile :: FilePath -> IO Trace
readTraceFile fname = do
  s <- BS.readFile fname
  case runGet trace s of
    Left err -> error err
    Right tr -> return tr
