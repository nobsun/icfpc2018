{-# OPTIONS -Wall #-}
{-# LANGUAGE BinaryLiterals #-}

module TraceDecoder where

import Control.Applicative (pure)
import Data.Word (Word8)
import Data.Serialize.Get (Get)

import Coordinate (SLD, LLD, ND)

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
nd = undefined
