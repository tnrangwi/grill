-- | This file defines workheet layout used externally.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Data.SheetLayout
(
 Address,
 row,
 col
)

where

newtype Address = Addr (Int, Int)

row :: Address -> Int
row (Addr (a, _)) = a

col :: Address -> Int
col (Addr (_, b)) = b

address :: Address -> (Int, Int)
address (Addr a) = a

makeAddr :: Int -> Int -> Address
makeAddr a b = Addr (a, b)