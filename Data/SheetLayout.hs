-- | This file defines workheet layout used externally.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Data.SheetLayout
(
 Address,
 row,
 col,
 makeAddr,
 maxRow,
 maxCol
)

where

import Data.Function (on)

-- | Type storing a sheet cell address. Format may change.
newtype Address = Addr { adr :: (Int, Int) }

-- | Max rows allowed in sheet. FIXME: Should be in Constants.
maxRow :: Int
maxRow = 256

-- | Max columns in sheet. FIXME: Should be in Constants.
maxCol :: Int
maxCol = 16

-- | Extract row from address.
row :: Address -> Int
row (Addr (a, _)) = a

-- | Extract column of address.
col :: Address -> Int
col (Addr (_, b)) = b

-- | Access row / col from string. While the internal representation of Address may change, address should not.
address :: Address -- ^ Input adress
        -> (Int, Int) -- ^ Row / Col
address (Addr a) = a

makeAddr :: Int -> Int -> Address
makeAddr a b = Addr (a, b)

instance Show Address
    where
      show (Addr (r, c)) = show r ++ ":" ++ show c

instance Ord Address
    where
      --(<=) a b = (<=) (address a) (address b)
      (<=) = (<=) `on` address
      max a b = Addr $ max (address a) (address b)


instance Eq Address
    where
      (==) = (==) `on` address
