-- | This file defines workheet layout used externally.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Data.SheetLayout
(
 Coord,
 Address,
 row,
 col,
 makeAddr,
 maxRow,
 maxCol,
 showAddress
)

where

import Data.Function (on)
import qualified Data.Word as Word

-- -- | Type used for both row and column index.
type Coord = Word.Word32

-- | Type storing a sheet cell address. Format may change.
newtype Address = Addr { adr :: (Coord, Coord) }

-- | Max rows allowed in sheet. FIXME: Should be in Constants.
maxRow :: Coord
maxRow = 256

-- | Max columns in sheet. FIXME: Should be in Constants.
maxCol :: Coord
maxCol = 16

-- | Extract row from address.
row :: Address -> Coord
row (Addr (a, _)) = a

-- | Extract column of address.
col :: Address -> Coord
col (Addr (_, b)) = b

-- | Access row / col from string. While the internal representation of Address may change, address should not.
address :: Address -- ^ Input adress
        -> (Coord, Coord) -- ^ Row / Col
address (Addr a) = a

makeAddr :: Coord -> Coord -> Address
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

-- | Show address in serialisable format. This currently is the
-- same as the usual show output.
showAddress :: Address
            -> String
showAddress = show
