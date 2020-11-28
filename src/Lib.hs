module Lib
    ( area
    ) where

import qualified Streamly.Prelude as S

area :: Float -> Float
area r = pi * (r * r)
