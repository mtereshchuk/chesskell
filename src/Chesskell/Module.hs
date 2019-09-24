module Chesskell.Module
  ( run
  ) where

import Data.Vector           (Vector)
import Data.Map.Strict       (Map)
import Graphics.Gloss hiding (Vector)
import Chesskell.Chess       (Piece)
import Chesskell.CoreCommons (Game)
import Chesskell.View        (chesskellDisplay, getStaticPic)

run :: IO ()
run = display chesskellDisplay white getStaticPic