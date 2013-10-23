module WOC.Gameplay.Units where

import Data.Vec
import qualified Data.Vector as V


data Unit = Unit { power :: Float,
                   resistance :: Float,
                   speed :: Float,
                   width :: Float,
                   height :: Float }

baseUnit :: Unit
baseUnit = Unit 1 1 1 1 1

