{-# LANGUAGE DeriveFunctor #-}
module WOC.Data.Collections where


data QuadTree a = QTNode { qtNodeValue :: a
                         , qtTopLeft :: QuadTree a
                         , qtTopRight :: QuadTree a
                         , qtBtmLeft :: QuadTree a
                         , qtBtmRight :: QuadTree a }
  deriving (Functor)
              
