module Trumfu where

import Pal

data Trumfu = Butifarra | Pal Pal deriving (Eq)

instance Show Trumfu where
  show (Butifarra) = "Butifarra"
  show (Pal pal)   = show pal
