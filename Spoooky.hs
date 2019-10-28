module Spoooky where
{-# Language FlexibleInstances #-}
class Eq a => Spoooky a where
  alive :: a
  ded :: a
  boo :: a -> Bool
  crossroads :: a -> b -> b -> b
  boo ghost = ghost == alive
  crossroads ghost deal noDeal = 
        if boo ghost then deal else noDeal


reaper :: Spoooky a => [a] -> [a]
reaper [] = []
reaper (x:xs) = crossroads x (x:reaper xs) (reaper xs)



instance Spoooky Bool where
  alive = True
  ded = False

instance Spoooky Integer where
  alive = 4
  ded = 13
  boo x = x `notElem` [13, 31, 666, 7]

{-
instance Spoooky [Char] where
  boo x = x == "spooky"
-}
 
instance Spoooky a => Spoooky (Maybe a) where
  alive = Just alive
  ded = Nothing
  boo (Just x) = boo x
  boo Nothing = False

{-instance Spoooky (Maybe a) where
  boo (Just x) = True
  boo Nothing = False
 -} 
instance Spoooky a => Spoooky [a] where
  alive = [alive]
  ded = []
  boo [] = False
  boo lst  = all boo lst 

