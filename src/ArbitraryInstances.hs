module ArbitraryInstances where

instance (Enum a, Bounded a, Show a, Show b) => Show (a -> b) where
  show f = show $ fmap (\a -> (a, f a)) [minBound, maxBound]

instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
  fa == fb = all (uncurry (==)) $ fmap (\x -> (fa x, fb x)) [minBound, maxBound]
