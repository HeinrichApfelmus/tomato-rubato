module Demonstrate where

-- | Type class to display values in a more fancy way than the simple @Show@ class allows.
class Demonstrate a where
    demo :: a -> IO ()


