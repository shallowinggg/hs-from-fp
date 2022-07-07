module AlgebraicDataTypes.Quad where

-- Exercises: The Quad

-- 1.
data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- how many different forms can this take?
-- 4 * 4 = 16
eQuad :: Either Quad Quad
eQuad = undefined

-- 2.
-- 4 * 4 = 16
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- 3.
-- 4 ^ 4 = 256
funcQuad :: Quad -> Quad
funcQuad = undefined

-- 4.
-- 2 * 2 * 2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- 5.
-- 2 ^ (2 * 2) = 16
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- 6.
-- 4 ^ (2 * 4) = 65536
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
