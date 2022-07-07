module TypeClass.Exercises where

-- Multiple choice
-- 1. C
-- 2. B
-- 3. A
-- 4. C
-- 5. A

-- Does it typecheck?
-- 1. No
-- data Person = Person Bool deriving (Show)
-- 2. No
-- 3.
-- 4. Yes

-- Given a datatype declaration, what can we do?
-- 1. No
-- phew = Papu (Rocks "chases") (Yeah True)
-- 2. Yes
-- 3. Yes
-- 4. No
-- Papu don't implement Ord

-- Match the types
-- 1. No
-- 2. No
-- 3. Yes
-- 4. Yes
-- 5. Yes
-- 6. Yes
-- 7. No
-- No, the type of myX infects the return type of sigmund. a is too general.
-- 8. No
-- 9. Yes
-- 10. Yes
-- 11. No

-- Type-Kwon-Do Two: Electric Typealoo
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromIntegral i