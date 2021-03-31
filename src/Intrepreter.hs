module Intrepreter where

-- Zipper based Brainfuck intrepreter 
--
-- lefts are stored in opposite order
data Tape a = Tape {    lefts  :: [a]
                      , focus  :: a
                      , rights :: [a]
                   } 
 deriving Show

-- >
moveLeft :: Tape Int -> Tape Int
moveLeft (Tape (x:xs) mid ys) = Tape xs x (mid:ys)
moveLeft (Tape [] mid ys) = Tape [] 0 (mid:ys)
-- <
moveRight :: Tape Int -> Tape Int
moveRight (Tape xs mid (y:ys)) = Tape (mid:xs) y ys
moveRight (Tape xs mid [])     = Tape (mid:xs) mid [] 
-- +
add :: Tape Int -> Tape Int 
add (Tape xs x ys) = Tape xs (x + 1) ys
-- - 
minus :: Tape Int -> Tape Int 
minus (Tape xs x ys) = Tape xs (x - 1) ys

