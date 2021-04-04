module Intrepreter where

-- Zipper based Brainfuck intrepreter 
--
-- lefts are stored in opposite order
data Tape a = Tape {    lefts  :: [a]
                      , focus  :: a
                      , rights :: [a]
                   } 
 deriving Show

empTape :: Tape Int
empTape = Tape [] 0 []

ex1 :: String
ex1 = "+++<+.+"

-- >
moveLeft :: Tape Int -> Tape Int
moveLeft (Tape (x:xs) mid ys) = Tape xs x (mid:ys)
moveLeft (Tape [] mid ys) = Tape [] 0 (mid:ys)
-- <
moveRight :: Tape Int -> Tape Int
moveRight (Tape xs mid (y:ys)) = Tape (mid:xs) y ys
moveRight (Tape xs mid [])     = Tape (mid:xs) mid [] 
-- +
increment :: Tape Int -> Tape Int 
increment (Tape xs x ys) = Tape xs (x + 1) ys
-- - 
decrement :: Tape Int -> Tape Int 
decrement (Tape xs x ys) = Tape xs (x - 1) ys

readMemory :: Tape Int -> Int
readMemory (Tape xs x ys) = x

modifyMemory :: Tape Int -> Int -> Tape Int
modifyMemory (Tape xs x ys) x' = Tape xs x' ys


run :: String -> Tape Int -> IO () 
run (x:xs) tape  
  | x == '+' = run xs (increment tape)
  | x == '-' = run xs (decrement tape)
  | x == '>' = run xs (moveRight tape)
  | x == '<' = run xs (moveLeft tape)
  | x == ',' = do
    x' <- readLn
    run xs (modifyMemory tape (read x' :: Int))
  |  x == '.' = do
      print tape
      run xs tape
run [] tape = print $ tape

