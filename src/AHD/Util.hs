module AHD.Util where


import Clash.Prelude

-- | Creates the truth table for a 2-bit `Bit`-function
--
-- >>> truthTable2 (\a b -> a `xor` b)
-- [0,1,1,0]


truthTable2 :: (Bit -> Bit -> a) -> [a]
truthTable2 f = [f a b | a <- [0,1], b <- [0,1]]

-- | Creates the truth table for a 3-bit `Bit`-function
--
-- >>> truthTable3 (\a b c -> (a `xor` b) .|. c)
-- [0,1,1,1,1,1,0,1]
truthTable3 :: (Bit -> Bit -> Bit -> a) -> [a]
truthTable3 f =  [f a b c | a <- [0,1], b <- [0,1], c <- [0,1]]

-- | Creates the truth table for a 2-bit `Bit`-function prepending the inputs to the function's value
--
-- >>> truthTable2' (\a b -> a `xor` b)
-- [(0,0,0),(0,1,1),(1,0,1),(1,1,0)]
truthTable2' :: (Bit -> Bit -> a) -> [(Bit, Bit, a)]
truthTable2' f = [(a, b, f a b) | a <- [0,1], b <- [0,1]]



-- | Creates the truth table for a 3-bit `Bit`-function prepending the inputs to the function's value
--
--  >>> truthTable3' (\a b c -> (a `xor` b) .|. c)
--  [(0,0,0,0),(0,0,1,1),(0,1,0,1),(0,1,1,1),(1,0,0,1),(1,0,1,1),(1,1,0,0),(1,1,1,1)]

truthTable3' :: (Bit -> Bit -> Bit -> a) -> [(Bit, Bit, Bit, a)]
truthTable3' f =  [(a,b,c,f a b c) | a <- [0,1], b <- [0,1], c <- [0,1]]

-- | Pretty prints the truth table of a 2-bit `Bit`-function
--
-- > eval2 (\a b -> a `xor` b)
-- > a b | f(a, b)
-- > -------------
-- > 0 0 | 0
-- > 0 1 | 1
-- > 1 0 | 1
-- > 1 1 | 0
eval2 :: (Show a) => (Bit -> Bit -> a) -> IO ()
eval2 f = do
  putStrLn "a b | f(a, b)"
  putStrLn "-------------"
  mapM_ (\(a,b,v) -> putStrLn $ show a <> " " <> show b <> " | " <> show v) vals
    where vals = truthTable2' f


-- | Pretty prints the truth table of a 3-bit `Bit`-function
--
-- > eval3 (\a b c -> (a `xor` b) .|. c)
-- > a b c | f(a, b, c)
-- > ------------------
-- > 0 0 0 | 0
-- > 0 0 1 | 1
-- > 0 1 0 | 1
-- > 0 1 1 | 1
-- > 1 0 0 | 1
-- > 1 0 1 | 1
-- > 1 1 0 | 0
eval3 :: (Show a) => (Bit -> Bit -> Bit -> a) -> IO ()
eval3 f = do
  putStrLn "a b c | f(a, b, c)"
  putStrLn "------------------"
  mapM_ (\(a,b,c, v) -> putStrLn $ show a <> " " <> show b <> " " <> show c <> " | " <> show v) vals
    where vals = truthTable3' f


-- $setup
-- >>> import Clash.Prelude
