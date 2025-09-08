module AHD.Util where


import Clash.Prelude

-- | Prints the truth table for a two-inbut Bit-function
--
-- >>> eval2 (\x y -> x `xor` y)

eval2 :: (Show a) => (Bit -> Bit -> a) -> IO ()
eval2 f = do
  putStrLn "a b | f(a, b)"
  putStrLn "-------------"
  mapM_ (\(a,b,v) -> putStrLn $ show a <> " " <> show b <> " | " <> show v) vals
  where
    vals = [(a,b,f a b) | a <- [0,1], b <- [0,1]]

eval3 :: (Show a) => (Bit -> Bit -> Bit -> a) -> IO ()
eval3 f = do
  putStrLn "a b c | f(a, b, c)"
  putStrLn "------------------"
  mapM_ (\(a,b,c, v) -> putStrLn $ show a <> " " <> show b <> " " <> show c <> " | " <> show v) vals
  where
    vals = [(a, b, c,f a b c) | a <- [0,1], b <- [0,1], c <- [0,1]]
