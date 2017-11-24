#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE BangPatterns #-}

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      !seven = add (1 + 2) undefined -- (1 + 3)

  putStrLn $ "Five: " ++ show five

