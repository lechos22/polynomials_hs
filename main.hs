import Data.List (transpose)
import System.IO

type EqNum a = (Num a, Eq a)

polytrim :: EqNum a => [a] -> [a]
polytrim (0:xs) = polytrim xs
polytrim arr = arr

polyzip :: EqNum a => [a] -> [a] -> [[a]]
polyzip a1 a2 = zipped where
  sizedif = length a1 - length a2
  pad_a1 =
    if sizedif < 0
    then (replicate (-sizedif) 0) ++ a1
    else a1
  pad_a2 =
    if sizedif > 0
    then (replicate sizedif 0) ++ a2
    else a2
  zipped = transpose [pad_a1, pad_a2]

polyadd :: EqNum a => [a] -> [a] -> [a]
polyadd p1 p2 = result where
  sums (x:xs) = (sum x: sums xs)
  sums [] = []
  result = sums $ polyzip p1 p2

polyopp :: EqNum a => [a] -> [a]
polyopp [] = []
polyopp (x:xs) = (-x: polyopp xs)

polysub :: EqNum a => [a] -> [a] -> [a]
polysub p1 p2 = polyadd p1 $ polyopp p2

polymuln :: EqNum a => [a] -> a -> [a]
polymuln [] _ = []
polymuln (x:xs) y = (x*y: polymuln xs y)

polymulp :: EqNum a => [a] -> [a] -> [a]
polymulp p1 p2 = result where
  pmul [] _ acc = acc
  pmul (x:xs) padding acc =
    let
      to_add = polymuln p2 x ++ padding
      new_acc = polyadd acc to_add
    in
      pmul xs (0:padding) new_acc
  result = pmul (reverse p1) [] []

polydiv :: EqNum a => Fractional a => [a] -> [a] -> ([a], [a])
polydiv _ [] = ([1/0], [])
polydiv p1 p2 = pdiv p1 [] where
  p2trim = polytrim p2
  (y:_) = p2trim
  pdiv pol acc = result where
    p1trim = polytrim pol
    (x:_) = p1trim
    len_dif = (length p1trim) - (length p2trim)
    to_acc = x / y
    new_acc = polyadd acc (to_acc: replicate len_dif 0)
    to_subtract = polymuln p2trim to_acc ++ replicate len_dif 0
    after_subtract = polysub p1trim to_subtract
    result =
      if len_dif < 0
      then (acc, p1trim)
      else pdiv after_subtract new_acc

flush :: IO ()
flush = hFlush stdout

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error msg

main :: IO Int
main = do
  putStr "Choose operation [+-*/%]: "
  flush
  c <- getLine
  assert (length c == 1) "Invalid operation"
  assert (elem (head c) "+-*/%") "Invalid operation"
  putStr "Chosen operation: A "
  putStr c
  putStrLn " B"
  putStr "Input coeffictients of polynomial A: "
  flush
  a <- getLine
  putStr "Input coeffictients of polynomial B: "
  flush
  b <- getLine
  let
    a' = map read $ words a
    b' = map read $ words b
    result =
      case c of
        "+" -> polyadd a' b'
        "-" -> polysub a' b'
        "*" -> polymulp a' b'
        "/" -> fst $ polydiv a' b'
        "%" -> snd $ polydiv a' b'
        _ -> []
  putStrLn $ "Result: " ++ (unwords $ map show result)
  return 0
