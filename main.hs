import Data.List (transpose)

type EqNum a = (Num a, Eq a)

pad :: EqNum a => a -> Int -> [a]
pad _ 0 = []
pad x n = (x: pad x (n-1))

polytrim :: EqNum a => [a] -> [a]
polytrim (0:xs) = polytrim xs
polytrim arr = arr

polyzip :: EqNum a => [a] -> [a] -> [[a]]
polyzip a1 a2 = let
  sizedif = length a1 - length a2
  pad_a1 =
    if sizedif > 0
    then a1
    else (pad 0 (-sizedif)) ++ a1
  pad_a2 =
    if sizedif < 0
    then a2
    else (pad 0 (sizedif)) ++ a2
  in
    transpose [pad_a1, pad_a2]

polyadd :: EqNum a => [a] -> [a] -> [a]
polyadd p1 p2 = let
  sums (x:xs) = (sum x: sums xs)
  sums [] = []
  in
    sums $ polyzip p1 p2

polyopp :: EqNum a => [a] -> [a]
polyopp [] = []
polyopp (x:xs) = (-x: polyopp xs)

polysub :: EqNum a => [a] -> [a] -> [a]
polysub p1 p2 = polyadd p1 $ polyopp p2

polymuln :: EqNum a => [a] -> a -> [a]
polymuln [] _ = []
polymuln (x:xs) y = (x*y: polymuln xs y)

polymulp :: EqNum a => [a] -> [a] -> [a]
polymulp p1 p2 = let
  pmul [] _ _ acc = acc
  pmul (x:xs) p padding acc = let
    to_add = polymuln p x ++ padding
    new_acc = polyadd acc to_add
    in pmul xs p (0:padding) new_acc
  in
    pmul (reverse p1) p2 [] []

polydiv :: EqNum a => Fractional a => [a] -> [a] -> ([a], [a])
polydiv _ [] = ([1/0], [])
polydiv p1 p2 = let
  p2trim = polytrim p2
  (y:_) = p2trim
  div pol acc = result where
    p1trim = polytrim pol
    (x:_) = p1trim
    len_dif = (length p1trim) - (length p2trim)
    to_acc = x / y
    new_acc = polyadd acc (to_acc: pad 0 len_dif)
    to_subtract = polymuln p2trim to_acc ++ pad 0 len_dif
    after_subtract = polysub p1trim to_subtract
    result =
      if len_dif < 0
      then (acc, p1trim)
      else div after_subtract new_acc
  in
    div p1 []

main :: IO Int
main = do
  print $ polydiv [6,13,1,-2] [1,2.5,1]
  return 0

