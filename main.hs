import Data.List (transpose)
import System.IO (hFlush, stdout)

polytrim :: Eq a => Num a => [a] -> [a]
polytrim (0:xs) = polytrim xs
polytrim arr = arr

polyzip :: Num a => [a] -> [a] -> [[a]]
polyzip a1 a2 = zipped where
  size_dif = length a1 - length a2
  zipped = transpose
    [ replicate (max 0 (-size_dif)) 0 ++ a1
    , replicate (max 0 size_dif) 0 ++ a2
    ]

polyadd :: Num a => [a] -> [a] -> [a]
polyadd p1 p2 = map sum $ polyzip p1 p2

polyopp :: Num a => [a] -> [a]
polyopp = map negate

polysub :: Num a => [a] -> [a] -> [a]
polysub p1 p2 = polyadd p1 $ polyopp p2

polymuln :: Num a => [a] -> a -> [a]
polymuln p n = map (*n) p

polymulp :: Num a => [a] -> [a] -> [a]
polymulp p1 p2 = result where
  pmul [] _ acc = acc
  pmul (x:xs) padding acc =
    let
      to_add = polymuln p2 x ++ padding
      new_acc = polyadd acc to_add
    in
      pmul xs (0:padding) new_acc
  result = pmul (reverse p1) [] []

polydiv :: Eq a => Num a => Fractional a => [a] -> [a] -> ([a], [a])
polydiv _ [] = ([1/0], [])
polydiv p1 p2 = pdiv p1 [] where
  p2trim = polytrim p2
  (y:_) = p2trim
  pdiv pol acc = result where
    p1trim = polytrim pol
    (x:_) = p1trim
    len_dif = length p1trim - length p2trim
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
  putStr "Choose operation [+-*/]: "
  flush
  c <- getLine
  assert (length c == 1) "Invalid operation"
  assert (head c `elem` "+-*/") "Invalid operation"
  putStrLn $
    if c == "/"
    then "A(x) = B(x) * Q(x) + R(x)"
    else "Q(x) = A(x) " ++ c ++ " B(x)"
  putStr "A(x): "
  flush
  a <- getLine
  putStr "B(x): "
  flush
  b <- getLine
  let
    a' = map read $ words a
    b' = map read $ words b
    result =
      case c of
        "+" -> "Q(x): " ++ unwords (map show $ polyadd a' b')
        "-" -> "Q(x): " ++ unwords (map show $ polysub a' b')
        "*" -> "Q(x): " ++ unwords (map show $ polymulp a' b')
        "/" -> text where
          (q, r) = polydiv a' b'
          text = "Q(x): " ++ unwords (map show q)
            ++ "\nR(x): " ++ unwords (map show r)
        _ -> error "Unreachable"
  putStrLn result
  return 0
