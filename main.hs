import Data.List (transpose, intercalate)
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

data Solutions a = SolutionList [a] | InfiniteSolutions

polyapply :: Num a => Floating a => [a] -> a -> a
polyapply [] _ = 0
polyapply a x = papply a 0 where
  papply [] acc = acc
  papply (a:as) acc = papply as new_acc where
    deg = length as
    new_acc = acc + a * x^deg

polysolve :: Eq a => Num a => Fractional a => Floating a => Ord a => [a] -> Solutions a
polysolve [] = InfiniteSolutions
polysolve (0: xs) = polysolve xs
polysolve [_] = SolutionList []
polysolve [a, b] = SolutionList [-b/a]
polysolve [a, b, c] = result where
  d = b^2 - 4 * a * c
  result
   | d < 0  = SolutionList []
   | d == 0 = SolutionList [(-b) / (2 * a)]
   | d > 0  = SolutionList [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
polysolve _ = error "Polynomial degree is too high"

solutionsToString :: Show a => Solutions a -> String
solutionsToString (SolutionList []) = "∅"
solutionsToString (SolutionList solutions) =
  "{" ++ intercalate ", " (map show solutions) ++ "}"
solutionsToString InfiniteSolutions = "R"

flush :: IO ()
flush = hFlush stdout

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error msg

readPoly :: Read a => Eq a => Num a => String -> IO [a]
readPoly name = do
  putStr $ name ++ "(x): "
  flush
  line <- getLine
  let
    poly = map read $ words line
    poly' = polytrim poly
  return poly'

readNumber :: Read a => String -> IO a
readNumber name = do
  putStr $ name ++ " = "
  flush
  read <$> getLine

main :: IO ()
main = do
  putStr "Choose operation [+-*/y0]: "
  flush
  c <- getLine
  assert (length c == 1) "Invalid operation"
  assert (head c `elem` "+-*/y0") "Invalid operation"
  let operationMessage
       | c == "/" = "W(x) = P(x) * Q(x) + R(x)"
       | c == "y" = "y = W(x)"
       | c == "0" = "W(x) = 0 <=> x ∈ A"
       | otherwise = "Q(x) = W(x) " ++ c ++ " P(x)"
  putStrLn operationMessage
  w <- readPoly "W"
  p <-
    if c /= "0" && c /= "y" then readPoly "P"
    else return []
  x <-
    if c == "y" then readNumber "x"
    else return 0
  putStrLn $ case c of
    "+" -> "Q(x): " ++ unwords (map show $ polyadd w p)
    "-" -> "Q(x): " ++ unwords (map show $ polysub w p)
    "*" -> "Q(x): " ++ unwords (map show $ polymulp w p)
    "/" -> text where
      (q, r) = polydiv w p
      text = "Q(x): " ++ unwords (map show q)
        ++ "\nR(x): " ++ unwords (map show r)
    "y" -> "y = " ++ show (polyapply w x)
    "0" -> "A = " ++ solutionsToString (polysolve w)
    _ -> error "Unreachable"
