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

polysolve :: Eq a => Num a => Fractional a => Floating a => Ord a => [a] -> Solutions a
polysolve [] = InfiniteSolutions
polysolve (0: xs) = polysolve xs
polysolve [_] = SolutionList []
polysolve [_, 0] = SolutionList [0]
polysolve [a, b] = SolutionList [-b/a]
polysolve [a, b, c] = result where
  d = b^2 - 4 * a * c
  result
   | d < 0  = SolutionList []
   | d == 0 = SolutionList [(-b) / (2 * a)]
   | d > 0  = SolutionList [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
polysolve _ = error "Polynomial degree is strictly greater than 2, I can't solve."

solutionsToString :: Show a => Solutions a -> String
solutionsToString (SolutionList solutions) =
  case solutions of
    [] -> "∅"
    _ -> "{" ++ intercalate ", " (map show solutions) ++ "}"
solutionsToString InfiniteSolutions = "R"

flush :: IO ()
flush = hFlush stdout

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error msg

main :: IO Int
main = do
  putStr "Choose operation [+-*/0]: "
  flush
  c <- getLine
  assert (length c == 1) "Invalid operation"
  assert (head c `elem` "+-*/0") "Invalid operation"
  let operationMessage
       | c == "/" = "W(x) = P(x) * Q(x) + R(x)"
       | c == "0" = "W(x) = 0 <=> x ∈ A"
       | otherwise = "Q(x) = W(x) " ++ c ++ " P(x)"
  putStrLn operationMessage
  putStr "W(x): "
  flush
  w <- getLine
  p <-
    if c /= "0" then do
      putStr "P(x): "
      flush
      getLine
    else return ""
  let
    w' = map read $ words w
    p' = map read $ words p
    result =
      case c of
        "+" -> "Q(x): " ++ unwords (map show $ polyadd w' p')
        "-" -> "Q(x): " ++ unwords (map show $ polysub w' p')
        "*" -> "Q(x): " ++ unwords (map show $ polymulp w' p')
        "/" -> text where
          (q, r) = polydiv w' p'
          text = "Q(x): " ++ unwords (map show q)
            ++ "\nR(x): " ++ unwords (map show r)
        "0" -> "A = " ++ solutionsToString (polysolve w')
        _ -> error "Unreachable"
  putStrLn result
  return 0
