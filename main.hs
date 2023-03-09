import           Data.List (intercalate, transpose, foldl')
import           System.IO (hFlush, stdout)

polytrim :: [Double] -> [Double]
polytrim (0:as) = polytrim as
polytrim arr    = arr

polyzip :: [Double] -> [Double] -> [[Double]]
polyzip p1 p2 = zipped
  where
    size_dif = length p1 - length p2
    zipped =
      transpose
        [ replicate (max 0 (-size_dif)) 0 ++ p1
        , replicate (max 0 size_dif) 0 ++ p2
        ]

polyadd :: [Double] -> [Double] -> [Double]
polyadd p1 p2 = map sum $ polyzip p1 p2

polyopp :: [Double] -> [Double]
polyopp = map negate

polysub :: [Double] -> [Double] -> [Double]
polysub p1 p2 = polyadd p1 $ polyopp p2

polymuln :: [Double] -> Double -> [Double]
polymuln p n = map (* n) p

polymulp :: [Double] -> [Double] -> [Double]
polymulp p1 p2 = result
  where
    pmul [] acc = acc
    pmul (a:as) acc =
      let to_add = polymuln p2 a ++ replicate (length as) 0
          new_acc = polyadd acc to_add
       in pmul as new_acc
    result = pmul p1 []

polydiv :: [Double] -> [Double] -> ([Double], [Double])
polydiv _ [] = ([1 / 0], [])
polydiv p1 p2 = pdiv p1 []
  where
    p2trim = polytrim p2
    (y:_) = p2trim
    pdiv pol acc = result
      where
        p1trim = polytrim pol
        (x:_) = p1trim
        len_dif = length p1trim - length p2trim
        to_acc = x / y
        new_acc = polyadd acc (to_acc : replicate len_dif 0)
        to_subtract = polymuln p2trim to_acc ++ replicate len_dif 0
        after_subtract = polysub p1trim to_subtract
        result =
          if len_dif < 0
            then (acc, p1trim)
            else pdiv after_subtract new_acc

data Solutions a
  = SolutionList [a]
  | InfiniteSolutions

joinSolutions :: Solutions a -> Solutions a -> Solutions a
joinSolutions (SolutionList a) (SolutionList b) = SolutionList $ a ++ b
joinSolutions _ _ = InfiniteSolutions

polyapply :: [Double] -> Double -> Double
polyapply [] _ = 0
polyapply p x = papply p 0
  where
    papply [] acc = acc
    papply (a:as) acc = papply as new_acc
      where
        deg = length as
        new_acc = acc + a * x ^ deg

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

areInts :: [Double] -> Bool
areInts [] = True
areInts arr = all isInt arr

divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1 .. ceiling $ sqrt (fromIntegral n :: Double) :: Integral a => a], n `mod` x == 0]

polysolve :: [Double] -> Solutions Double
polysolve [] = InfiniteSolutions -- empty polynomial
polysolve (0:as) = polysolve as -- skip leading zeros
polysolve [_] = SolutionList [] -- constant different from 0
polysolve [a, b] = SolutionList [-b / a] -- linear
polysolve [a, b, c] = result -- quadratic
  where
    d = b ^ 2 - 4 * a * c
    result
      | d < 0 = SolutionList []
      | d == 0 = SolutionList [(-b) / (2 * a)]
      | d > 0 = SolutionList [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
polysolve (a0:as) = result -- higher degree
  where
    eps = 1e-6 -- acceptable error
    a_last = last (a0:as)
    a_last_divisors = map fromIntegral $ divisors (ceiling $ abs a_last :: Int)
    candidates
     | areInts (a0:as) && a0 == 1.0 = 0:(a_last_divisors ++ map negate a_last_divisors)
     | otherwise = [0]
    guessed = filter (\x -> abs (polyapply (a0:as) x) < eps) candidates
    new_poly = foldl' (\acc x -> fst $ polydiv acc [1, -x]) (a0:as) guessed
    result
     | null guessed = SolutionList []
     | otherwise = joinSolutions (SolutionList guessed) $ polysolve new_poly

solutionsToString :: Show a => Solutions a -> String
solutionsToString (SolutionList []) = "∅"
solutionsToString (SolutionList solutions) =
  "{" ++ intercalate ", " (map show solutions) ++ "}"
solutionsToString InfiniteSolutions = "R"

flush :: IO ()
flush = hFlush stdout

assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False msg = error msg

readPoly :: String -> IO [Double]
readPoly name = do
  putStr $ name ++ "(x): "
  flush
  line <- getLine
  let poly = map read $ words line
      poly' = polytrim poly
  return poly'

readNumber :: Read a => String -> IO a
readNumber name = do
  putStr $ name ++ " = "
  flush
  read <$> getLine

showPoly :: [Double] -> String
showPoly [] = "0"
showPoly poly = intercalate " + " $ reverse $ zipWith (curry showTerm) [0 .. ] (reverse poly)
  where
    showTerm (0, c) = show c
    showTerm (1, c) = show c ++ "x"
    showTerm (n, c) = show c ++ "x^" ++ show n

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
        | c == "0" = "A = {x | W(x) = 0}"
        | otherwise = "Q(x) = W(x) " ++ c ++ " P(x)"
  putStrLn operationMessage
  w <- readPoly "W"
  p <-
    if c /= "0" && c /= "y"
      then readPoly "P"
      else return []
  x <-
    if c == "y"
      then readNumber "x"
      else return 0
  putStrLn $
    case c of
      "+" -> "Q(x) = " ++ showPoly (polyadd w p)
      "-" -> "Q(x) = " ++ showPoly (polysub w p)
      "*" -> "Q(x) = " ++ showPoly (polymulp w p)
      "/" -> text
        where
          (q, r) = polydiv w p
          text =
            "Q(x) = " ++ showPoly q ++
            "\nR(x) = " ++ showPoly r
      "y" -> "y = " ++ show (polyapply w x)
      "0" -> "A ⊇ " ++ solutionsToString (polysolve w)
      _ -> error "Unreachable"
