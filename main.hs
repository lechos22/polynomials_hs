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
divisors n = smalls ++ middle ++ bigs where
  root = sqrt (fromIntegral n :: Double)
  smalls = [x | x <- [1 .. (ceiling root :: Integral a => a) - 1], n `mod` x == 0]
  root_int = floor root :: Integral a => a
  middle = [root_int | fromIntegral root_int == root]
  bigs = map (n `div`) smalls

polysolve :: [Double] -> Solutions Double
polysolve [] = InfiniteSolutions -- empty polynomial
polysolve (0:as) = polysolve as -- skip leading zeros
polysolve [_] = SolutionList [] -- constant different from 0
polysolve [a, b] = SolutionList [-b / a] -- linear
polysolve [a, b, c] = result -- quadratic
  where
    d = b ^ 2 - 4 * a * c
    d0 = (-b) / (2 * a)
    result
      | d < 0 = SolutionList []
      | d == 0 = SolutionList [d0, d0]
      | d > 0 = SolutionList [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
polysolve (a0:as) = result -- higher degree
  where
    eps = 1e-6 -- acceptable error
    a_last = last (a0:as)
    a_last_divisors = map fromIntegral $ divisors (round $ abs a_last :: Int)
    a0_divisors = map fromIntegral $ divisors (round $ abs a0 :: Int)
    pos_candidates = [a / b | a <- a_last_divisors, b <- a0_divisors]
    candidates
     | areInts (a0:as) = 0:(pos_candidates ++ map negate pos_candidates)
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

showPoly :: String -> [Double] -> IO ()
showPoly _ [] = putStrLn "0"
showPoly name poly = putStrLn $ name ++ "(x): " ++ unwords (map show poly)

showPoly' :: String -> [Double] -> IO ()
showPoly' _ [] = putStrLn "0"
showPoly' name poly = putStrLn $ name ++ "(x) = " ++ intercalate " + " (reverse $ map showTerm $ filter ((/=0).snd) $ zip [0 ..] poly)
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
  case c of
    "+" -> showPoly "Q" $ polyadd w p
    "-" -> showPoly "Q" $ polysub w p
    "*" -> showPoly "Q" $ polymulp w p
    "/" -> do
        let (q, r) = polydiv w p
        showPoly "Q" q
        showPoly "R" r
    "y" -> putStrLn ("y = " ++ show (polyapply w x))
    "0" -> putStrLn ("A ⊇ " ++ solutionsToString (polysolve w))
    _ -> error "Unreachable"
