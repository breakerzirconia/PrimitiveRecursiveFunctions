{-# LANGUAGE Strict #-}

module PRF
    ( PRF(..)
    , compute
    , self
    , one
    , nth
    , flip2
    , duplicate
    , prfSum
    , prfProduct
    , prfPower
    , prfLimitedDecrement
    , prfLimitedSub
    , prfLessOrEquals
    , prfLess
    , prfEquals
    , prfFactorial
    , prfIf
    , prfNot
    , printPRF
    ) where

data PRF
    = Z
    | S
    | P Int Int
    | C PRF [PRF]
    | R PRF PRF
    deriving (Eq, Show)

checkNonNegativeArgs :: [Integer] -> [Integer]
checkNonNegativeArgs [] = []
checkNonNegativeArgs (x:xs)
    | x < 0 = error "Negative argument spotted!"
    | otherwise = x : checkNonNegativeArgs xs

compute :: PRF -> [Integer] -> Integer
compute prf args = compute' prf $ checkNonNegativeArgs args

compute' :: PRF -> [Integer] -> Integer
compute' Z args
    | length args /= 1 = error "Z: length args != 1"
    | otherwise = 0
compute' S args
    | length args /= 1 = error "S: length args != 1"
    | otherwise = head args + 1
compute' (P n i) args
    | length args /= n = error $ "P: length args != " ++ show n
    | otherwise = args !! (i - 1)
compute' (C f gs) args
    | length gs > 0 = compute' f $ fmap (flip compute args) gs
    | otherwise = error "C: length gs == 0"
compute' (R f g) args
    | length args > 1 = let (xInit, y) = (init args, last args)
                        in if y == 0
                           then compute f xInit
                           else let args' = xInit ++ [y - 1]
                                in compute g $ args' ++ [compute (R f g) args']
    | otherwise = error "R: length args == 0"

-- utility functions
self = P 1 1
one = C S [Z]
nth :: Int -> PRF
nth x
    | x < 0 = error $ "nth: argument is less than zero: <" ++ show x ++ ">"
    | x == 0 = Z
    | otherwise = C S [nth (x - 1)]
flip2 prf = C prf [P 2 2, P 2 1]
duplicate prf = C prf [self, self]

-- homework functions
prfSum = R self (C S [P 3 3])
prfProduct = R Z (C prfSum [P 3 1, P 3 3])
prfPower = R one (C prfProduct [P 3 1, P 3 3])
prfLimitedDecrement = duplicate $ R Z (P 3 2)
prfLimitedSub = R self (C prfLimitedDecrement [P 3 3])
prfLessOrEquals = C (R one (C Z [P 3 1])) [P 2 1, prfLimitedSub]
prfLess = C (R one (C Z [P 3 1])) [P 2 1, C prfLimitedSub [C S [P 2 1], P 2 2]]
prfEquals = C prfProduct [prfLessOrEquals, flip2 prfLessOrEquals]
prfFactorial = duplicate $ R one (C prfProduct [C S [P 3 2], P 3 3])
prfIf = R (P 2 2) (P 4 1)
prfNot = duplicate $ R one (C Z [P 3 1])

newline :: IO ()
newline = putStrLn ""

listArgs :: [Integer] -> String
listArgs args = "(" ++ ((init . tail . show) args) ++ ")"

printPRF :: PRF -> String -> [Integer] -> IO ()
printPRF prf name args
    = putStrLn
    $ name ++ (listArgs args) ++ " = " ++ show (prf `compute` args)

main :: IO ()
main = do
    putStrLn $ "=== Sum ==="
    printPRF prfSum "Sum" [12, 13]
    printPRF prfSum "Sum" [0, 2]
    printPRF prfSum "Sum" [2, 0]
    printPRF prfSum "Sum" [0, 0]
    putStrLn $ "=== Product ==="
    printPRF prfProduct "Product" [12, 13]
    printPRF prfProduct "Product" [0, 2]
    printPRF prfProduct "Product" [2, 0]
    printPRF prfProduct "Product" [0, 0]
    putStrLn $ "=== Power ==="
    printPRF prfPower "Power" [3, 6]
    printPRF prfPower "Power" [6, 3]
    putStrLn $ "=== LimitedDecrement ==="
    printPRF prfLimitedDecrement "LimitedDecrement" [134]
    printPRF prfLimitedDecrement "LimitedDecrement" [12]
    printPRF prfLimitedDecrement "LimitedDecrement" [1]
    printPRF prfLimitedDecrement "LimitedDecrement" [0]
    putStrLn $ "=== LimitedSub ==="
    printPRF prfLimitedSub "LimitedSub" [8, 6]
    printPRF prfLimitedSub "LimitedSub" [8, 8]
    printPRF prfLimitedSub "LimitedSub" [6, 6]
    putStrLn $ "=== LessOrEquals ==="
    printPRF prfLessOrEquals "LessOrEquals" [43, 12]
    printPRF prfLessOrEquals "LessOrEquals" [12, 12]
    printPRF prfLessOrEquals "LessOrEquals" [11, 12]
    putStrLn $ "=== Less ==="
    printPRF prfLess "Less" [43, 12]
    printPRF prfLess "Less" [12, 12]
    printPRF prfLess "Less" [11, 12]
    putStrLn $ "=== Equals ==="
    printPRF prfEquals "Equals" [23, 41]
    printPRF prfEquals "Equals" [21, 43]
    printPRF prfEquals "Equals" [32, 32]
    putStrLn $ "=== Factorial ==="
    printPRF prfFactorial "Factorial" [0]
    printPRF prfFactorial "Factorial" [1]
    printPRF prfFactorial "Factorial" [6]
    putStrLn $ "=== If ==="
    printPRF prfIf "If" [3, 4, 0]
    printPRF prfIf "If" [3, 4, 1]
    printPRF prfIf "If" [3, 4, 1111]
    putStrLn $ "=== Not ==="
    printPRF prfNot "Not" [23]
    printPRF prfNot "Not" [1]
    printPRF prfNot "Not" [0]