{-# LANGUAGE Strict #-}

module PRF
    ( PRF(..)
    , compute
    , self
    , one
    , nth
    , prfSum
    , prfProduct
    , prfPower
    , prfLimitedDecrement
    , prfLimitedSub
    , prfLess
    , prfFactorial
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

newline :: IO ()
newline = putStrLn ""

-- utility functions
self = P 1 1
one = C S [Z]
nth :: Int -> PRF
nth x
    | x < 0 = error $ "nth: argument is less than zero: <" ++ show x ++ ">"
    | x == 0 = Z
    | otherwise = C S [nth (x - 1)]

-- homework functions
prfSum = R self (C S [P 3 3])
prfProduct = R Z (C prfSum [P 3 1, P 3 3])
prfPower = R one (C prfProduct [P 3 1, P 3 3])
prfLimitedDecrement = C (R Z (P 3 2)) [self, self]
prfLimitedSub = R self (C prfLimitedDecrement [P 3 3])
prfFalseLess = C (R one (C Z [P 3 1])) [P 2 1, prfLimitedSub] -- incorrect
prfLess = C (R one (C Z [P 3 1])) [P 2 1, C prfLimitedSub [C S [P 2 1], P 2 2]]
prfFactorial = C (R one (C prfProduct [C S [P 3 2], P 3 3])) [self, self]

printPRF :: PRF -> [Integer] -> IO ()
printPRF prf args = print $ prf `compute` args

main :: IO ()
main = do
    printPRF prfSum [12, 13]
    printPRF prfProduct [12, 13]
    printPRF prfPower [3, 5]
    printPRF prfLimitedDecrement [4]
    printPRF prfLimitedDecrement [0]
    printPRF prfLimitedSub [8, 6]
    printPRF prfLimitedSub [8, 8]
    printPRF prfLimitedSub [6, 6]
    printPRF prfFalseLess [43, 12]
    printPRF prfFalseLess [12, 12]
    printPRF prfFalseLess [11, 12]
    printPRF prfLess [43, 12]
    printPRF prfLess [12, 12]
    printPRF prfLess [11, 12]
    printPRF prfFactorial [0]
    printPRF prfFactorial [1]
    printPRF prfFactorial [7]