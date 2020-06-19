{-# LANGUAGE Strict #-}

module PRF
    ( PRF(..)
    , compute
    , self
    , one
    , two
    , nth
    , flip2
    , duplicate
    , prfSum
    , prfProduct
    , prfPower
    , prfLimitedDecrement
    , prfLimitedDifference
    , prfLessOrEquals
    , prfLess
    , prfEquals
    , prfFactorial
    , prfIf
    , prfNot
    , prfAnd
    , prfOr
    , prfNotEquals
    , prfXor
    , prfQuotient
    , prfModulo
    , prfIsPrime
    , prfPlog
    , prfSqrt
    , prfPair
    , godelNil
    , godelHead
    , godelTail
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
two = C S [one]
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
prfLimitedDifference = R self (C prfLimitedDecrement [P 3 3])
prfLessOrEquals = C (R one (C Z [P 3 1])) [P 2 1, prfLimitedDifference]
prfLess = C (R one (C Z [P 3 1])) [P 2 1, C prfLimitedDifference [C S [P 2 1], P 2 2]]
prfEquals = C prfProduct [prfLessOrEquals, flip2 prfLessOrEquals]
prfFactorial = duplicate $ R one (C prfProduct [C S [P 3 2], P 3 3])
prfIf = R (P 2 2) (P 4 1)
prfTernary = C prfIf [P 3 2, P 3 3, P 3 1]
prfNot = duplicate $ R one (C Z [P 3 1])
prfAnd = C prfIf [C one [P 2 1], C Z [P 2 1], prfProduct]
prfOr = C prfIf [C one [P 2 1], C Z [P 2 1], prfSum]
prfNotEquals = C prfNot [prfEquals]
prfXor = prfNotEquals
prfQuotDetermine = C prfTernary [C prfLess [C prfLimitedDifference [P 4 1, C prfProduct [P 4 4, P 4 2]], P 4 2], P 4 4, C S [P 4 4]]
prfQuotient = C (R (C Z [P 2 1]) prfQuotDetermine) [P 2 1, P 2 2, P 2 1]
prfModulo = C prfLimitedDifference [P 2 1, C prfProduct [P 2 2, prfQuotient]]
prfIsPrime = duplicate $ R Z (C prfTernary [C prfEquals [P 3 2, C one [P 3 1]], C one [P 3 1], C prfTernary [C prfModulo [P 3 1, P 3 2], C prfProduct [C one [P 3 1], P 3 3], C Z [P 3 1]]])
prfPlogR = R (C Z [P 2 1]) (C prfTernary [C prfEquals [C prfModulo [P 4 1, C prfPower [P 4 2, P 4 4]], C Z [P 4 1]], C S [P 4 4], P 4 4])
prfPlog = C prfLimitedDecrement [C prfPlogR [P 2 2, P 2 1, P 2 2]]
prfIntermediateSqrt = duplicate $ R Z (C prfTernary [C prfLessOrEquals [C prfProduct [P 3 3, P 3 3], P 3 1], C S [P 3 3], P 3 3])
prfSqrt = C prfTernary [C prfLess [self, two], prfIntermediateSqrt, C prfLimitedDecrement [prfIntermediateSqrt]]
prfPair = C prfSum [C prfQuotient [C prfProduct [prfSum, C prfSum [prfSum, C one [P 2 1]]], C two [P 2 1]], P 2 2]

godelNil = one
godelHead = duplicate $ R Z (C prfTernary [C prfEquals [C prfModulo [P 3 1, C prfPower [C two [P 3 1], P 3 2]], C Z [P 3 1]], P 3 2, P 3 3])
godelTail = C prfQuotient [self, C prfPower [two, godelHead]]

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
    putStrLn $ "=== LimitedDifference ==="
    printPRF prfLimitedDifference "LimitedDifference" [8, 6]
    printPRF prfLimitedDifference "LimitedDifference" [8, 8]
    printPRF prfLimitedDifference "LimitedDifference" [6, 6]
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
    putStrLn $ "=== And ==="
    printPRF prfAnd "And" [0, 0]
    printPRF prfAnd "And" [0, 1]
    printPRF prfAnd "And" [1, 0]
    printPRF prfAnd "And" [1, 1]
    putStrLn $ "=== Or ==="
    printPRF prfOr "Or" [0, 0]
    printPRF prfOr "Or" [0, 1]
    printPRF prfOr "Or" [1, 0]
    printPRF prfOr "Or" [1, 1]
    putStrLn $ "=== NotEquals ==="
    printPRF prfNotEquals "NotEquals" [23, 41]
    printPRF prfNotEquals "NotEquals" [21, 43]
    printPRF prfNotEquals "NotEquals" [32, 32]
    putStrLn $ "=== Xor ==="
    printPRF prfXor "Xor" [0, 0]
    printPRF prfXor "Xor" [0, 1]
    printPRF prfXor "Xor" [1, 0]
    printPRF prfXor "Xor" [1, 1]
    putStrLn $ "=== Quotient ==="
    printPRF prfQuotient "Quotient" [5, 2]
    printPRF prfQuotient "Quotient" [2, 5]
    printPRF prfQuotient "Quotient" [5, 5]
    printPRF prfQuotient "Quotient" [49, 5]
    printPRF prfQuotient "Quotient" [50, 5]
    printPRF prfQuotient "Quotient" [51, 5]
    printPRF prfQuotient "Quotient" [0, 5]
    printPRF prfQuotient "Quotient" [5, 0]
    printPRF prfQuotient "Quotient" [0, 0]
    printPRF prfQuotient "Quotient" [3, 1]
    putStrLn $ "=== Modulo ==="
    printPRF prfModulo "Modulo" [5, 2]
    printPRF prfModulo "Modulo" [2, 5]
    printPRF prfModulo "Modulo" [5, 5]
    printPRF prfModulo "Modulo" [49, 5]
    printPRF prfModulo "Modulo" [50, 5]
    printPRF prfModulo "Modulo" [51, 5]
    printPRF prfModulo "Modulo" [0, 5]
    printPRF prfModulo "Modulo" [5, 0]
    printPRF prfModulo "Modulo" [0, 0]
    printPRF prfModulo "Modulo" [3, 1]
    putStrLn $ "=== IsPrime ==="
    printPRF prfIsPrime "IsPrime" [2]
    printPRF prfIsPrime "IsPrime" [3]
    printPRF prfIsPrime "IsPrime" [4]
    printPRF prfIsPrime "IsPrime" [5]
    printPRF prfIsPrime "IsPrime" [6]
    printPRF prfIsPrime "IsPrime" [7]
    printPRF prfIsPrime "IsPrime" [8]
    printPRF prfIsPrime "IsPrime" [9]
    printPRF prfIsPrime "IsPrime" [10]
    printPRF prfIsPrime "IsPrime" [11]
    putStrLn $ "=== Plog ==="
    printPRF prfPlog "Plog" [3, 17]
    printPRF prfPlog "Plog" [3, 18]
    printPRF prfPlog "Plog" [3, 19]
    putStrLn $ "=== Sqrt ==="
    printPRF prfSqrt "Sqrt" [0]
    printPRF prfSqrt "Sqrt" [1]
    printPRF prfSqrt "Sqrt" [2]
    printPRF prfSqrt "Sqrt" [3]
    printPRF prfSqrt "Sqrt" [4]
    printPRF prfSqrt "Sqrt" [5]
    printPRF prfSqrt "Sqrt" [6]
    printPRF prfSqrt "Sqrt" [7]
    printPRF prfSqrt "Sqrt" [8]
    printPRF prfSqrt "Sqrt" [9]
    printPRF prfSqrt "Sqrt" [10]
    putStrLn $ "=== Pair ==="
    printPRF prfPair "Pair" [4, 5]
    printPRF prfPair "Pair" [5, 4]
    printPRF prfPair "Pair" [7, 2]
    printPRF prfPair "Pair" [2, 7]
    putStrLn $ "=== Nil ==="
    printPRF godelNil "Nil" [12]
    putStrLn $ "=== Head ==="
    printPRF godelHead "Head" [10]
    printPRF godelHead "Head" [11]
    printPRF godelHead "Head" [12]
    putStrLn $ "=== Tail ==="
    printPRF godelTail "Tail" [10]
    printPRF godelTail "Tail" [11]
    printPRF godelTail "Tail" [12]