data Token = Empty
    | Num Double
    | Op String
    | Err String
    deriving (Show, Eq)

get_num :: Token -> Double
get_num (Num x) = x

inc :: [Token] -> [Token]
inc [] = [Err "inc: empty stack"]
inc (x:xs) 
    | is_num x = (Num $ (get_num x) + 1 ): xs
    | otherwise = Err "inc: arg not a Number" : xs

plus :: [Token] -> [Token]
plus [] = [Err "+: empty stack"]
plus (x:xs)
    | length (x:xs) < 2 = Err "+: not enough args" : xs
    | is_num x = Num( (get_num x) + (get_num (xs!!0))) : tail xs

is_num :: Token -> Bool
is_num (Num x) = True
is_num _ = False

is_op :: Token -> Bool
is_op (Op x) = True
is_op _ = False

is_err :: Token -> Bool
is_err (Err x) = True
is_err _ = False

find_op :: Token -> ([Token] -> [Token])
find_op x
    | x == Op "inc" = inc
    | x == Op "+" = plus

start_calc_stack :: ([Token], [Token]) -> ([Token], [Token])
start_calc_stack ([],(y:ys)) 
    | is_op y = start_calc_stack (find_op y $ [], (ys)) 
    | otherwise = start_calc_stack ([y], (ys))
start_calc_stack ((x:xs),[]) = ((x:xs),[])
start_calc_stack ((x:xs), (y:ys))
    | is_err x = ((x:xs), (y:ys))
    | is_op y = start_calc_stack  (find_op y $ (x:xs), ys)
    | otherwise = start_calc_stack (y:(x:xs), ys)

tokenize :: String -> Token
tokenize x 
    | or [x == "inc", x == "dec", x == "sqrt", x == "sin", x == "cos", x == "inv", x == "+"] = Op x
    | otherwise = Num (read x :: Double)

calcStack :: String -> String
calcStack [] = []
calcStack (x:xs) = show $ fst $ start_calc_stack $ ([], (map tokenize (words $ (x:xs))))

