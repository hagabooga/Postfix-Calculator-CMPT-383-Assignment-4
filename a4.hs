-- Token Defintion
data Token = Empty
    | Num Double
    | Op String
    | Err String
    deriving (Show, Eq)

-- Operator Logic
apply_op :: Token -> [Token] -> [Token]
apply_op (Op x) ((Num y): ys)
    | x == "inc" = (Num $ y + 1) : ys

-- Calculate Stack
start_calc_stack :: ([Token], [Token]) -> ([Token], [Token])
start_calc_stack ((x:xs),[]) = ((x:xs),[])
start_calc_stack ([],((Num y):ys)) = ([Num y], ys)
start_calc_stack (((Err x):xs),_) = (((Err x):xs),_)
start_calc_stack (((x:xs), ((Op y):ys))) = start_calc_stack (apply_op (Op y) (x:xs), ys) 
start_calc_stack (_,((Num y):ys)) = start_calc_stack (y:_, ys)

-- String to Token
tokenize :: String -> Token
tokenize x
    | or (map (\z -> x == z) ["inc","dec","sqrt","sin","cos","inv","+","*","-","/","+all","*all","dup","pop","clear","swap"]) = Op x
    | otherwise = Num (read x :: Double)

calcStack :: String -> String
calcStack [] = []
calcStack (x:xs) = show $ fst $ start_calc_stack $ ([], (map tokenize (words $ (x:xs))))
