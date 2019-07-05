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
    | otherwise = Err(x++ ": illegal operator") : ys

-- Calculate Stack
start_calc_stack :: ([Token], [Token]) -> ([Token], [Token])
start_calc_stack ((x:xs),[]) = ((x:xs),[])
start_calc_stack ([], ((Op y):ys)) = ([Err (y++": empty stack")], ys)
start_calc_stack ([],((Num y):ys)) = start_calc_stack ([Num y], ys)
start_calc_stack (((Err x):xs), (y:ys)) = (((Err x):xs),(y:ys))
start_calc_stack (((x:xs), ((Op y):ys))) = start_calc_stack (apply_op (Op y) (x:xs), ys) 
start_calc_stack ((x:xs),((Num y):ys)) = start_calc_stack ((Num y):(x:xs), ys)

-- String to Token
tokenize :: String -> Token
tokenize x
    | or (map (\z -> x == z) ["inc","dec","sqrt","sin","cos","inv","+","*","-","/","+all","*all","dup","pop","clear","swap"]) = Op x
-- implement maybe for tokenizing  

calcStack :: String -> String
calcStack [] = []
calcStack (x:xs) = show $ fst $ start_calc_stack $ ([], (map tokenize (words $ (x:xs))))
