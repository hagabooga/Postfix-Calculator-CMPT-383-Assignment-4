-- Token Defintion
data Token = Num Double
    | Op String
    | Err String
    deriving (Show, Eq)

-- Operator Logic
apply_op :: Token -> [Token] -> [Token]
-- Empty Stack
apply_op (Op x) []
    | x == "clear" = []
    | otherwise = [Err (x ++ ": empty stack")]
-- 1 Element in the Stack
apply_op (Op x) [Num y]
    | x == "inc" = [Num (y + 1)]
    | x == "dec" = [Num (y - 1)] 
    | x == "sqrt" = [Num (sqrt y)] 
    | x == "sin" = [Num (sin y)] 
    | x == "cos" = [Num (cos y)] 
    | x == "inv" = [Num (1/y)] 
    | x == "+all" = [Num y]
    | x == "*all" = [Num y]
    | x == "dup" = [Num y, Num y] 
    | x == "pop" = []
    | x == "clear" = []
    | or (map (\z -> x == z) ["+","*","-","/", "swap"]) = [Err (x ++ ": not enough arguments")]
    | otherwise = [Err (x ++ ": illegal operator")]
-- 2 or More Elements in the Stack
apply_op (Op x) ((Num y):(Num y2):ys)
    | x == "inc" = (Num (y + 1)) :(Num y2):ys
    | x == "dec" = (Num (y - 1)) :(Num y2):ys
    | x == "sqrt" = (Num (sqrt y)) :(Num y2):ys
    | x == "sin" = (Num (sin y)) :(Num y2):ys
    | x == "cos" = (Num (cos y)) :(Num y2):ys
    | x == "inv" = (Num (1/y)) :(Num y2):ys
    | x == "+" = (Num (y2 + y)) :ys
    | x == "*" = (Num (y2 * y)) :ys
    | x == "-" = (Num (y2 - y)) :ys
    | x == "/" = (Num (y2 / y)) :ys
    | x == "+all" = [Num (foldl (+) 0 (getNumListAsDouble ((Num y):(Num y2):ys)))]
    | x == "*all" = [Num (foldl (*) 1 (getNumListAsDouble ((Num y):(Num y2):ys)))]
    | x == "dup" = (Num y):(Num y):(Num y2):ys
    | x == "pop" = (Num y2):ys
    | x == "clear" = []
    | x == "swap" = (Num y2):(Num y):ys
    | otherwise = Err(x ++ ": illegal operator"):(Num y2):ys

-- Get the Stack and Convert it to a Double List
getNumListAsDouble :: [Token] -> [Double]
getNumListAsDouble [] = []
getNumListAsDouble ((Num x):xs) = x : getNumListAsDouble xs

-- Calculate Stack (Apply the given equation by using a stack. Input is (stack, equation). Output will be (final stack, []))
start_calc_stack :: ([Token], [Token]) -> ([Token], [Token])
---- Terminating Cases
-- Empty Stack
start_calc_stack ([],[]) = ([Err "empty stack"],[])
start_calc_stack ((x:xs),[]) = ((x:xs),[])
-- End if there is an error
start_calc_stack (_,((Err x):ys)) = ([Err x],[])
start_calc_stack (((Err x):xs), (y:ys)) = ([Err x],[])
----- Recursion
-- Push Num to Empty Stack
start_calc_stack ([],((Num y):ys)) = start_calc_stack ([Num y], ys)
-- Push Num to Stack
start_calc_stack ((x:xs),((Num y):ys)) = start_calc_stack ((Num y):(x:xs), ys)
-- Apply Op to Empty Stack
start_calc_stack ([], (Op y):ys) = start_calc_stack (apply_op (Op y) [], ys)
-- Apply Op to Stack
start_calc_stack (((x:xs), (Op y):ys)) = start_calc_stack (apply_op (Op y) (x:xs), ys) 

-- Check String is Double (must be a number and can only have 1 decimal that is a number following)
stringIsDouble :: String -> Bool -> Bool
stringIsDouble [] _ = True
stringIsDouble (x:xs) True
    | and (map (\z -> not $ x == z) ['1','2','3','4','5','6','7','8','9','0']) = False
    | otherwise = stringIsDouble xs True
stringIsDouble (x:xs) False
    | last (x:xs) == '.' = False
    | or (map (\z -> x == z) ['1','2','3','4','5','6','7','8','9','0']) = stringIsDouble xs False
    | x == '.' = stringIsDouble xs True
    | otherwise = False

-- String to Token (Op must be operators shown in assignment & Num must be a double. Anything else is illegal.)
tokenize :: String -> Token
tokenize [] = Err "empty stack"
tokenize (x:xs)
    | or (map (\z -> (x:xs) == z) ["inc","dec","sqrt","sin","cos","inv","+","*","-","/","+all","*all","dup","pop","clear","swap"]) = Op (x:xs)
tokenize (x:xs) 
    | x == '-' = if stringIsDouble xs False then Num (read (x:xs) :: Double) 
        else Err ((x:xs) ++ ": illegal operator")
    | or (map (\z -> x == z) ['1','2','3','4','5','6','7','8','9','0']) = 
        if stringIsDouble (x:xs) False then Num (read (x:xs) :: Double) 
        else Err ((x:xs) ++ ": illegal double")
    | otherwise = Err ((x:xs) ++ ": illegal operator")

-- Token to String (Get values from tokens as strings)
tokenToString :: Token -> String
tokenToString (Num x) = show x
tokenToString (Err x) = x

-- Assignment Functions
calcStack :: String -> String
calcStack [] = show [Err "empty stack"]
calcStack (x:xs) = show $ fst $ start_calc_stack ([], (map tokenize (words $ (x:xs))))

-- 'head' function pops the stack
calc :: String -> String
calc [] = "empty stack"
calc (x:xs) = tokenToString $ head $ fst $ start_calc_stack ([], (map tokenize (words (x:xs))))

