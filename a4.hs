data Token = Num Double
    | Op String
    deriving (Show, Eq)
    
inc :: Token -> Double
inc (Num x) = x + 1

main = print $ inc (Num 1)