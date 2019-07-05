data Token = Num Double
    | Op String
    deriving (Show, Eq)
    
inc :: Num -> Double
inc x = x + 1

main = print $ inc (Num 1)