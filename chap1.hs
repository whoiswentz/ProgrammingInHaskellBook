double x = x + x

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b >= x]

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do 
    x <- act
    xs <- seqn acts
    return (x:xs)

-- Exercices

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs