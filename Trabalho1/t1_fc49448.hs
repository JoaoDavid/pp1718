--Joao David n49448

--Soma dos algarismos de um numero ate 3 algarismos
somaAlgarismos :: Int -> Int
somaAlgarismos x = (mod x 10) + (mod (div x 10) 10) + (div x 100)

--Lista com todos os numeros inacreditaveis no intervalo [1,999]
inacreditaveis :: [Int]
inacreditaveis = [ x | x <- [1..999] , (mod x (somaAlgarismos x) == 0)]

--Lista com todos os numeros rarosInacreditaveis no intervalo [1,999]
rarosInacreditaveis :: [Int]
rarosInacreditaveis =  [ x | x <- inacreditaveis, eRaro x]

--Verifica se um dado numero e raro
eRaro :: Int -> Bool
eRaro x = length [ y | y <- [1..5], not ((elem (x-y) inacreditaveis) || (elem (x+y) inacreditaveis))] == 5


--Conjuncao Logica , devolve 1 caso seja true, 0 caso seja falso
f :: Bool -> Bool -> Integer
f x y = if x && y then 1 else 0

--Forma um tuplo a partir do input dado
g :: a -> b -> ([a], [b])
g x y = ([x], [y])

--True: o elemento dado e maior que todos os elementos da lista dada
h :: Ord t =>t -> [t] -> Bool
h x xs = (length [ y | y <- xs, x > y]) == length xs