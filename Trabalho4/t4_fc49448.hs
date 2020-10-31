--Joao David n49448

module Descendencia  
( raiz
, juntar 
) where

--Utilizei uma string para identificar a pessoa em questao,
--e uma lista de Familia para facilmente adicionar um novo filho a essa pessoa
data Familia = Pessoa String [Familia]

instance Show Familia where  
    show = meuShow

instance Eq Familia where
    (==) = meuEquals

instance Ord Familia where
    (<=) = meuOrd


raiz :: String -> Familia
raiz nome = Pessoa nome []

juntar :: String -> String -> Familia -> Familia
juntar pai filho (Pessoa atual [])
    |pai == atual = Pessoa atual [raiz filho] --1
    |otherwise = Pessoa atual [] --2
juntar pai filho (Pessoa atual (x:xs))
    |pai == atual = Pessoa atual ([raiz filho] ++ (x:xs)) --3
    |otherwise = if (numElemNovo == numElemAtual) then addNovo2 else addNovo --4
    where
    addNovo = Pessoa atual ([juntar pai filho x]++ xs) --vai tentar juntar no primeiro filho da lista atual
    addNovo2 = Pessoa atual ([x]++(juntarAux pai filho xs)) --vai tentar juntar nos outros filhos da lista atual
    numElemNovo = numElem addNovo
    numElemAtual = numElem (Pessoa atual (x:xs))

juntarAux :: String -> String -> [Familia] -> [Familia]
juntarAux pai filho [] = []
juntarAux pai filho (x:xs) = [juntar pai filho x] ++ juntarAux pai filho xs


meuShow :: Familia -> String
meuShow fam = init (toString fam 0)

meuEquals :: Familia -> Familia -> Bool
meuEquals famA famB = (numElem famA == numElem famB) && (numGera famA == numGera famB)

meuOrd :: Familia -> Familia -> Bool
meuOrd famA famB = (numGera famA <= numGera famB) && (numElem famA <= numElem famB)


toString :: Familia -> Int -> String
toString (Pessoa atual []) n = (daEspacosN n) ++ atual ++ "\n"
toString (Pessoa atual (x:xs)) n = (daEspacosN n) ++ atual ++ "\n" ++ (toString x (n+1)) ++ (toStringAux xs n)    

toStringAux :: [Familia] -> Int -> String
toStringAux (y:ys) n =  (toString y (n+1)) ++ (toStringAux ys n)
toStringAux [] n = ""

daEspacosN :: Int -> String
daEspacosN n = (replicate (n*2) ' ')


numGera :: Familia -> Int
numGera (Pessoa _ []) = 1
numGera (Pessoa atual listaDesc) = 1 + (maximum $ map (numGera) listaDesc)    
    
numElem :: Familia -> Int
numElem (Pessoa _ []) = 1
numElem (Pessoa atual listaDesc) = 1 + (sum $ map (numElem) listaDesc)
