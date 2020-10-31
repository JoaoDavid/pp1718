--Joao David n49448

module DNA  
( dnaParaRna  
, estabilidadePrimer  
, dnaValido  
) where


dnaParaRna :: [Char] -> [Char]
dnaParaRna xs = reverse $ map baseComp xs
    where
        baseComp :: Char -> Char
        baseComp c
            | c == 'A' = 'U'
            | c == 'T' = 'A'
            | c == 'C' = 'G'
            | c == 'G' = 'C'
            | otherwise = '_'


estabilidadePrimer :: [Char] -> Double
estabilidadePrimer xs
    | (length xs) < 36 = (contaCG xs)/fromIntegral(length xs)
    | otherwise =  (contaCG (take 18 xs) + contaCG (take 18 (reverse xs)))/36
    where    
        contaCG :: [Char] -> Double
        contaCG xs = fromIntegral (length (filter (\ c -> elem c "CG") xs))



dnaValido :: [Char] -> Bool
dnaValido [] = False
dnaValido xs = foldl (\acc x -> acc && (elem x "ATCG")) True xs
