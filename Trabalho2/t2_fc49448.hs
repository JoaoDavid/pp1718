--Joao David n49448

--Ex1
areaTrapeziosFixos :: (Double -> Double) -> (Double, Double) -> Int -> Double
areaTrapeziosFixos f (x,y) n = subIntervalos f x n h 0
    where h = (y-x)/(fromIntegral n)

subIntervalos :: (Double -> Double) -> Double -> Int -> Double -> Int -> Double
subIntervalos f x n h i
    |i < n = (areaTrapezio (f x) (f (x+h)) h) + (subIntervalos f (x+h) n h (i+1))
    |otherwise = 0


--Ex2
areaTrapeziosVariaveis :: (Double -> Double) -> (Double, Double) -> Double -> Double
areaTrapeziosVariaveis f (x,y) e
    |trapG - trapPqs < e = trapPqs
    |otherwise = (areaTrapeziosVariaveis f (x,x+h) e) + (areaTrapeziosVariaveis f (x+h,y) e)
    where h = (y-x)/2
          trapG = areaTrapezio (f x) (f y) (y-x)
          trapPqs = (areaTrapezio (f x) (f (x+h)) h) + (areaTrapezio (f (x+h)) (f y) h)


areaTrapezio :: Double -> Double -> Double -> Double    
areaTrapezio a b h = ((a+b)*h)/2


