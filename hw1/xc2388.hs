main = do
      print (take 10 fibs)
      print (take 10 primes)
      print (take 10 partC)
      print (take 10 partD)

   

fibs = 1:1:zipWith (+) fibs (tail fibs)

primes= sieve [2..]
      where sieve(p:xs)= p:sieve[x|x<-xs, x `mod` p >0]

fibT=zip [1..] fibs
primeT=zip [1..] primes

partC=select fibT primes

partD= select primeT fibs


select (x:xs) (y:ys)= if fst(x)==y 
                      then snd(x):select (x:xs) ys
                      else select xs (y:ys)
