sieve ([]) = []
sieve (x:xs) = x:sieve([ y | y <- xs, y `mod` x /= 0 ])

sieve' ([]) = []
sieve' (x:xs)
  | x*x >= last xs = (x:xs)
  | otherwise = x:sieve'([ y | y <- xs, y `mod` x /= 0])

sieve'' ([],n) = []
sieve'' (x:xs,n)
  | x*x >= n = (x:xs)
  | otherwise = x:sieve''([ y | y <- xs, y `mod` x /= 0],n)

primes_leq (n) = sieve([2..n])
primes_leq'(n) = sieve'([2..n])
primes_leq''(n) = sieve''([2..n],n)
