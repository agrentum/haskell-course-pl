divisors n = [ k | k <- [1..n], n `mod` k == 0 ]

is_prime n = length (divisors n) == 2

primes_leq n = [ p | p <- [1..n], is_prime p ]

pi n = length (primes_leq n)
