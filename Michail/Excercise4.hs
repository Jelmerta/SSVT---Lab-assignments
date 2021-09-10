import Lab1

revPrimes = [x | x <- [1..10000], prime x && prime (reversal x)]

