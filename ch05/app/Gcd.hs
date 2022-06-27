import Flow

gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

main :: IO ()
main =
  undefined
