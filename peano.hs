data Number = Zero | Successor Number deriving (Show)

eq :: Number -> Number -> Bool
eq Zero Zero = True
eq Zero (Successor _) = False
eq (Successor _) Zero = False
eq (Successor a) (Successor b) = eq a b

lt :: Number -> Number -> Bool
lt Zero Zero = False
lt Zero _ = True
lt _ Zero = False
lt (Successor a) (Successor b) = lt a b

gt :: Number -> Number -> Bool
gt Zero Zero = False
gt Zero _ = False
gt _ Zero = True
gt (Successor a) (Successor b) = gt a b

add :: Number -> Number -> Number
add Zero Zero = Zero
add Zero (Successor a) = Successor a
add (Successor a) Zero = (Successor a)
add (Successor a) (Successor b) = Successor(Successor(add a b))

sub :: Number -> Number -> Number
sub Zero _ = Zero
sub (Successor a) Zero = (Successor a)
sub (Successor a) (Successor b) = (sub a b)

mul :: Number -> Number -> Number
mul Zero _ = Zero
mul _ Zero = Zero
mul (Successor Zero) (Successor a) = Successor a
mul (Successor a) (Successor Zero) = Successor a
mul (Successor a) (Successor b) = rec (Successor a) (Successor b) (Successor b)
  where
  rec (Successor Zero) v r = r
  rec i v r = rec (sub i (Successor Zero)) v (add v r)

division :: Number -> Number -> Number
division Zero _ = Zero
division _ Zero = Zero
division (Successor a) (Successor Zero) = Successor a
division (Successor a) (Successor b) = rec (Successor a) (Successor b) (Zero)
  where
  rec a v r 
    | (lt a v) = r 
    | otherwise = (rec (sub a v) v (add (Successor Zero) r))

gcd' :: Number -> Number -> Number
gcd' Zero _ = Zero
gcd' _ Zero = Zero
gcd' (Successor Zero) (Successor a) = (Successor a) 
gcd' (Successor a) (Successor Zero) = (Successor a) 
gcd' (Successor a) (Successor b) = euclidan (Successor a) (Successor b)
  where 
  euclidan a b
    | eq a b = a
    | gt a b = euclidan (sub a b) b
    | otherwise = euclidan a (sub b a)

main = do
  print $ "eq"
  print $ eq Zero Zero
  print $ eq Zero (Successor Zero)
  print $ eq (Successor Zero) Zero
  print $ eq (Successor Zero) (Successor Zero)
  print $ eq (Successor (Successor Zero)) (Successor (Successor (Successor Zero)))
  print $ eq (Successor (Successor Zero)) (Successor (Successor Zero))

  print $ "lt"
  print $ lt Zero Zero
  print $ lt Zero (Successor Zero)
  print $ lt (Successor Zero) Zero
  print $ lt (Successor Zero) (Successor Zero)
  print $ lt (Successor (Successor Zero)) (Successor (Successor (Successor Zero)))
  print $ lt (Successor (Successor Zero)) (Successor (Successor Zero))

  print $ "gt"
  print $ gt Zero Zero
  print $ gt Zero (Successor Zero)
  print $ gt (Successor Zero) Zero
  print $ gt (Successor Zero) (Successor Zero)
  print $ gt (Successor (Successor Zero)) (Successor (Successor (Successor Zero)))
  print $ gt (Successor (Successor (Successor Zero))) (Successor (Successor Zero))

  print $ "add"
  print $ add Zero Zero
  print $ add (Successor Zero) Zero
  print $ add (Successor Zero) (Successor Zero) 
  print $ add (Successor Zero) (Successor (Successor (Zero)))
  print $ add (Successor (Successor (Successor (Zero)))) (Successor (Successor (Zero)))

  print $ "sub"
  print $ sub Zero Zero
  print $ sub (Successor Zero) Zero
  print $ sub (Successor Zero) (Successor Zero) 
  print $ sub (Successor Zero) (Successor (Successor (Zero)))
  print $ sub (Successor (Successor (Successor (Successor (Zero))))) (Successor (Successor (Zero)))

  print $ "mul"
  print $ mul Zero (Successor Zero)
  print $ mul (Successor Zero) (Successor Zero)
  print $ mul (Successor (Successor Zero)) (Successor Zero)
  print $ mul (Successor Zero) (Successor (Successor Zero))
  print $ mul (Successor (Successor (Successor Zero))) (Successor (Successor Zero))
  print $ mul (Successor (Successor Zero)) (Successor (Successor (Successor Zero)))
  print $ mul (Successor (Successor (Successor (Successor (Successor (Successor Zero)))))) (Successor (Successor (Successor Zero)))

  print $ "division"
  print $ division Zero (Successor Zero)
  print $ division (Successor Zero) (Successor Zero)
  print $ division (Successor (Successor Zero)) (Successor Zero)
  print $ division (Successor (Successor (Successor (Successor (Successor (Successor Zero)))))) (Successor (Successor (Successor Zero)))
  print $ division (Successor (Successor (Successor (Successor (Successor (Successor (Successor (Successor Zero)))))))) (Successor (Successor Zero))

  print $ "gcd"
  print $ gcd' (Successor (Successor (Successor (Successor Zero)))) (Successor (Successor Zero))
  print $ gcd' (Successor (Successor (Successor (Successor Zero)))) (Successor (Successor (Successor (Successor (Successor (Successor (Successor (Successor Zero))))))))

