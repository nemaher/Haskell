module Church where

type ChurchNat a = (a -> a) -> (a -> a)

churchNatToInt :: (Num a) => ChurchNat a -> a
churchNatToInt cn = cn (+1) 0

suc :: (ChurchNat t) -> (ChurchNat t)
suc = \n s z ->  s (n s z)

--add :: (ChurchNat t) -> (ChurchNat t) -> (ChurchNat t)
add = \n m -> (n suc) m

mult = \n m -> (n (add m)) zero

zero :: ChurchNat t
zero = \s z -> z    -- lamda \s.\z.z
                    -- :t zero (ask type)
one :: (a -> a) -> (a -> a)
one = \s z -> s z

two = add one one

four = mult two two

iden :: a -> a
iden = \ x -> x
