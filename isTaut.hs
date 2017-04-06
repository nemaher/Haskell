module Tautology where
import  System.IO

data Prop =  Const Bool --This sets up the prop by looking for spacific statements and recursivly calling itself untill it gets to a single var Char
          | Var Char
          | Not Prop
          | Or Prop Prop
          | And Prop Prop
          | Imply Prop Prop
          deriving (Show, Read) --reads in the input from main

type Assoc key value = [(key, value)] --Associates a key such as out prop chars with a value such as our bools
type Subst = Assoc Char Bool --this is where it is setting the subset up with an associtive pair of char and bool

find :: Eq key => key -> Assoc key value -> value
find key top = head [value | (k, value) <- top, k == key] --returning the head of the first Assoc v, (True or False)

eval :: Subst -> Prop -> Bool --brings in the prop to identify what line to evaluate and the bool of the subset
eval _ (Const base) = base --base case where it finds out what the value of the bool is 
eval s (Var x) = find x s --base case where finds the left variable of the map tree (x - Predicate s - Structure)
eval s (Not p) = not (eval s p) -- next four lines evaluate what the bools depending on the prop
eval s (Or p q) = eval s p || eval s q 
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char] --takes in the prop to set up the list of chars that are in our l
vars (Const _) = [] 
vars (Var x) = [x] 
vars (Not prop) = vars prop
vars (And prop1 prop2) = vars prop1 ++ vars prop2 --appends the left and right side of prop into a list of chars
vars (Or prop1 prop2) = vars prop1 ++ vars prop2
vars (Imply prop1 prop2) = vars prop1 ++ vars prop2

charList :: Eq a => [a] -> [a] --binary operator and list of the chars
charList [] = []
charList (x:xs) = x : charList (filter (/=x) xs) --filters out duplicate values in the char list

--sets up the tree
bools :: Int -> [[Bool]] --makes a list of lists of the bools in the truth table
bools 0 = [[]] --base case once all bools have been evaluated
bools n = map (False:) previous ++ map (True:) previous -- setting each bool to both true and false in the map to 2^n difrent combinations.
  where previous = bools (n - 1) --subtracting one from the total size to get previous bool

substitution :: Prop -> [Subst]
substitution prop = map (zip vs) (bools (length vs)) --map sets up the tree with the zip pair of all the bools posible in our tautology
  where vs = charList (vars prop) --removes duplicate chars in our tree

isTaut :: Prop -> String
isTaut p = outTaut(and [eval s p | s <- substitution p])

outTaut :: Bool -> String
outTaut True = "This is a Tautology"
outTaut False = "This is not a Tautology"

main :: IO()
main =
 do putStrLn "Enter your tautology prop number to check: "
    input <- getLine
    putStrLn (isTaut(read input))
    continue <- yesno "Continue?"
    if continue then main else return()

yesno :: String -> IO Bool
yesno prompt = do
          putStr $ prompt ++ " y/n: "
          hFlush stdout
          str <- getLine
          case str of
            "y" -> return True
            "n" -> return False
            _   -> do
              putStrLn "Invalid input."
              yesno prompt

--Problem Test Cases

p1 :: Prop
p1 = And (Var 'P') (Not (Var 'P'))

p2 :: Prop
p2 = Or (Var 'P') (Not (Var 'P'))

p3 :: Prop
p3 = Not (Or (Var 'P') (Var 'Q'))

p4 :: Prop
p4 = Imply (And (Var 'P') (Var 'Q')) (Var 'P')

p5 :: Prop
p5 = Imply (Var 'P') (And (Var 'P') (Var 'Q'))

p6 :: Prop
p6 = Imply (And (Var 'P') (Imply (Var 'P') (Var 'Q'))) (Var 'Q')

p7 :: Prop
p7 = Imply (Var 'P') (Imply (Or (Var 'Q') (Var 'R')) (Not (Var 'P')))

--you can even do Logical Equivalents!
--de morgan's isTaut p8 == isTaut p9
p8 :: Prop
p8 = Not (And (Var 'P') (Var 'Q'))

p9 :: Prop
p9 = Or (Not (Var 'P')) (Not (Var 'Q'))

--Distributive Law
--isTaut p10 == isTaut p11
p10 :: Prop
p10 = And (Var 'P') (Or (Var 'Q') (Var 'R'))

p11 :: Prop
p11 = Or (And (Var 'P') (Var 'Q')) (And (Var 'P') (Var 'R'))

--Idempotent Laws
--isTaut p12 == isTaut p14
--isTaut p13 == isTaut p14
p12 :: Prop
p12 = And (Var 'P') (Var 'P')

p13 :: Prop
p13 = And (Var 'P') (Var 'P')

p14 :: Prop
p14 = (Var 'P')

--also you can do constant bools
p15 :: Prop
p15 = Imply (Const True) (Const False)

p16 :: Prop
p16 = Imply (Const False) (Const True)

p17 :: Prop
p17 = Not (And (Imply (Const True) (Const False)) (Or (Const False) (Const True)))

