module Assignment1 (subst, interleave, unroll) where


--Author: Lin Li (Nastul) <lili1@student.unimelb.edu.au>
--Purpose: practice our Haskell programming skills and write a few fairly simple Haskell functions.


--subst takes two values and a list, and replaces every occurrence of the first value with the second in the list.
--this function use conditional statement to determines whether the first input value matches the first element of the list,if matched use the second input value to replace the first element of the list 
subst :: Eq t => t -> t -> [t] -> [t]
subst first second list = if first == (head list)
then second:(tail list) 
else list



--takes two lists and returns the interleaving of the two arguments. That, the result is a list in which the first, third, fifth . . . elements come fromt the first argument and the second, fourth, sixth . . . come from second. If either argument is shorter than the other, the excess elements of the longer comprise the end of the resulting list.
--this function use recursion, we pick up the first element in both lists and merge them, then Let the function keep calling the subset after removing the first element until one of the lists becomes an empty set
interleave :: [t] -> [t] -> [t]
interleave list_a [] = list_a ++ []
interleave [] list_b = []  ++ list_b
interleave (head_a:as) (head_b:bs) =  head_a : [head_b] ++ interleave as bs


--takes a list and an integer and constructs a list of the specified length made up by “unrolling” the input list as many times as needed to construct a list of that length. That is, the output consists of the input list repeated as many times as necessary to have the specified length. 
--repeatList function helps unroll output the complete list, input the number of times we need to repeat, and the list
--for unroll function, if the input number is higher than the length of list, we just use take function to output the list, otherwise,
--The input number divided by the length of the list, we will get the number of times that we have to repeat the entire list, then Take the modulus of the input number and the length of the list
--we will get the number of additional elements required, use take function to add all this elements.
unroll :: Int -> [a] -> [a]
unroll 0 list = []
unroll number list = if number <= length list then take number list else repeatList ((number `div` (length list))-1) list ++ take (number `mod` (length list)) list
repeatList 0 list = list
repeatList num list = list ++ (repeatList (num - 1) list)








