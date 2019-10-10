--Creating custom functions using
--1_Recursion
--2_List comprehension
--3_Lamdas, higher order functions


reverse_1::[a]->[a]
reverse_1 [] = []
reverse_1 (x:xs) = reverse_1 xs ++ [x]

reverse_3::[a]->[a]
reverse_3 = foldr (\x xs -> xs ++ [x]) []

-------------------------------------------------

concat_1::[[a]]->[a]
concat_1 [] = []
concat_1 (xs:xxs) = xs ++ concat_1 xxs

concat_2::[[a]]->[a]
concat_2 xxs = [x | xs <- xxs, x <- xs]

-------------------------------------------------

zip_1::[a]->[b]->[(a,b)]
zip_1 _ [] = []
zip_1 [] _ = []
zip_1 (x:xs) (y:ys) = (x, y):zip_1 xs ys

zip_2::[a]->[b]->[(a,b)]
zip_2 xs ys = [(x,y) | (a,x)<-zip_1 [1..] xs, (b,y)<-zip_1 [1..] ys, a == b]

-------------------------------------------------

unzip_1::[(a,b)]->([a],[b])
unzip_1 [] = ([],[])
unzip_1 ((x,y):xs) = (x: fst rest, y: snd rest)
                    where
                        rest = unzip_1 xs

unzip_2::[(a,b)]->([a],[b])
unzip_2 xs = (as, bs)
            where
                as = [x | (x,y)<-xs]
                bs = [y | (x,y)<-xs]

-------------------------------------------------

drop_1::Int->[a]->[a]
drop_1 0 xs = xs
drop_1 _ [] = []
drop_1 n (x:xs) = drop_1 (n-1) xs

drop_2::Int->[a]->[a]
drop_2 n xs = [b | (a, b)<-zip_1 [1..] xs, a > n]

-------------------------------------------------

init_1::[a]->[a]
init_1 [] = []
init_1 [x] = []
init_1 (x:xs) = x:init_1 xs

init_2::[a]->[a]
init_2 xs = [x | (x,y)<- zip xs [1..], y /= length xs]

-------------------------------------------------

factors_2::Int->[Int]
factors_2 n = [x | x<-[1..n], n`mod`x == 0]

prime_2::Int->Bool
prime_2 n = factors_2 n == [1,n]

primes_2::Int->[Int]
primes_2 k = [x | x<-[1..k], prime_2 x]

-------------------------------------------------

map_1::(a->b)->[a]->[b]
map_1 _ [] = []
map_1 f (x:xs) = f x: map f xs

map_2::(a->b)->[a]->[b]
map_2 f xs = [f x | x <- xs]

-------------------------------------------------

filter_1::(a->Bool)->[a]->[a]
filter_1 _ [] = []
filter_1 p (x:xs)
    | p x       = x : filter_1 p xs
    | otherwise = filter_1 p xs

filter_2::(a->Bool)->[a]->[a]
filter_2 p xs = [x | x <- xs, p x == True]

-------------------------------------------------

iterate_1::(a -> a) -> a -> [a]
iterate_1 f x = x : iterate_1 f (f x)


main = putStrLn "Hello, World!"