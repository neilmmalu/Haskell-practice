--Creating custom functions using
--1_Recursion
--2_List comprehension
--3_Lamdas, higher order functions


reverse_1::[a]->[a]
reverse_1 [] = []
reverse_1 (x:xs) = reverse_1 xs ++ [x]

reverse_2::[a]->[a]
reverse_2 xs = [xs !! (i-1) | i<-[(length xs), (length xs) - 1..1]]

reverse_3::[a]->[a]
reverse_3 = foldl (\xs x -> x:xs) []

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

unzip_3::[(a,b)]->([a],[b])
unzip_3 xs = (map fst xs, map snd xs)

-------------------------------------------------

drop_1::Int->[a]->[a]
drop_1 0 xs = xs
drop_1 _ [] = []
drop_1 n (x:xs) = drop_1 (n-1) xs

drop_2::Int->[a]->[a]
drop_2 n xs = [b | (a, b)<-zip_1 [1..] xs, a > n]

drop_3::Int->[a]->[a]
drop_3 n xs = map fst (filter(\(x,y) -> y > n) (zip xs [1..]))

-------------------------------------------------

init_1::[a]->[a]
init_1 [] = []
init_1 [x] = []
init_1 (x:xs) = x:init_1 xs

init_2::[a]->[a]
init_2 xs = [x | (x,y)<- zip xs [1..], y /= length xs]

init_3::[a]->[a]
init_3 xs = map fst (filter(\(x,y)->y/=length xs) (zip xs [1..]))

-------------------------------------------------

factors_2::Int->[Int]
factors_2 n = [x | x<-[1..n], n`mod`x == 0]

factors_3::Int->[Int]
factors_3 n = filter(\x -> n`mod`x == 0) [1..n]

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

map_3::(a->b)->[a]->[b]
map_3 f xs = foldr (\x acc -> f x: acc) [] xs 

-------------------------------------------------

filter_1::(a->Bool)->[a]->[a]
filter_1 _ [] = []
filter_1 p (x:xs)
    | p x       = x : filter_1 p xs
    | otherwise = filter_1 p xs

filter_2::(a->Bool)->[a]->[a]
filter_2 p xs = [x | x <- xs, p x == True]

filter_3::(a->Bool)->[a]->[a]
filter_3 p = foldr (\x acc -> if p x then x:acc else acc) []

-------------------------------------------------

takewhile_1::(a->Bool)->[a]->[a]
takewhile_1 _ [] = []
takewhile_1 p (x:xs)
    | p x       = x : takewhile_1 p xs
    | otherwise = []


takewhile_3::(a->Bool)->[a]->[a]
takewhile_3 p = foldr (\x acc -> if p x then x:acc else []) []
-------------------------------------------------

iterate_1::(a -> a) -> a -> [a]
iterate_1 f x = x : iterate_1 f (f x)


elem_3::(Eq a)=>a->[a]->Bool
elem_3 x xs = foldl (\acc y -> if x == y then True else acc) False xs

main = putStrLn "Hello, World!"