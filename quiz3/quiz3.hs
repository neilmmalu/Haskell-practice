
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



main = putStrLn "Hello, World!"