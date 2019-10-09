import Data.List

bzip::[a]->[b]->[(a,b)]
bzip _ [] = []
bzip [] _ = []
bzip (x:xs) (y:ys) = (x,y): bzip xs ys

triple::Int -> [(Int, Int, Int)]
triple k = [(x, y, z) | x <- [1..k], y <- [1..k], z <- [1..k], x*x+y*y == z*z]

grid::Int->Int->[(Int, Int)]
grid a b = [(x,y) | x<-[0..a], y<-[0..b]]

square::Int->[(Int, Int)]
square a = [(x,y) | (x,y)<-grid a a, x/= y] 

primes::[Int]
primes = [x | x<-[2..], isPrime x]

isPrime::Int -> Bool
isPrime n = factors n == [1, n]

factors::Int->[Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

kthPrime::Int->Int
kthPrime k = myHead[y | (x, y)<-zip [1..] primes, x == k]

myHead::[a]->a
myHead [] = undefined
myHead (x:xs) = x

perfects::Int->[Int]
perfects n = [x | x<-[1..n], 2*x == sum(factors x)]

oddCountDown::Int->[Int]
oddCountDown k
    | k <= 0 = []
    | otherwise = helper k
        where
            helper::Int->[Int]
            helper k = (2*k-1):oddCountDown(k-1);

removeEven::[Int]->[Int]
removeEven xs = [y | (x,y)<-bzip [1..] xs, x`mod`2 == 1]

dropComposite::[Int]->[Int]
dropComposite xs = [y | (x,y)<-bzip [1..] xs, factors x == [1, x]]

myDrop::Int->[Int]->[Int]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (_:xs) = myDrop (n-1) xs

mergeSort::(Ord a)=>[a]->[a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort a = 
    merge (mergeSort l) (mergeSort r)
    where
        l = take ((length a)`div`2) a
        r = drop ((length a)`div`2) a

merge::(Ord a)=>[a]->[a]->[a]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
    | (a <= b) = a:(merge as (b:bs))
    | otherwise = b:(merge (a:as) bs)

quickSort::(Ord a)=>[a]->[a]
quickSort [] = []
quickSort (x:xs) = 
    let left = quickSort[a | a<-xs, a <= x]
        right = quickSort[a | a<-xs, a > x]
    in left ++ [x] ++ right

length'::[a]->Int
length' xs = sum [1 | _<-xs]

concat::[[a]]->[a]
concat xxs = [x | xs<-xxs, x<-xs]

firsts::[(a,b)]->[a]
firsts xs = [x | (x, _) <- xs]

myReverse::[a]->[a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myInsert::(Ord a)=>a->[a]->[a]
myInsert x [] = [x]
myInsert x (y:ys)
    | x <= y = x:y:ys
    |otherwise = y:(myInsert x ys)

myFind:: Eq a=>a->[(a,b)]->[b]
myFind a xs = [x | (a', x)<-xs, a'== a ]

divideByTen::(RealFloat a)=> a->a
divideByTen = (/10)

applyTwice::(a->a)->a->a
applyTwice f x = f (f x)

zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip'::(a->b->c)->(b->a->c)
flip' f x y = f y x

largestDivisible::(Integral a)=>a->a->a
largestDivisible n d = head(filter p [n, (n-1)..])
    where
        p x = x`mod`d == 0
        
----------------------------------------------------------------

data Shape = Circle Float | Rect Float Float
            deriving (Show)

shapeEg::[Shape]
shapeEg = [(Circle 0.5), (Rect 0.4 0.6)]

area::Shape->Float
area (Circle r) = 3.14*r*r
area (Rect l b) = l*b



main = putStrLn "Hello, World!"
