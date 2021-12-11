module Ch5
  ( (!!)
  , (#)
  , ($)
  , apply
  , applyFlipped
  , catMaybes
  , concat
  , const
  , drop
  , dropEnd
  , dropWhile
  , filter
  , findIndex
  , findLastIndex
  , flip
  , head
  , index
  , init
  , last
  , length
  , null
  , range
  , reverse
  , singleton
  , snoc
  , tail
  , take
  , takeEnd
  , takeWhile
  , test
  , uncons
  , unzip
  )
  where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ head (Nil :: List Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ takeEnd 3 (1:2:3:4:5:6:Nil)
  log $ show $ takeEnd 10 (1:Nil)

apply :: ∀ a b. (a -> b) -> a -> b
apply f a = f a

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped a f = f a

infixl 1 applyFlipped as #

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

const :: ∀ a b. a -> b -> a
const a _ = a

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (_y:ys) x = snoc ys x

length :: ∀ a. List a -> Int
length l = go 0 l
  where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_:xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x:_) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_:xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l
  where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a}
uncons Nil = Nothing
uncons (x:xs) = Just { head: x, tail: xs}

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ n | n < 0 = Nothing
index (x:_) 0 = Just x
index (_:xs) n = index xs (n - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex f list = go list 0
  where
  go :: List a -> Int -> Maybe Int
  go Nil _ = Nothing
  go (x:xs) idx
    | f x = Just idx
    | otherwise = go xs (idx + 1)

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex f list = go list 0 Nothing
  where
  go :: List a -> Int -> Maybe Int -> Maybe Int
  go Nil _ lastIndex = lastIndex
  go (x:xs) idx lastIndex
    | f x = go xs (idx + 1) (Just idx)
    | otherwise = go xs (idx + 1) lastIndex

reverse :: List ~> List
reverse list = go list Nil
  where
  go :: ∀ a. List a -> List a -> List a
  go Nil acc = acc
  go (x:xs) acc = go xs (x:acc)

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil:xs) = concat xs
concat ((y:ys):xs) = y : concat (ys:xs)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter f list = reverse $ go list Nil
  where
  go Nil acc = acc
  go (x:xs) acc = if f x then go xs (x:acc) else go xs acc

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : catMaybes xs

range :: Int -> Int -> List Int
range s e = go Nil e s
  where
    go acc start end
      | start == end = start : acc
      | otherwise = go (start : acc) (start + step) end
    step = if s < e then -1 else 1

take :: ∀ a. Int -> List a -> List a
take count = reverse <<< go Nil (max 0 count)
  where
  go acc _ Nil = acc
  go acc 0 _ = acc
  go acc n (x:xs) = go (x:acc) (n - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop n xs
  | n <= 0 = xs
drop n (_x:xs) = drop (n - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile f (x:xs) = if f x then x : takeWhile f xs else Nil

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile f (x:xs) = if f x then dropWhile f xs else xs

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n list = snd $ go list
  where
  go Nil = Tuple 0 Nil
  go (x:xs) =
    let
      Tuple len rem = go xs
    in
      if len < n
      then Tuple (len + 1) (x:rem)
      else Tuple len rem

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x:xs) = go xs
    # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x:xs) (y:ys) = Tuple x y : zip xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y: xys) =
  unzip xys
  # \(Tuple xs ys) -> Tuple (x:xs) (y:ys)