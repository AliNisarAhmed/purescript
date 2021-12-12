module Ch7a where

import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (>=), (<>))

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "------------------------------------"
  log $ show $ (Left "left" :: Either _ Unit)
  log $ show $ (Right (Just 42) :: Either Unit _)

data Maybe a
  = Nothing
  | Just a

-- instance maybeEquality :: Eq a => Eq (Maybe a) where
--   eq Nothing Nothing = true
--   eq (Just a) (Just b) = a == b
--   eq _ _ = false

-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare (Just x) (Just y) = compare x y
--   compare Nothing _ = LT
--   compare _ Nothing = GT

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y =
  case compare x y of
    LT -> false
    _ -> true

infixl 4 greaterThanOrEq as >=

----- SHOW

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show (Just x) = "Just " <> show x
--   show Nothing = "Nothing"

------ DERIVING Auto

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance eqOrd :: Ord a => Ord (Maybe a)

derive instance genericMaybe ::Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow