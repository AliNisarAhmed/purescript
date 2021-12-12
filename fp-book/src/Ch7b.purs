module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Number.Approximate (neqApproximate)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  let person = Person
        { name: FullName "Ali Ahmed"
        , age: Age 23
        , occupation: Doctor
        }
  log $ show $ (toCSV person # fromCSV) == Just person


--- CSV TypeClass

newtype CSV = CSV String

derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String

derive instance newtypeFullname :: Newtype FullName _
derive newtype instance eqFullName :: Eq FullName

instance showFullName :: Show FullName where
  show (FullName name) = name

newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed

derive instance eqOccupation :: Eq Occupation

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }


derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

derive instance eqPerson :: Eq Person

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) =
    CSV $ show name <> "," <> show age <> "," <> show occupation

--------------------- FROM CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) =
    case (split (Pattern ",") str) of
      [name, age, occupation] ->
        case fromString age of
          Just age' ->
            case toOccupation occupation of
              Just occ' -> Just $ Person
                { name : FullName name
                , age : Age age'
                , occupation : occ'
                }
              Nothing -> Nothing
          Nothing -> Nothing
      _ -> Nothing

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing