module Ch6 where

import Data.Maybe
import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as StringUnicode
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.String.CodeUnits as String
import Type.Proxy (Proxy(..))

data Person = Person
  { name :: String,
  age :: Int,
  address :: Address
  }

data Company = Company
  {
  name :: String,
  address :: Address
  }

data Residence
  = Home Address
  | Facility Address

type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

person :: Person
person = Person
  { name: "Joe Mama"
  , age: 22
  , address:
    { street1: "123 Main Street"
    , street2: "Apt 152"
    , city: "Jamestown"
    , state: "CA"
    , zip: "95327"
    }
  }
company :: Company
company = Company
  { name: "Acme"
    , address:
    { street1: "987 Tesla Way"
    , street2: "Suite 101"
    , city: "Irvine"
    , state: "CA"
    , zip: "92602"
    }
  }
-- Buford, WY has population of 1
home :: Residence
home = Home
  { street1: "1 1st Street"
    , street2: "Apt 1"
    , city: "Buford"
    , state: "WY"
    , zip: "82052"
  }
facility :: Residence
facility = Facility
  { street1: "54321 Countdown Ave"
  , street2: ""
  , city: "Huntsville"
  , state: "AL"
  , zip: "35805"
  }

class HasAddress a where
  getAddress :: a -> Address

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company c) = c.address

instance hasAddressResidence :: HasAddress Residence where
  getAddress (Home address) = address
  getAddress (Facility address) = address

----------------------------------------------------------

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) =
    p1.name == p2.name && p1.age == p2.age && p1.address == p2.address

--------------------------------------------------------

data Place = First | Second | Third

instance eqPlace :: Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance ordPlace :: Ord Place where
  compare First First = EQ
  compare First _ = LT
  compare Second Second = EQ
  compare Second First = GT
  compare Second Third = LT
  compare Third Third = EQ
  compare Third _ = GT

derive instance genericPlace :: Generic Place _
instance showSomeType :: Show Place where
  show = genericShow

newtype FirstName = FirstName String
derive instance newTypeFirstName :: Newtype FirstName _

newtype LastName = LastName String
derive instance newTypeLastName :: Newtype LastName _

fullName :: FirstName -> LastName -> String
fullName first last = unwrap first <> " " <> unwrap last

derive instance eqFirstName :: Eq FirstName

newtype CEO = CEO Person
newtype Janitor = Janitor Person

derive instance newtypeCEO :: Newtype CEO _
derive instance newtypeJanitor :: Newtype Janitor _

-- instance hasAddressCEO :: HasAddress CEO where
--   getAddress (CEO p) = getAddress p

-- instance hasAddressJanitor :: HasAddress Janitor where
--   getAddress (Janitor p) = getAddress p

genericPersonHasAddress :: ∀ a. Newtype a Person => a -> Address
genericPersonHasAddress wrappedPerson =
  getAddress $ unwrap wrappedPerson

instance hasAddressCEO :: HasAddress CEO where
  getAddress = genericPersonHasAddress

instance hasAddressJanitor :: HasAddress Janitor where
  getAddress = genericPersonHasAddress

---- MULTI Parametric type classes

class Decapitate collection element where
  decapitate :: collection -> Maybe { head :: element, tail :: collection }

instance decapitateList :: Decapitate (List a) a where
  decapitate = List.uncons

-- Monomorphic instance in the second type parameter - Char
instance decapitateString :: Decapitate String Char where
  decapitate = String.uncons

-- Functional Dependency

instance decapitateStringUnicode :: Decapitate String CodePoint where
  decapitate = StringUnicode.uncons

-- COMPILER ERROR - as element type cannot be inferred
-- genericTail
--   :: ∀ collection element
--   . Decapitate collection element
--   => collection
--   -> Maybe collection
-- genericTail xs =
--   case decapitate xs of
--     Just { tail } -> Just tail
--     Nothing -> Nothing

genericTail1
  :: ∀ collection element
  . Decapitate collection element
  => element
  -> collection
  -> Maybe collection
genericTail1 _ xs =
  case (decapitate xs :: Maybe { head :: element, tail :: collection }) of
    Just { tail } -> Just tail
    Nothing -> Nothing

t :: Maybe String
t = genericTail1 'c' "abc"

tu :: Maybe String
tu = genericTail1 (codePointFromChar 'a') "abc"

--- solving the above using Proxy

genericTail2
  :: ∀ collection element
  . Decapitate collection element
  => Proxy element
  -> collection
  -> Maybe collection
genericTail2 _ xs =
  case (decapitate xs :: Maybe { head :: element, tail :: collection }) of
    Just { tail } -> Just tail
    Nothing -> Nothing

tx :: Maybe String
tx = genericTail2 (Proxy :: Proxy Char) "abc"

ty :: Maybe String
ty = genericTail2 (Proxy :: Proxy CodePoint) "xyz"


--- Functional Deps

class Decapitate2 collection element | collection -> element where
  decapitate2 :: collection -> Maybe { head :: element, tail :: collection }

genericTail3
  :: ∀ collection element
  . Decapitate2 collection element
  => collection
  -> Maybe collection
genericTail3 xs = case decapitate2 xs of
  Just { tail } -> Just tail
  Nothing -> Nothing

instance decapitateString2 :: Decapitate2 String Char where
  decapitate2 = String.uncons

-- COMPILER ERROR - OVERLAPPING INSTANCE
-- instance decapitateStringUnicode2 :: Decapitate2 String CodePoint where
--   decapitate2 = String.uncons


-- solution -> reverse the function deps

class Decapitate4 collection element | element -> collection where
  decapitate4 :: collection -> Maybe { head :: element, tail :: collection }

genericTail4
  :: ∀ collection element
  . Decapitate4 collection element
  => collection
  -> Maybe collection
genericTail4 xs = case (decapitate4 xs :: Maybe { head :: element, tail :: collection }) of
  Just { tail } -> Just tail
  Nothing -> Nothing

instance decapitateString4 :: Decapitate4 String Char where
  decapitate4 = String.uncons

instance decapitateStringUnicode4 :: Decapitate4 String CodePoint where
  decapitate4 = StringUnicode.uncons