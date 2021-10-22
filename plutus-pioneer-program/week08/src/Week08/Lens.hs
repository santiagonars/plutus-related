{-# LANGUAGE TemplateHaskell #-}

module Week08.Lens where

import Control.Lens

newtype Company = Company {_staff :: [Person]} deriving Show -- a newtype wrapper around a list of persons
                  -- when dealing with lenses, it is conventional to call field names with a leading underscore

data Person  = Person -- a record type with two fields
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address {_city :: String} deriving Show -- a newtype wrapper around a string

alejandro, lars :: Person  -- define two people
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company -- define a company where the staff consists of these 2 persons
iohk = Company { _staff = [alejandro, lars] }

goTo :: String -> Company -> Company  -- function takes string of new city name to update Address for each person of a company
goTo there c = c {_staff = map movePerson (_staff c)} -- map movePerson over the list of the staff of the given company
  where
    movePerson p = p {_address = (_address p) {_city = there}} -- helper function takes the existing Address of person and changes the city name to `there`
                                                               -- this can become quite messy to update; optics help provide first class field accessors
makeLenses ''Company -- lenses library has this template haskell to automatically implement lenses
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company -- function implemention using lenses
goTo' there c = c & staff . each . address . city .~ there  -- Zoom into the staff, then reach each person into the staff, 
                                                            -- then each of the their address and then city, and the set it `there`
