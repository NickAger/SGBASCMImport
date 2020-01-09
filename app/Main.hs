{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.Text as T
import qualified System.Environment as Env
import qualified Data.Csv as Csv
import Data.Csv ((.!))
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Lib

-- Export from Excel
data ExistingMemberFields = ExistingMemberFields {
    one :: !T.Text -- "1"
  , name :: !T.Text -- "AGER, ADAMS"
  , join :: !T.Text -- "2018"
  , pay :: !T.Text -- "chq, HLM, OL"
  , typ :: !T.Text -- "J, S, F"
  , clubBoat :: !T.Text -- "1 or blank"
  , dp :: !T.Text -- "dinghy park; 1,2 or blank - number of dinghy park spaces paid for"
  , rackspace :: !T.Text -- "rack space; 1,2 or blank - number of rack spaces paid for"
  , locker :: !T.Text -- "1 or blank"
  , outboard :: !T.Text -- "outboard; 1 or blank"
  , blank1 :: !T.Text
  , dinghyParkAndRackLocation1 :: !T.Text -- D32, K18
  , dinghyParkAndRackLocation2 :: !T.Text -- D32, K18
  , dinghyParkAndRackLocation3 :: !T.Text -- D32, K18
  , dinghyParkAndRackLocation4 :: !T.Text -- D32, K18  only saw 3 being used ie 4, 5 and 6 where blank
  , dinghyParkAndRackLocation5 :: !T.Text -- D32, K18
  , dinghyParkAndRackLocation6 :: !T.Text -- D32, K18
  , blank2 :: !T.Text
  , pbReg :: !T.Text -- appeared to be blank, but comment says 1 - on SGBA register of power boat users
  , pb2 :: !T.Text -- 1 or blank "PB2 or equivalent"
  , pb2valid :: !T.Text -- A = certificate is listed, B = done course, C = claims to have PB2, D = Equivalent OK as helm, E = Equivalent for reg only
  , safetyBoard :: !T.Text -- 1 or blank  -- seems to be blank
  , firstAid :: !T.Text -- 1 or blank -- seems to be blank
  , blank3 :: !T.Text
  , adultNames :: !T.Text -- "Roger Acton"
  , blank4 :: !T.Text -- the whole column is "1" why??
  , childrensNames :: !T.Text -- "Sasha, Ted" (but lots missing) and differing spacing between sometimes "&", sometimes ",", sometimes a space
  , quantityOfChildren :: !T.Text -- 4, 3, 2, 1 or blank
  , locker :: !T.Text -- "blank or L9"
  , outboard :: !T.Text -- "OB" or "July '19"
  , flt1 :: !T.Text -- 1 or blank not sure what it means
  , duty2015 :: !T.Text -- "O", "OOC", "C", "CC", "H", "HH",  "O", "OO", "OOO", "OC"
  , duty2016 :: !T.Text -- "O", "OOC", "C", "CC", "H", "HH",  "O", "OO", "OOO", "OC"
  , duty2017 :: !T.Text --  as above
  , duty2018 :: !T.Text --  as above
  , duty2019 :: !T.Text --  as above
  , flt2 :: !T.Text -- 1 or blank not sure what it means
  , commentsAndSkills :: !T.Text -- mostly blank but "holiday parking", "Honorary Annual M."
  , address1 :: !T.Text
  , address2 :: !T.Text
  , town :: !T.Text
  , county :: !T.Text
  , postcode :: !T.Text
  , telephone :: !T.Text
  , mobile :: !T.Text
  , emailaddress :: !T.Text
  , dontTouch1 :: !T.Text
  , dontTouchQflt :: !T.Text 
  , dontTouchVolunteerFlt :: !T.Text
  , ood :: !T.Text -- 1 or blank
  , assOod :: !T.Text -- 1 or blank
  , helmCrew :: !T.Text -- 1 or blank
  , shoreSupport :: !T.Text -- 1 or blank
  , wpBoats :: !T.Text -- 1 or blank
  , wpFacilities :: !T.Text -- 1 or blank
} deriving (Generic, Show)

instance Csv.FromRecord ExistingMemberFields
instance Csv.DefaultOrdered ExistingMemberFields

-- Export for SCM
data ExportFields = ExportFields { 
    uid :: !T.Text -- Only used to identify existing records if reimporting exported data that has been updated.  This is the Unique ID created when a Contact is initially added or imported.
  , title :: !T.Text -- IGNORE; not in existing data
  , first_name :: !T.Text -- extract from Adult Names, Childrens names
  , middle_name :: !T.Text -- IGNORE; not in existing data
  , last_name :: !T.Text -- last_name; combine "name" and "Adult Names"
  , full_name :: !T.Text -- IGNORE: Alternative to first and last name in separate columns.  Will be split into first and last.
  , suffix :: !T.Text -- IGNORE; not in existing data
  , nickname :: !T.Text -- IGNORE; not in existing data
  , gender :: !T.Text -- Maually enter [Male|Female]
  , company_name :: !T.Text -- IGNORE
  , job_title :: !T.Text -- IGNORE
  , dob :: !T.Text -- "Used to work out age for age restricted activities or membership categories.", need DOB for junior members
  , dod :: !T.Text -- "Prevents inappropriate communications being sent by suppressing mailings to deceased contacts."
  , email :: !T.Text
  , primary_email :: !T.Text -- IGNORE
  , phone :: !T.Text 
  , mobile :: !T.Text
  , fax :: !T.Text -- IGNORE
  , url :: !T.Text -- IGNORE
  , twitter :: !T.Text -- IGNORE
  , facebook :: !T.Text -- IGNORE
  , skype :: !T.Text -- IGNORE
  , linkedin :: !T.Text -- IGNORE
  , street :: !T.Text -- address1
  , locality :: !T.Text -- address2
  , city :: !T.Text -- town
  , county :: !T.Text -- county
  , postcode :: !T.Text --  postcode
  , latitude :: !T.Text -- IGNORE
  , longitude :: !T.Text -- IGNORE
  , bank_account_name :: !T.Text -- IGNORE
  , bank_number :: !T.Text -- IGNORE
  , bank_sort :: !T.Text -- IGNORE
  , bank_name :: !T.Text -- IGNORE
  , bank_branch :: !T.Text -- IGNORE
  , membership_started :: !T.Text -- JOIN though its year so it will need to be put into dd/mm/yyyy
  , membership_ended :: !T.Text -- CHECK, not sure if we want to enter expired members????
  , membership_number :: !T.Text -- same number if part of same membership
  , membership_reference :: !T.Text -- ???
  , membership_type :: !T.Text  -- "Joint", "Family Membership", "Single" - translate from "Typ"
  , membership_type_original_id :: !T.Text -- IGNORE ???
  , pay_method :: !T.Text -- "Pay" ???
  , membership_frozen :: !T.Text -- IGNORE
  , mem_frozen_amount :: !T.Text -- IGNORE
  , membership_is_primary :: !T.Text -- [Y|N] ???
  , concession :: !T.Text -- IGNORE
  , boat :: !T.Text -- Boat Name use "AGER1" etc
  , boat_id :: !T.Text --   ??
  , boat_ids :: !T.Text -- ??
  , boat_type :: !T.Text -- IGNORE
  , boat_class :: !T.Text -- IGNORE
  , boat_model :: !T.Text -- IGNORE
  , boat_number :: !T.Text -- IGNORE (sail nunber)
  , boat_colour :: !T.Text -- IGNORE (Boat Hull Colour)
  , boat_tcc :: !T.Text -- IGNORE (Boat IRC/TCF/TCC)
  , boat_clyde_mph :: !T.Text -- IGNORE (Boat Clyde min/hr)
  , mooring :: !T.Text -- Might be dinghy park
  , mooring_until :: !T.Text -- start of next seasons membership???
  , rya_number :: !T.Text -- IGNORE
  , ya_number :: !T.Text -- IGNORE
  , original_id :: !T.Text -- ignore
  , content :: !T.Text -- ignore
  , summary :: !T.Text -- ignore
  , description :: !T.Text -- ignore
  , password :: !T.Text -- ignore
  , uuid :: !T.Text -- ignore
  , tag :: !T.Text -- "Committee Member/Safety boat assessed"  (but it only contains a single tag)
  , subs_paid_by :: !T.Text -- Expects Original ID of Contact (used when first importing data from legacy system).
  , invoices_paid_by :: !T.Text -- needs UID only when reimporting
  , affiliate_code :: !T.Text -- IGNORE
  , login_email :: !T.Text -- need to consider what we do with kids that don't have email addresses
  , ice_name :: !T.Text -- In Case of Emergency Name
  , ice_number :: !T.Text -- In Case of Emergency Number
  , medical_info :: !T.Text -- Medical / Dietary info
  } deriving (Generic, Show)
instance Csv.ToNamedRecord ExportFields
instance Csv.DefaultOrdered ExportFields


readCSVLines filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader {-Csv.NoHeader-} csvData  :: Either String (V.Vector ExistingMemberFields) of
      Left err -> do
        error "failed to read file:"
        putStrLn err
        undefined 
      Right v -> return v

main :: IO ()
main = do
    csvLines <- readCSVLines "/Users/nickager/programming/SGBASCMImport/originalData/memDB11Nov2019.csv"
    print csvLines
    return ()