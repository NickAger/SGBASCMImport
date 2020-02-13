{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BoatParkSpaces where

import qualified Data.Text as T
import GHC.Generics
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import qualified Control.Monad as CM

--

data ExportSummary = ExportSummary {
    lastname :: !T.Text
  , full_name :: !T.Text
  , dob :: !T.Text
  , email :: !T.Text
  , phone :: !T.Text
  , mobile :: !T.Text
  , street :: !T.Text -- address1
  , locality :: !T.Text -- address2
  , city :: !T.Text -- town
  , county :: !T.Text -- county
  , postcode :: !T.Text --  postcode  
  , membership_started :: !T.Text -- JOIN though its year so it will need to be put into dd/mm/yyyy
  , membership_number :: Int -- same number if part of same membership
  , membership_type :: !T.Text  -- "Joint", "Family Membership", "Single" - translate from "Typ"
  , membership_is_primary :: Bool
  , tags :: [T.Text]
  , boat_park_spaces :: [T.Text]
  , contact_id :: Int
} deriving (Generic, Show)


-- Export to SCM
data BoatSchemaFields = BoatSchemaFields {
      system_id :: !T.Text
    , name :: !T.Text
    , manufacturer :: !T.Text
    , keel :: !T.Text
    , construction :: !T.Text
    , colour :: !T.Text
    , nat_reg :: !T.Text
    , ssr :: !T.Text
    , insurance :: !T.Text
    , disc_number :: !T.Text
    , loa :: !T.Text
    , beam :: !T.Text
    , draught :: !T.Text
    , vessel_type :: !T.Text
    , usage :: !T.Text
    , status :: !T.Text
    , notes :: !T.Text
    , loa_units :: !T.Text
    , beam_units :: !T.Text
    , draught_units :: !T.Text
    , loa_f :: !T.Text
    , loa_i :: !T.Text
    , beam_f :: !T.Text
    , beam_i :: !T.Text
    , draught_f :: !T.Text
    , draught_i :: !T.Text
    , weight :: !T.Text
    , mast_height :: !T.Text
    , rig_type :: !T.Text
    , design_name :: !T.Text
    , mast_units :: !T.Text
    , mast_f :: !T.Text
    , mast_i :: !T.Text
    , sail_number :: !T.Text
    , sail_colour :: !T.Text
    , owner_name :: !T.Text
    , primary_name :: !T.Text
    , secondary_numbers :: !T.Text
    , irc_tcc :: !T.Text
    , irc_crew_number :: !T.Text
    , irc_cert_number :: !T.Text
    , portsmouth :: !T.Text
    , clyde_mph :: !T.Text
    , klass_name :: !T.Text
    , import_id :: !T.Text
    , contact_ids :: !T.Text
    , original_contact_ids :: !T.Text
    , location :: !T.Text
    , directions :: !T.Text
    , instructions :: !T.Text
} deriving (Generic, Show, Eq)
instance Csv.ToNamedRecord BoatSchemaFields
instance Csv.DefaultOrdered BoatSchemaFields

createSCMBoat :: T.Text ->  T.Text -> T.Text -> T.Text -> T.Text -> BoatSchemaFields
createSCMBoat mooringId boatName ownerName contactid description =
    BoatSchemaFields {
      system_id = ""
    , name = boatName
    , manufacturer = ""
    , keel = ""
    , construction = ""
    , colour = ""
    , nat_reg = ""
    , ssr = ""
    , insurance = ""
    , disc_number = ""
    , loa = ""
    , beam = ""
    , draught = ""
    , vessel_type = ""
    , usage = ""
    , status = ""
    , notes = description
    , loa_units = ""
    , beam_units = ""
    , draught_units = ""
    , loa_f = ""
    , loa_i = ""
    , beam_f = ""
    , beam_i = ""
    , draught_f = ""
    , draught_i = ""
    , weight = ""
    , mast_height = ""
    , rig_type = ""
    , design_name = ""
    , mast_units = ""
    , mast_f = ""
    , mast_i = ""
    , sail_number = ""
    , sail_colour = ""
    , owner_name = ownerName
    , primary_name = ""
    , secondary_numbers = ""
    , irc_tcc = ""
    , irc_crew_number = ""
    , irc_cert_number = ""
    , portsmouth = ""
    , clyde_mph = ""
    , klass_name = ""
    , import_id = boatName
    , contact_ids = ""
    , original_contact_ids = contactid
    , location = ""
    , directions = ""
    , instructions = ""
  }


-- Export to SCM
data MooringAllocationSchemaFields = MooringAllocationSchemaFields {
      membership_number :: Int
    , original_id :: !T.Text
    , title :: !T.Text
    , first_name :: !T.Text
    , last_name :: !T.Text
    , sail_number :: !T.Text
    , boat :: !T.Text
    , boat_original_id :: !T.Text
    , mooring_group :: !T.Text
    , mooring_space :: !T.Text
    , mooring_space_original_id :: !T.Text
    , from :: !T.Text
    , until :: !T.Text
    , mooring_price :: !T.Text
    , import_non_billable :: !T.Text
} deriving (Generic, Show, Eq)
instance Csv.ToNamedRecord MooringAllocationSchemaFields
instance Csv.DefaultOrdered MooringAllocationSchemaFields

-- Export to SCM
data MooringDefinitionSchemaFields = MooringDefinitionSchemaFields {
      id :: !T.Text
    , name :: !T.Text
    , group :: !T.Text
    , _type :: !T.Text
    , note :: !T.Text
    , price :: !T.Text
    , original_id :: !T.Text
} deriving (Generic, Show, Eq)
instance Csv.ToNamedRecord MooringDefinitionSchemaFields
instance Csv.DefaultOrdered MooringDefinitionSchemaFields

--

-- boats.csv export from memDB

data BoatsImportFields = BoatsImportFields {
    nameId :: !T.Text
  , membershipName :: !T.Text
  , boatInfo :: !T.Text
  , used :: !T.Text
  , available :: !T.Text
  , unused1 :: !T.Text
  , unused2 :: !T.Text
  , unused3 :: !T.Text
} deriving (Generic, Show, Eq)
instance Csv.FromRecord BoatsImportFields
instance Csv.DefaultOrdered BoatsImportFields

readBoatsCSVLines :: FilePath -> IO (V.Vector BoatsImportFields)
readBoatsCSVLines filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader {-Csv.NoHeader-} csvData  :: Either String (V.Vector BoatsImportFields) of
      Left err -> do
        error "failed to read file:"
        putStrLn err
        undefined
      Right v -> 
        return $ V.filter (\boat -> not $ T.null (nameId boat)) v

---

{-
Dinghy spaces:
D1 .. D60
-}
createDinghySpaces :: [MooringDefinitionSchemaFields]
createDinghySpaces =
  map createSpace [1..60]
  where
    createSpace :: Integer ->  MooringDefinitionSchemaFields
    createSpace i = 
      let 
        nameId = T.pack $ "D" ++ show i
      in
        MooringDefinitionSchemaFields {
            id = ""
          , name = nameId
          , group = "Dinghy Park Spaces"
          , _type  = "Hard standing"
          , note = ""
          , price = "Dinghy Park Space"
          , original_id = nameId
        }

{-
Quarry
R1 .. R10
-}
createQuarrySpaces :: [MooringDefinitionSchemaFields]
createQuarrySpaces =
  map createSpace [1..10]
  where
    createSpace :: Integer ->  MooringDefinitionSchemaFields
    createSpace i = 
      let 
        nameId = T.pack $ "R" ++ show i
      in
        MooringDefinitionSchemaFields {
            id = ""
          , name = nameId
          , group = "Quarry Spaces"
          , _type  = "Hard standing"
          , note = ""
          , price = "Dinghy Park Space"
          , original_id = nameId
        }
 
{-
Canoe racks:
K1 .. K50
-}
createCanoeRackSpaces :: [MooringDefinitionSchemaFields]
createCanoeRackSpaces =
  map createSpace [1..50]
  where
    createSpace :: Integer ->  MooringDefinitionSchemaFields
    createSpace i = 
      let 
        nameId = T.pack $ "K" ++ show i
      in
        MooringDefinitionSchemaFields {
            id = ""
          , name = nameId
          , group = "Canoe racks"
          , _type  = "Rack"
          , note = ""
          , price = "Canoe Rack"
          , original_id = nameId
        }

{-
Toast racks (for inflatables)
T1 .. T20
-}
createInflatableRackSpaces :: [MooringDefinitionSchemaFields]
createInflatableRackSpaces =
  map createSpace [1..20]
  where
    createSpace :: Integer ->  MooringDefinitionSchemaFields
    createSpace i = 
      let 
        nameId = T.pack $ "T" ++ show i
      in
        MooringDefinitionSchemaFields {
            id = ""
          , name = nameId
          , group = "Inflatable rack storage"
          , _type  = "Rack"
          , note = ""
          , price = "Inflatable Rack"
          , original_id = nameId
        }

---

lookupDescription :: V.Vector BoatsImportFields -> T.Text -> T.Text
lookupDescription boats space =
  let
    boat = V.find (\boat -> (nameId boat) == space) boats
  in
    boatInfo (fromJust boat)

exportBoats :: [BoatSchemaFields] -> IO ()
exportBoats boats = do
  let boatsCsv = Csv.encodeDefaultOrderedByName boats
  BL.writeFile "/Users/nickager/programming/SGBASCMImport/exportedData/exportedBoats.csv"  boatsCsv

exportMoorings :: [MooringAllocationSchemaFields] -> IO ()
exportMoorings moorings = do
  let mooringsCsv = Csv.encodeDefaultOrderedByName moorings
  BL.writeFile "/Users/nickager/programming/SGBASCMImport/exportedData/exportedMoorings.csv"  mooringsCsv


exportMemberBoats :: [ExportSummary] -> IO ()
exportMemberBoats members = do
  let membersWithBoats = filter (\member -> not $ null $ boat_park_spaces member) members
  boats <- readBoatsCSVLines "/Users/nickager/programming/SGBASCMImport/originalData/boats.csv"
  let boatsAllocationTupleArray =  membersWithBoats >>= (exportSpaces boats)
  let (scmBoats, scmMoorings) = unzip boatsAllocationTupleArray
  exportBoats scmBoats
  exportMoorings scmMoorings
  where
    exportSpaces :: V.Vector BoatsImportFields -> ExportSummary ->  [(BoatSchemaFields, MooringAllocationSchemaFields)]
    exportSpaces boats member  = 
      map (\(space, idx) -> exportBoat (T.unpack space) (lookupDescription boats space) idx member) $ zip (boat_park_spaces member) [1..]


exportBoat :: String -> T.Text -> Int -> ExportSummary -> (BoatSchemaFields, MooringAllocationSchemaFields)
exportBoat space@('D':_) = exportDinghy (T.pack space) "Dinghy Park Spaces" 
exportBoat space@('R':_) = exportDinghy (T.pack space) "Quarry Spaces" 
exportBoat space@('T':_) = exportInflatable (T.pack space) 
exportBoat space@('K':_) = exportCanoe (T.pack space) 
exportBoat _ = error "Unrecognised craft"

exportDinghy :: T.Text -> T.Text -> T.Text -> Int -> ExportSummary -> (BoatSchemaFields, MooringAllocationSchemaFields)
exportDinghy space group description idx member = 
  let
    dinghyName = (lastname member) `T.append`  "-Dinghy" `T.append`  (T.pack $ show idx)
    scmBoat = createSCMBoat space dinghyName (full_name member) (T.pack $ show $ contact_id member) description
    firstLast = T.breakOn " " (full_name member)
    mooringAllocation = 
      MooringAllocationSchemaFields {
          membership_number = (membership_number :: ExportSummary -> Int) member
        , original_id = space `T.append` "-" `T.append` (lastname member) 
        , title = ""
        , first_name = fst firstLast
        , last_name = snd firstLast
        , sail_number = ""
        , boat = dinghyName
        , boat_original_id = dinghyName
        , mooring_group = group
        , mooring_space = space
        , mooring_space_original_id = space
        , from = "2019-03-01"
        , until = "2020-02-28"
        , mooring_price = "Dinghy Park Space"
        , import_non_billable = ""
      }
    in
      (scmBoat, mooringAllocation)

exportCanoe :: T.Text -> T.Text -> Int -> ExportSummary -> (BoatSchemaFields, MooringAllocationSchemaFields)
exportCanoe space description idx member = 
  let
    canoeName = (lastname member) `T.append` "-Canoe" `T.append` (T.pack $ show idx)
    scmBoat = createSCMBoat space canoeName (full_name member) (T.pack $ show $contact_id member) description
    firstLast = T.breakOn " " (full_name member)
    mooringAllocation = 
      MooringAllocationSchemaFields {
          membership_number = (membership_number :: ExportSummary -> Int) member
        , original_id = space `T.append` "-" `T.append` (lastname member) 
        , title = ""
        , first_name = fst firstLast
        , last_name = snd firstLast
        , sail_number = ""
        , boat = canoeName
        , boat_original_id = canoeName
        , mooring_group = "Canoe racks"
        , mooring_space = space
        , mooring_space_original_id = space
        , from = "2019-03-01"
        , until = "2020-02-28"
        , mooring_price = "Canoe Rack"
        , import_non_billable = ""
      }
    in
      (scmBoat, mooringAllocation)

exportInflatable :: T.Text -> T.Text -> Int -> ExportSummary -> (BoatSchemaFields, MooringAllocationSchemaFields)
exportInflatable space description idx member = 
  let
    inflatableName = (lastname member) `T.append` "-Inflatable" `T.append` (T.pack $ show idx)
    scmBoat = createSCMBoat space inflatableName (full_name member) (T.pack $ show $contact_id member) description
    firstLast = T.breakOn " " (full_name member)
    mooringAllocation = 
      MooringAllocationSchemaFields {
          membership_number = (membership_number :: ExportSummary -> Int) member
        , original_id = space `T.append` "-" `T.append` (lastname member) 
        , title = ""
        , first_name = fst firstLast
        , last_name = snd firstLast
        , sail_number = ""
        , boat = inflatableName
        , boat_original_id = inflatableName
        , mooring_group = "Inflatable rack storage"
        , mooring_space = space
        , mooring_space_original_id = space
        , from = "2019-03-01"
        , until = "2020-02-28"
        , mooring_price = "Inflatable Rack"
        , import_non_billable = ""
      }
    in
      (scmBoat, mooringAllocation)

