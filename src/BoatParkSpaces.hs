{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BoatParkSpaces where

import qualified Data.Text as T
import GHC.Generics
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

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
      membership_number :: !T.Text
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
          , group = "Quary Spaces"
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
