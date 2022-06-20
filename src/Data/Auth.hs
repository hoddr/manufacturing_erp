{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Auth
Description: Data type declarations for authentication.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for authentication, including users, user permissions, and tokens.

-}

module Data.Auth
  ( NewUser(..)
  , PermissionSet(..)
  , Token(..)
  , User(..)
  , UserPermissions(..)
  )
  where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , object
                  , parseJSON
                  , toJSON
                  , withObject
                  , (.=)
                  , (.:)
                  )
import Data.Text ( Text )
import Data.Time ( UTCTime(..) )
import Data.UUID ( toText )
import Database.PostgreSQL.Simple.FromRow ( FromRow
                                          , field
                                          , fromRow
                                          )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )

-- LOCAL --
import Data.Utils ( liftM6 )

-- | User accounts. User permissions are mappings to the user's id.
data User = User { userid :: Int
                 , userfirst :: Text
                 , userlast :: Text
                 , userusername :: Text
                 , userpassword :: Text
                 , userrole :: Text
                 } deriving (Show)

instance ToJSON User where
  toJSON u = object [ "id" .= userid u
                    , "first" .= userfirst u
                    , "last" .= userlast u
                    , "username" .= userusername u
                    , "password" .= ("" :: Text)
                    , "role" .= userrole u
                    ]

instance FromJSON User where
  parseJSON = withObject "user" $ \u -> User
    <$> u .: "id"
    <*> u .: "first"
    <*> u .: "last"
    <*> u .: "username"
    <*> u .: "password"
    <*> u .: "role"

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow u = [ toField (userfirst u)
            , toField (userlast u)
            , toField (userusername u)
            , toField (userpassword u)
            , toField (userrole u)
            ]

-- | Temporary type for a new user.
newtype NewUser = NewUser User

instance Show NewUser where
  show (NewUser u) = show u

instance ToJSON NewUser where
  toJSON (NewUser u) = object [ "id" .= userid u
                              , "first" .= userfirst u
                              , "last" .= userlast u
                              , "username" .= userusername u
                              , "password" .= userpassword u
                              , "role" .= userrole u
                              ]

-- 0 = no permissions, 1 = read only, 2 = write only, 3 = r/w
-- | Wrapper to contain necessary permission level and 'UserPermissions' record lookup to retrieve necessary value.
newtype PermissionSet = PermissionSet (Int, UserPermissions -> Int)

-- | Mapping structure for all user permissions.
data UserPermissions = UserPermissions { up_id :: Int
                                       , up_userId :: Int
                                       , up_assemblies :: Int
                                       , up_customers :: Int
                                       , up_fabrication :: Int
                                       , up_feedbackComplaints :: Int
                                       , up_inventory :: Int
                                       , up_materials :: Int
                                       , up_orders :: Int
                                       , up_pricing :: Int
                                       , up_pricingLists :: Int
                                       , up_projects :: Int
                                       , up_purchase :: Int
                                       , up_quotes :: Int
                                       , up_users :: Int
                                       , up_vendors :: Int
                                       , up_po :: Int
                                       , up_errLog :: Int
                                       , up_curbCo :: Int
                                       } deriving (Show)

instance ToJSON UserPermissions where
  toJSON up = object  [ "id" .= up_id up
                      , "userId" .= up_userId up
                      , "assemblies" .= up_assemblies up
                      , "customers" .= up_customers up
                      , "fabrication" .= up_fabrication up
                      , "feedbackComplaints" .= up_feedbackComplaints up
                      , "inventory" .= up_inventory up
                      , "materials" .= up_materials up
                      , "orders" .= up_orders up
                      , "pricing" .= up_pricing up
                      , "pricingLists" .= up_pricingLists up
                      , "projects" .= up_projects up
                      , "purchase" .= up_purchase up
                      , "quotes" .= up_quotes up
                      , "users" .= up_users up
                      , "vendors" .= up_vendors up
                      , "purchaseOrders" .= up_po up
                      , "errorLog" .= up_errLog up
                      , "curbCo" .= up_curbCo up
                      ]

instance FromJSON UserPermissions where
  parseJSON = withObject "userPermissions" $ \up -> UserPermissions
    <$> up .: "id"
    <*> up .: "userId"
    <*> up .: "assemblies"
    <*> up .: "customers"
    <*> up .: "fabrication"
    <*> up .: "feedbackComplaints"
    <*> up .: "inventory"
    <*> up .: "materials"
    <*> up .: "orders"
    <*> up .: "pricing"
    <*> up .: "pricingLists"
    <*> up .: "projects"
    <*> up .: "purchase"
    <*> up .: "quotes"
    <*> up .: "users"
    <*> up .: "vendors"
    <*> up .: "purchaseOrders"
    <*> up .: "errorLog"
    <*> up .: "curbCo"

instance FromRow UserPermissions where
  fromRow = UserPermissions <$> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field
                            <*> field

-- | User authentication token (Bearer token strategy).
data Token = Token { tknid :: Int
                   , tknuser :: User
                   , tknuuid :: Text
                   , tknexpires :: Maybe UTCTime
                   , tknisOneTime :: Bool
                   } deriving (Show)

instance ToJSON Token where
  toJSON t = object [ "id" .= tknid t
                    , "user" .= toJSON (tknuser t)
                    , "token" .= tknuuid t
                    , "expires" .= tknexpires t
                    , "isOneTime" .= tknisOneTime t
                    ]

instance FromJSON Token where
  parseJSON = withObject "token" $ \t -> do
    tid <- t .: "id"
    u <- t .: "user"
    user <- parseJSON u
    tk <- t .: "token"
    e <- t .: "expires"
    oneTime <- t .: "isOneTime"
    pure $ Token { tknid = tid
                 , tknuser = user
                 , tknuuid = tk
                 , tknexpires = e
                 , tknisOneTime = oneTime
                 }

instance ToRow Token where
  toRow t = [ toField (userid $ tknuser t) ]

instance FromRow Token where
  fromRow = Token <$> field
                  <*> liftM6 User field field field field field field
                  <*> fmap toText field
                  <*> field
                  <*> field
