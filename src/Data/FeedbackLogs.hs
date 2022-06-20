{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.FeedbackLogs
Description: Data type declarations for feedback complaints and error logs.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for feedback complaints and internal error logs.

-}

module Data.FeedbackLogs
  ( ErrorLogEntry(..)
  , FeedbackComplaint(..)
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
                  , (.:?)
                  )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Database.PostgreSQL.Simple.FromRow ( FromRow
                                          , field
                                          , fromRow
                                          )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )

-- LOCAL --
import Data.Auth ( User(..) )
import Data.CustomersVendors ( Customer(..) )
import Data.Utils ( liftM6
                  , liftM7
                  )

-- | Type to track external feedback complaints. Similar to 'ErrorLogEntry'.
data FeedbackComplaint = FeedbackComplaint { fc_id :: Int
                                           , fc_orderNumber :: Text
                                           , fc_customer :: Customer
                                           , fc_entryDate :: UTCTime
                                           , fc_occurrenceDate :: UTCTime
                                           , fc_issueNote :: Text
                                           , fc_resolutionNote :: Text
                                           , fc_isResolved :: Bool
                                           , fc_isAppUpdated :: Bool
                                           , fc_isRecordedIn90 :: Bool
                                           , fc_randdCost :: Maybe Double
                                           , fc_israndd :: Bool
                                           , fc_user :: User
                                           } deriving (Show)

instance ToJSON FeedbackComplaint where
  toJSON fc = object [ "id" .= fc_id fc
                     , "orderNumber" .= fc_orderNumber fc
                     , "customer" .= toJSON (fc_customer fc)
                     , "entryDate" .= fc_entryDate fc
                     , "occurrenceDate" .= fc_occurrenceDate fc
                     , "issueNote" .= fc_issueNote fc
                     , "resolutionNote" .= fc_resolutionNote fc
                     , "isResolved" .= fc_isResolved fc
                     , "isAppUpdated" .= fc_isAppUpdated fc
                     , "isRecordedIn90" .= fc_isRecordedIn90 fc
                     , "randdCost" .= fc_randdCost fc
                     , "isRandd" .= fc_israndd fc
                     , "assignedTo" .= toJSON (fc_user fc)
                     ]

instance FromJSON FeedbackComplaint where
  parseJSON = withObject "feedbackComplaint" $ \fc -> do
    fcid <- fc .:? "id"
    ordNum <- fc .: "orderNumber"
    ocustObj <- fc .: "customer"
    cust <- parseJSON ocustObj
    entryDate <- fc .: "entryDate"
    occurrenceDate <- fc .: "occurrenceDate"
    issueNote <- fc .: "issueNote"
    resolutionNote <- fc .: "resolutionNote"
    isResolved <- fc .: "isResolved"
    isAppUpdated <- fc .: "isAppUpdated"
    isRecordedIn90 <- fc .: "isRecordedIn90"
    rdcost <- fc .:? "randdCost"
    isRd <- fc .: "isRandd"
    userObj <- fc .: "assignedTo"
    user <- parseJSON userObj
    pure $ FeedbackComplaint { fc_id = fromMaybe 0 fcid
                             , fc_orderNumber = ordNum
                             , fc_customer = cust
                             , fc_entryDate = entryDate
                             , fc_occurrenceDate = occurrenceDate
                             , fc_issueNote = issueNote
                             , fc_resolutionNote = resolutionNote
                             , fc_isResolved = isResolved
                             , fc_isAppUpdated = isAppUpdated
                             , fc_isRecordedIn90 = isRecordedIn90
                             , fc_randdCost = rdcost
                             , fc_israndd = isRd
                             , fc_user = user
                             }

instance FromRow FeedbackComplaint where
  fromRow = FeedbackComplaint <$> field
                              <*> field
                              <*> liftM7 Customer field field field field field field field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> liftM6 User field field field field field field

-- ERROR LOG --
-- | Type for internal errors that need tracking/handling. Similar to 'FeedbackComplaint'.
data ErrorLogEntry = ErrorLogEntry { elog_id :: Int
                                   , elog_orderNumber :: Text
                                   , elog_customer :: Customer
                                   , elog_entryDate :: UTCTime
                                   , elog_occurrenceDate :: UTCTime
                                   , elog_issueNote :: Text
                                   , elog_resolutionNote :: Text
                                   , elog_rootCauseNote :: Text
                                   , elog_code :: Text
                                   , elog_assignedTo :: Text
                                   , elog_isResolved :: Bool
                                   , elog_isRecordedIn90 :: Bool
                                   , elog_randdCost :: Maybe Double
                                   , elog_isRandd :: Bool
                                   , elog_reportedBy :: User
                                   } deriving (Show)

instance ToJSON ErrorLogEntry where
  toJSON el = object [ "id" .= elog_id el
                     , "orderNumber" .= elog_orderNumber el
                     , "customer" .= toJSON (elog_customer el)
                     , "entryDate" .= elog_entryDate el
                     , "occurrenceDate" .= elog_occurrenceDate el
                     , "issueNote" .= elog_issueNote el
                     , "resolutionNote" .= elog_resolutionNote el
                     , "rootCauseNote" .= elog_rootCauseNote el
                     , "code" .= elog_code el
                     , "reportedBy" .= toJSON (elog_reportedBy el)
                     , "isResolved" .= elog_isResolved el
                     , "isRecordedIn90" .= elog_isRecordedIn90 el
                     , "randdCost" .= elog_randdCost el
                     , "isRandd" .= elog_isRandd el
                     , "assignedTo" .= elog_assignedTo el
                     ]

instance FromJSON ErrorLogEntry where
  parseJSON = withObject "errorLogEntry" $ \el -> do
    elid <- el .:? "id"
    on <- el .: "orderNumber"
    cobj <- el .: "customer"
    c <- parseJSON cobj
    ed <- el .: "entryDate"
    od <- el .: "occurrenceDate"
    issNote <- el .: "issueNote"
    resNote <- el .: "resolutionNote"
    rootNote <- el .: "rootCauseNote"
    code <- el .: "code"
    uobj <- el .: "reportedBy"
    rb <- parseJSON uobj
    isResolved <- el .: "isResolved"
    isRecorded <- el .: "isRecordedIn90"
    rdCost <- el .:? "randdCost"
    isRd <- el .: "isRandd"
    assignedTo <- el .: "assignedTo"
    pure $ ErrorLogEntry { elog_id = fromMaybe 0 elid
                         , elog_orderNumber = on
                         , elog_customer = c
                         , elog_entryDate = ed
                         , elog_occurrenceDate = od
                         , elog_issueNote = issNote
                         , elog_resolutionNote = resNote
                         , elog_rootCauseNote = rootNote
                         , elog_code = code
                         , elog_reportedBy = rb
                         , elog_isResolved = isResolved
                         , elog_isRecordedIn90 = isRecorded
                         , elog_randdCost = rdCost
                         , elog_isRandd = isRd
                         , elog_assignedTo = assignedTo
                         }

instance FromRow ErrorLogEntry where
  fromRow = ErrorLogEntry <$> field
                          <*> field
                          <*> liftM7 Customer field field field field field field field
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
                          <*> liftM6 User field field field field field field

instance ToRow ErrorLogEntry where
  toRow el = [ toField (elog_orderNumber el)
             , toField (cid $ elog_customer el)
             , toField (elog_occurrenceDate el)
             , toField (elog_issueNote el)
             , toField (elog_resolutionNote el)
             , toField (elog_rootCauseNote el)
             , toField (elog_code el)
             , toField (elog_assignedTo el)
             , toField (elog_isResolved el)
             , toField (elog_isRecordedIn90 el)
             , toField (elog_randdCost el)
             , toField (elog_isRandd el)
             , toField (userid $ elog_reportedBy el)
             ]
