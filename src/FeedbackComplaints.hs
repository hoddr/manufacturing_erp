{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: FeedbackComplaints
Description: Logging external feedback complaints.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides feedback complaint functions for tracking external, customer-related errors that WERE reported by customers. Similar to error log entries with minor tweaks.

-}

module FeedbackComplaints
  ( addFeedbackComplaint
  , deleteFeedbackComplaint
  , editFeedbackComplaint
  , listFeedbackComplaints
  )
  where

import Database.PostgreSQL.Simple ( Query )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , Context(..)
                , Customer(..)
                , FeedbackComplaint(..)
                , QueryPag(..)
                , User(..)
                )
import Customers ( baseCustomerQuery )
import Database ( runExecute
                , runQuery
                )
import Mail ( emailFeedbackAssignment )
import Utils ( badReq
             , gen
             , passOnError
             , passSuccess
             )


-- FUNCTIONS --
-- | Adds new feedback complaint entry. Emails a notice to the assigned user.
addFeedbackComplaint :: Context -> FeedbackComplaint -> IO (Either APIError FeedbackComplaint)
addFeedbackComplaint c fc = runQuery c insertSql ( fc_orderNumber fc
                                                 , cid $ fc_customer fc
                                                 , fc_occurrenceDate fc
                                                 , fc_issueNote fc
                                                 , fc_resolutionNote fc
                                                 , fc_isResolved fc
                                                 , fc_isAppUpdated fc
                                                 , fc_isRecordedIn90 fc
                                                 , fc_randdCost fc
                                                 , fc_israndd fc
                                                 , userid $ fc_user fc
                                                 )
  >>= either passOnError (gen (\f nid -> f { fc_id = nid })
                              "Failure to return new feedback complaint id"
                              fc
                         )
  >>= either passOnError (\fcnew ->
    emailFeedbackAssignment (userusername (fc_user fcnew) <> "@erp.com")
                            (fc_id fcnew)
    >>= either passOnError (\_ -> passSuccess fcnew)
                         )

-- | Deletes feedback complaint specified by id ('Int').
deleteFeedbackComplaint :: Context -> Int -> IO (Either APIError ())
deleteFeedbackComplaint c fcid = runExecute c deleteSql [fcid]
  >>= either passOnError (\_ -> passSuccess ())

-- | Edits the feedback complaint entry specified by id ('Int'). See source for editable fields.
editFeedbackComplaint :: Context -> FeedbackComplaint -> Int -> IO (Either APIError FeedbackComplaint)
editFeedbackComplaint c fc fcid
  | fcid /= fc_id fc = badReq 400 "Feedback complaint uri id and body id mismatch"
  | otherwise = runExecute c updateSql ( fc_orderNumber fc
                                       , cid $ fc_customer fc
                                       , fc_occurrenceDate fc
                                       , fc_issueNote fc
                                       , fc_resolutionNote fc
                                       , fc_isResolved fc
                                       , fc_isAppUpdated fc
                                       , fc_isRecordedIn90 fc
                                       , fc_randdCost fc
                                       , fc_israndd fc
                                       , userid $ fc_user fc
                                       , fc_id fc
                                       )
    >>= either passOnError (\_ -> passSuccess fc)

-- | List feedback complaint entries within bounds of querystring search and/or pagination.
-- Will return an empty list for no results.
listFeedbackComplaints :: Context -> QueryPag -> IO (Either APIError [FeedbackComplaint])
listFeedbackComplaints ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listFeedbackComplaints ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, s, s, l, o)


-- SQL --
-- | Base SQL string for retrieving entries from database.
baseSql :: Query
baseSql = "select \
          \feedback_complaints.id as id, \
          \feedback_complaints.order_number as orderNumber, "
       <> baseCustomerQuery -- this doesn't contain comma at end, needed here
       <> ", \
          \feedback_complaints.entry_date as entryDate, \
          \feedback_complaints.occurrence_date as occurrenceDate, \
          \feedback_complaints.issue_note as issueNote, \
          \feedback_complaints.resolution_note as resolutionNote, \
          \feedback_complaints.is_resolved as isResolved, \
          \feedback_complaints.is_app_updated as isAppUpdated, \
          \feedback_complaints.is_recorded_in_90 as isRecordedIn90, \
          \feedback_complaints.randd_cost as rdCost, \
          \feedback_complaints.is_randd as isRandd, \
          \users.id as userId, \
          \users.fname as first, \
          \users.lname as last, \
          \users.username as username, \
          \users.password as password, \
          \users.role as role \
          \from feedback_complaints \
          \inner join customers \
          \on feedback_complaints.fk_customer_id = customers.id \
          \inner join users \
          \on feedback_complaints.fk_user_id = users.id"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by entry_date desc"

-- | Base insertion SQL. Check database for any default values not specified here.
insertSql :: Query
insertSql = "insert into feedback_complaints (\
            \order_number, \
            \fk_customer_id, \
            \occurrence_date, \
            \issue_note, \
            \resolution_note, \
            \is_resolved, \
            \is_app_updated, \
            \is_recorded_in_90, \
            \randd_cost, \
            \is_randd, \
            \fk_user_id \
            \) values \
            \(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Naive search for matching, case-sensitive strings.
searchSql :: Query
searchSql = " where \
            \feedback_complaints.order_number like ? or \
            \customers.name like ? or \
            \users.fname like ? or \
            \users.lname like ?"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update feedback_complaints set \
            \order_number = ?, \
            \fk_customer_id = ?, \
            \occurrence_date = ?, \
            \issue_note = ?, \
            \resolution_note = ?, \
            \is_resolved = ?, \
            \is_app_updated = ?, \
            \is_recorded_in_90 = ?, \
            \randd_cost = ?, \
            \is_randd = ?, \
            \fk_user_id = ? \
            \where id = ?"

-- | Base delete SQL.
deleteSql :: Query
deleteSql = "delete from feedback_complaints where id = ?"

-- | Pagination SQL portion.
paginationSql :: Query
paginationSql = " limit ? offset ?"
