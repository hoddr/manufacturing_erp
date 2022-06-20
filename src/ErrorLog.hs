{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Error Log
Description: Logging company errors.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides error log functions for tracking internal, ERP-related errors that were not reported by customers. Similar to feedback complaints logic with minor tweaks.

-}

module ErrorLog
  ( -- * Functions
    addErrorLogEntry
  , deleteErrorLogEntry
  , editErrorLogEntry
  , listErrorLogEntries
  )
  where

import Database.PostgreSQL.Simple ( Query )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , Context(..)
                , Customer(..)
                , ErrorLogEntry(..)
                , QueryPag(..)
                , User(..)
                )
import Customers ( baseCustomerQuery )
import Database ( runExecute
                , runQuery
                )
import Utils ( badReq
             , gen
             , passOnError
             , passSuccess
             )


-- FUNCTIONS --
-- | Adds new error log entry.
addErrorLogEntry :: Context -> ErrorLogEntry -> IO (Either APIError ErrorLogEntry)
addErrorLogEntry ctx el = runQuery ctx insertSql el
  >>= either passOnError (gen (\nel nid -> nel { elog_id = nid })
                              "Failure to return new error log entry id"
                              el)

-- | Deletes error log entry specified by id ('Int').
deleteErrorLogEntry :: Context -> Int -> IO (Either APIError ())
deleteErrorLogEntry ctx elid = runExecute ctx deleteSql [elid]
  >>= either passOnError (\_ -> passSuccess ())

-- | Edits the error log entry specified by id ('Int'). See source for editable fields.
editErrorLogEntry :: Context -> Int -> ErrorLogEntry -> IO (Either APIError ErrorLogEntry)
editErrorLogEntry ctx elid el
  | elid /= elog_id el = badReq 400 "Error log entry uri id and body id mismatch"
  | otherwise = runExecute ctx updateSql ( elog_orderNumber el
                                         , cid $ elog_customer el
                                         , elog_occurrenceDate el
                                         , elog_issueNote el
                                         , elog_resolutionNote el
                                         , elog_rootCauseNote el
                                         , elog_code el
                                         , elog_assignedTo el
                                         , elog_isResolved el
                                         , elog_isRecordedIn90 el
                                         , elog_randdCost el
                                         , elog_isRandd el
                                         , userid $ elog_reportedBy el
                                         , elog_id el
                                         )
    >>= either passOnError (\_ -> passSuccess el)

-- | List error log entries within bounds of querystring search and/or pagination.
-- Will return an empty list for no results.
listErrorLogEntries :: Context -> QueryPag -> IO (Either APIError [ErrorLogEntry])
listErrorLogEntries ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listErrorLogEntries ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, s, s, s, s, l, o)


-- SQL --
-- | Base SQL string for retrieving entries from database.
baseSql :: Query
baseSql = "select \
          \error_log.id as id, \
          \error_log.order_number as orderNumber, "
       <> baseCustomerQuery -- this doesn't contain comma at end, needed here
       <> ", \
          \error_log.entry_date as entryDate, \
          \error_log.occurrence_date as occurrenceDate, \
          \error_log.issue_note as issueNote, \
          \error_log.resolution_note as resolutionNote, \
          \error_log.root_cause as rootCauseNote, \
          \error_log.code as code, \
          \error_log.assigned_to as assignedTo, \
          \error_log.is_resolved as isResolved, \
          \error_log.is_recorded_in_90 as isRecordedIn90, \
          \error_log.randd_cost as randdCost, \
          \error_log.is_randd as isRandd, \
          \users.id as userId, \
          \users.fname as first, \
          \users.lname as last, \
          \users.username as username, \
          \users.password as password, \
          \users.role as role \
          \from error_log \
          \inner join customers \
          \on error_log.fk_customer_id = customers.id \
          \inner join users \
          \on error_log.fk_user_id = users.id"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by entry_date desc"

-- | Base insertion SQL. Check database for any default values not specified here.
insertSql :: Query
insertSql = "insert into error_log (\
            \order_number, \
            \fk_customer_id, \
            \occurrence_date, \
            \issue_note, \
            \resolution_note, \
            \root_cause, \
            \code, \
            \assigned_to, \
            \is_resolved, \
            \is_recorded_in_90, \
            \randd_cost, \
            \is_randd, \
            \fk_user_id \
            \) values \
            \(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Naive search for matching, case-sensitive strings.
searchSql :: Query
searchSql = " where \
            \error_log.order_number like ? or \
            \customers.name like ? or \
            \users.fname like ? or \
            \users.lname like ? or \
            \error_log.code like ? or \
            \error_log.assigned_to like ?"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update error_log set \
            \order_number = ?, \
            \fk_customer_id = ?, \
            \occurrence_date = ?, \
            \issue_note = ?, \
            \resolution_note = ?, \
            \root_cause = ?, \
            \code = ?, \
            \assigned_to = ?, \
            \is_resolved = ?, \
            \is_recorded_in_90 = ?, \
            \randd_cost = ?, \
            \is_randd = ?, \
            \fk_user_id = ? \
            \where id = ?"

-- | Base delete SQL.
deleteSql :: Query
deleteSql = "delete from error_log where id = ?"

-- | Pagination SQL portion.
paginationSql :: Query
paginationSql = " limit ? offset ?"
