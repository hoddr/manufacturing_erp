{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Database
Description: PostgreSQL.Simple wrapper.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides light wrapper functions for the postgresql database. Also provides helper function(s) related to database connections. A simple try wrapper is used to give better error info on unique/duplicate entries (see Postgresql error codes).

-}

module Database
  ( -- * Functions
    formatTestQuery
  , getConn
  , runExecute
  , runExecute_
  , runExecuteMany
  , runQuery
  , runQuery_
  , runReturning
  , transactionWrapper
  ) where

import Control.Exception ( try )
import Data.ByteString ( ByteString )
import Database.PostgreSQL.Simple ( Connection
                                  , ConnectInfo(..)
                                  , Query
                                  , SqlError(..)
                                  , connect
                                  , connectDatabase
                                  , connectHost
                                  , connectPort
                                  , connectUser
                                  , connectPassword
                                  , formatQuery
                                  , execute
                                  , execute_
                                  , executeMany
                                  , query
                                  , query_
                                  , returning
                                  , withTransaction
                                  )
import Database.PostgreSQL.Simple.FromRow ( FromRow )
import Database.PostgreSQL.Simple.ToRow ( ToRow )
import GHC.Int ( Int64 )
import System.Environment ( getEnv )

-- LOCAL IMPORTS --
import APITypes ( APIError
                , Context(..)
                , contextConn
                )
import Utils ( badReq
             , passSuccess
             )

-- FNS --
-- | Returns database connection info via the 'connect' function. Database port should come from env var at some point.
getConn :: IO Connection
getConn = do
  user <- getEnv "ERP_PB_DBUSER"
  pass <- getEnv "ERP_PB_DBPASS"
  db <- getEnv "ERP_PB_DBNAME"
  host <- getEnv "ERP_PB_DBHOST"
  connect $ ConnectInfo { connectHost = host
                        , connectPort = 5432
                        , connectUser = user
                        , connectPassword = pass
                        , connectDatabase = db
                        }

-- | Basic query with parameterized inputs permitted.
runQuery :: (ToRow q, FromRow r)
         => Context
         -> Query
         -> q
         -> IO (Either APIError [r])
runQuery c sql params = trySql $ query (contextConn c) sql params

-- | Basic query without parameterized inputs.
runQuery_ :: FromRow r
          => Context
          -> Query
          -> IO (Either APIError [r])
runQuery_ c sql = trySql $  query_ (contextConn c) sql

-- | Basic execute with parameterized inputs permitted.
runExecute :: ToRow q
           => Context
           -> Query
           -> q
           -> IO (Either APIError Int64)
runExecute c sql params = trySql $ execute (contextConn c) sql params

-- | Basic execute without paramaterized inputs.
runExecute_ :: Context
            -> Query
            -> IO (Either APIError Int64)
runExecute_ c sql = trySql $ execute_ (contextConn c) sql

-- | Used mainly for purchase orders. Sets a number of entries in single SQL statement. See usage(s) for example case.
runExecuteMany :: ToRow q
               => Context
               -> Query
               -> [a]
               -> (a -> q)
               -> IO (Either APIError Int64)
runExecuteMany c sql ins mapfn = trySql $ executeMany (contextConn c) sql (map mapfn ins)

-- | Basic insert SQL and returns new id ('Int').
runReturning :: (ToRow q, FromRow r)
             => Context
             -> Query
             -> [q]
             -> IO (Either APIError [r])
runReturning c sql params = trySql $ returning (contextConn c) sql params

-- | Helper to test full sql syntax generated.
formatTestQuery :: ToRow q
                => Context
                -> Query
                -> q
                -> IO ByteString
formatTestQuery c = formatQuery (contextConn c)

-- | Try wrapper that provides slightly better error message for specific SQL error code(s).
trySql :: IO a -> IO (Either APIError a)
trySql iofn = do
  res <- try iofn
  case res of
       Left (e :: SqlError) -> print e >> case sqlState e of
                                               "23505" -> badReq 400 "Unique key violation - check for duplicate entry"
                                               _ -> badReq 500 "Internal server error"
       Right x -> passSuccess x

-- | Simple wrapper to support transactions in multi-step database actions.
transactionWrapper :: Context -> IO a -> IO a
transactionWrapper c = withTransaction (contextConn c)
