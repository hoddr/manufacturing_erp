{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Customers
Description: Customer management and markups.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related to customers and customer markups. Outside of fixed
pricing lists, customer specific markups are applied to each item after all other
markups are applied, effectively giving "tiered" pricing. Note that project-specific customer variants should match the base customer markup unless noted otherwise.

Note: customer type field has no real use at the moment beyond backwards compatability with QB.

-}

module Customers
  ( -- * Functions
    addCustomer
  , editCustomer
  , getCustomerById
  , getCustomerByName
  , listCustomers
  , setTaxExemptFlag
    -- * SQL
  , baseCustomerQuery
  )
  where

import Data.Text ( Text )
import Database.PostgreSQL.Simple ( Query )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg
                , Context(..)
                , Customer(..)
                , QueryPag(..)
                )
import Database ( runExecute
                , runQuery
                )
import Utils ( apiMsg204
             , badReq
             , eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds a new customer.
addCustomer :: Context -> Customer -> IO (Either APIError Customer)
addCustomer ctx c =
  runQuery ctx insertSql ( cname c
                         , ccompany c
                         , ctype c
                         , cmarkup c
                         , cisTaxExempt c
                         )
  >>= eitherPassErr (gen (\cst nid -> cst { cid = nid })
                              "Failure to return new customer id"
                              c
                    )

-- | Retrieves customer record by id ('Int'). Returns 404 'APIError' if not found.
getCustomerById :: Context -> Int -> IO (Either APIError Customer)
getCustomerById ctx custId =
  runQuery ctx (baseSql <> " where customers.id = ?" <> sortSql) [custId]
  >>= eitherPassErr (wasFound "Customer not found")

-- | Retrieves customer record by name ('Text'). Returns 404 'APIError' if not found.
getCustomerByName :: Context -> Text -> IO (Either APIError Customer)
getCustomerByName c n = runQuery c (baseSql <> " where customers.name = ?" <> sortSql) [n]
                  >>= eitherPassErr (wasFound "Customer not found")

-- | Edits the specified customer specified by id ('Int'). See source for fields that may be adjusted.
editCustomer :: Context -> Customer -> Int -> IO (Either APIError Customer)
editCustomer ctx c custid
  | cid c == custid = runExecute ctx updateSql ( cname c
                                               , ccompany c
                                               , ctype c
                                               , cmarkup c
                                               , cisTaxExempt c
                                               , cid c
                                               )
                      >>= eitherPassErr (\_ -> passSuccess c)
  | otherwise = badReq 400 "Customer uri id and body id mismatch"

-- | List customers within bounds of querystring search and/or pagination.
-- Will return an empty list for no results.
listCustomers :: Context -> QueryPag -> IO (Either APIError [Customer])
listCustomers ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listCustomers ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, s, l, o)

-- | Simple helper to toggle tax exempt flag.
setTaxExemptFlag :: Context -> Int -> Bool -> IO (Either APIError APIMsg)
setTaxExemptFlag ctx custId flag = runExecute ctx taxExemptFlag (flag, custId)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No Content")

-- SQL --
{-| Universal customer query lines for minimizing code replication. Note that the 1.00 returns a placeholder
for the CC customer markup/discount factors (from separate lookup table). This field is not needed except
within CC related features, so we simply use a quick lookup rather than complicating the table joins for
a number of other queries (e.g. quotes, orders, etc).

WARNING: If this changes, the corresponding SQL in CC source file must be updated as well.
-}
baseCustomerQuery :: Query
baseCustomerQuery = "customers.id as customerId, \
                    \customers.name as customerName, \
                    \customers.company as customerCompany, \
                    \customers.customer_type as customerType, \
                    \customers.markup as customerMarkup, \
                    \customers.is_tax_exempt as customerIsTaxExempt, \
                    \(1.00 :: Real) as curbCoMarkup "

{- | Base SQL string for retrieving customers from database. -}
baseSql :: Query
baseSql = "select "
       <> baseCustomerQuery
       <> "from customers"

-- | Base insertion SQL. No default values at this time.
insertSql :: Query
insertSql = "insert into customers \
            \(name, company, customer_type, markup, is_tax_exempt) \
            \values (?, ?, ?, ?, ?) returning id"

-- | Naive search for matching, case-sensitive strings.
searchSql :: Query
searchSql = " where \
            \customers.name like ? or \
            \customers.company like ? or \
            \customers.customer_type like ?"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by customers.id asc"

-- | Pagination SQL portion.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update customers \
            \set name = ?, \
            \company = ?, \
            \customer_type = ?, \
            \markup = ?, \
            \is_tax_exempt = ? \
            \where customers.id = ?"

-- | Simple toggle query.
taxExemptFlag :: Query
taxExemptFlag = "update customers set is_tax_exempt = ? where id = ?"
