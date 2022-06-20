{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Addresses
Description: Management of address book entries.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Shared type for storing and using customer or vendor addresses. Primarily used for generation
in HTML files and templates. Could be extended to support shipping at some point. No significant address verification is done prior to storage at this time.

-}

module Addresses
  ( addAddress
  , assignCCQuoteToAddress
  , assignCustomerToAddress
  , assignVendorToAddress
  , baseAddressQuery
  , deleteAddress
  , editAddress
  , getAddressById
  , listAddresses
  , retrieveCCQuoteAddress
  , retrieveCustomerAddress
  , retrieveVendorAddress
  )
  where

import Data.ByteString ( ByteString )
import Database.PostgreSQL.Simple ( Query )
import Data.Text ( Text )
import GHC.Int ( Int64 )


-- LOCAL IMPORTS --
import APITypes ( Address(..)
                , APIError(..)
                , Context
                , QueryPag(..)
                )
import Database ( runExecute
                , runQuery
                )
import Utils ( badReq
             , eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds new address entry. Not yet specified to a customer/vendor.
addAddress :: Context -> Address -> IO (Either APIError Address)
addAddress ctx a = runQuery ctx insertSql ( add_street a
                                          , add_street2 a
                                          , add_city a
                                          , add_state a
                                          , add_zip a
                                          )
  >>= eitherPassErr (gen (\na nid -> na { add_id = nid })
                         "Failure to return new address id"
                         a
                    )

-- | Retrieves address record by id ('Int'). Returns 404 'APIError' if not found.
getAddressById :: Context -> Int -> IO (Either APIError Address)
getAddressById ctx aid =
  runQuery ctx (baseSql <> " where addresses.id = ?" <> sortSql) [aid]
  >>= eitherPassErr (wasFound "Address not found")

-- | Retrieves addresses by querystring. Returns empty list if none found.
listAddresses :: Context -> QueryPag -> IO (Either APIError [Address])
listAddresses ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listAddresses ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, s, s, s, l, o)

-- | Delete address record by id ('Int').
deleteAddress :: Context -> Int -> IO (Either APIError ())
deleteAddress ctx aid = runExecute ctx delAddressSql [aid] >>= eitherPassErr (\_ -> passSuccess ())

-- | Edits the specified address entry by id ('Int').
editAddress :: Context -> Int -> Address -> IO (Either APIError Address)
editAddress ctx aid a
  | add_id a == aid = runExecute ctx updateSql ( add_street a
                                               , add_street2 a
                                               , add_city a
                                               , add_state a
                                               , add_zip a
                                               , add_id a
                                               )
    >>= eitherPassErr (\(_ :: Int64) -> passSuccess a)
  | otherwise = badReq 400 "Address uri id and body id mismatch"

-- | Assign customer by id ('Int') to address book entry.
assignCustomerToAddress :: Context -> Int -> Int -> IO (Either APIError ())
assignCustomerToAddress = assignToAddress "C"

-- | Assign vendor by id ('Int') to address book entry.
assignVendorToAddress :: Context -> Int -> Int -> IO (Either APIError ())
assignVendorToAddress = assignToAddress "V"

-- | Assign CC quote by id ('Int') to address book entry.
assignCCQuoteToAddress :: Context -> Int -> Int -> IO (Either APIError ())
assignCCQuoteToAddress = assignToAddress "T"

-- | Helper to reduce code repetition. Assigns customer/vendor to address book entry based on query.
assignToAddress :: Text -> Context -> Int -> Int -> IO (Either APIError ())
assignToAddress objType ctx aid objid = runExecute ctx unassignSql (objid, objType)
  >>= eitherPassErr (\_ -> runExecute ctx assignSql (objid, objType, aid))
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Retrieve a mapped address for the customer (if exists). 404 if not found.
retrieveCustomerAddress :: Context -> Int -> IO (Either APIError Address)
retrieveCustomerAddress = retrieveMappedAddress "C" "Customer address not found"

-- | Retrieve a mapped address for the vendor (if exists). 404 if not found.
retrieveVendorAddress :: Context -> Int -> IO (Either APIError Address)
retrieveVendorAddress = retrieveMappedAddress "V" "Vendor address not found"

-- | Retrieves a mapped address for the CC quote (if exists). 404 if not found.
retrieveCCQuoteAddress :: Context -> Int -> IO (Either APIError Address)
retrieveCCQuoteAddress = retrieveMappedAddress "T" "CC quote address not found - must be set prior to adding curbs"

-- | Helper to retrieve a mapped address for given obj type. 404 if not found.
retrieveMappedAddress :: Text -> ByteString -> Context -> Int -> IO (Either APIError Address)
retrieveMappedAddress objType msg404 ctx objid = runQuery ctx getMapSql (objType, objid)
  >>= eitherPassErr (wasFound msg404)

-- SQL --
-- | Base search fields for address retreival. Exported.
baseAddressQuery :: Query
baseAddressQuery = "address_book.id as addressId, \
                 \address_book.street as street, \
                 \address_book.street2 as street2, \
                 \address_book.city as city, \
                 \address_book.state as state, \
                 \address_book.zip as zip "

-- | Main search query.
baseSql :: Query
baseSql = "select "
       <> baseAddressQuery
       <> "from address_book"

-- | Main insertion SQL. Check database for default value(s).
insertSql :: Query
insertSql = "insert into address_book (\
            \street, \
            \street2, \
            \city, \
            \state, \
            \zip \
            \) values (?, ?, ?, ?, ?) \
            \returning id"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by address_book.street desc"

-- | Deletion SQL.
delAddressSql :: Query
delAddressSql = "delete from address_book where id = ?"

-- | Update address SQL.
updateSql :: Query
updateSql = "update address_book set \
            \street = ?, \
            \street2 = ?, \
            \city = ?, \
            \state = ?, \
            \zip = ? \
            \where address_book.id = ?"

-- | Assign mapping to address entry. ref_type C -> customer, V -> vendor. ref_id is either customer or vendor unique int.
assignSql :: Query
assignSql = "insert into m_address_obj (\
            \ref_id, \
            \ref_type, \
            \fk_address_book_id \
            \) values (?, ?, ?)"

-- | Removes any current mappings before new one applied.
unassignSql :: Query
unassignSql = "delete from m_address_obj where \
              \ref_id = ? and \
              \ref_type = ?"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Search SQL for naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " where \
            \address_book.street like ? or \
            \address_book.street2 like ? or \
            \address_book.city like ? or \
            \address_book.state like ? or \
            \address_book.zip like ?"

-- | Query to retrieve any matching address entries for a given mapping type.
getMapSql :: Query
getMapSql = baseSql
         <> " join m_address_obj \
            \on address_book.id = m_address_obj.fk_address_book_id \
            \where m_address_obj.ref_type = ? \
            \and m_address_obj.ref_id = ?"
