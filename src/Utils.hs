{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Utils
Description: Utilities and common helper functions.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Any utility and helper functions that don't have a better place to be.

-}
module Utils
  ( -- * API Errors
    apiError500
  , apiError400
  , apiError401
  , apiError403
  , apiError404
  , badReq
  -- * API Messages
  , apiMsg200
  , apiMsg201
  , apiMsg204
  -- * Utilities
  , calcWeightedCost
  , isValidOrderType
  , isValidQuoteType
  , eitherPassErr
  , flattenData
  , flattenErrors
  , fst5
  , gen
  , passOnError
  , passSuccess
  , safeHead
  , textToInt
  , wasFound
  -- * SQL
  , inventorySql
  , insertInventorySql
  ) where


import Data.Aeson ( encode )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as LB
import Data.Either ( partitionEithers )
import qualified Data.Text as T
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )
import Network.HTTP.Types ( mkStatus
                          , status500
                          )
import Text.Read ( readEither )



-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                )


-- FUNCTIONS
-- | Wrapper to generate 'APIError' response with given error code and message. Defaults to 500 error on invalid code numbers.
badReq :: Int -> ByteString -> IO (Either APIError a)
badReq code bs
  | code == 400 = partial $ apiError400 bs
  | code == 401 = partial $ apiError401 bs
  | code == 403 = partial apiError403
  | code == 404 = partial $ apiError404 bs
  | otherwise = partial apiError500
  where partial = pure . Left

-- | Produces 500 Internal Server error.
apiError500 :: APIError
apiError500 = APIError status500

-- | Produces 400 Bad Request error with given message.
apiError400 :: ByteString -> APIError
apiError400 = APIError . mkStatus 400

-- | Produces 401 Unauthorized error with given message.
apiError401 :: ByteString -> APIError
apiError401 = APIError . mkStatus 401

-- | Produces 403 Forbidden error with given message.
apiError403 :: APIError
apiError403 = APIError $ mkStatus 403 "Invalid/insufficient user privileges"

-- | Produces 404 Not Found error with given message.
apiError404 :: ByteString -> APIError
apiError404 = APIError . mkStatus 404

-- | Produces 200 OK response with given message.
apiMsg200 :: ByteString -> APIMsg
apiMsg200 = APIMsg . mkStatus 200

-- | Produces 201 Created response with given message.
apiMsg201 :: ByteString -> APIMsg
apiMsg201 = APIMsg . mkStatus 201

-- | Produces 204 No Content response with given message.
apiMsg204 :: ByteString -> APIMsg
apiMsg204 = APIMsg . mkStatus 204

-- | Safe alternative to 'head'.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

-- | Helper for subsequent monadic actions.
eitherPassErr :: (b -> IO (Either APIError a))
              -> Either APIError b
              -> IO (Either APIError a)
eitherPassErr = either passOnError

-- | Helper for passing on error to monadic chain.
passOnError :: APIError -> IO (Either APIError a)
passOnError = pure . Left

-- | Helper for passing success to monadic chain.
passSuccess :: a -> IO (Either APIError a)
passSuccess = pure . Right

-- | Helper to take list of monadic action results and flatten to either single error message or list of results.
flattenData :: [Either APIError a] -> Either APIError [a]
flattenData eds = case partitionEithers eds of
                         ([], ds) -> Right ds
                         (e:_, _) -> Left e

-- | Takes list errors and joins to single error (joins all info into single 400 error).
flattenErrors :: [APIError] -> APIError
flattenErrors = apiError400 . LB.toStrict . LB.concat . map encode

-- | Wrapper for simplfying check for singular entry GET requests from database.
wasFound :: ByteString -> [a] -> IO (Either APIError a)
wasFound errMsg as = maybe (badReq 404 errMsg) passSuccess (safeHead as)

-- | Wrapper to attach new database generated sequential int to the object; else 500 error.
gen :: (a -> Int -> a)
    -> T.Text
    -> a
    -> [Only Int]
    -> IO (Either APIError a)
gen _ errMsg _ [] = print errMsg >> pure (Left apiError500)
gen updateFn _ obj ((Only newId):_) = passSuccess $ updateFn obj newId

-- | Helper for routing.
textToInt :: T.Text -> Either String Int
textToInt = readEither . T.unpack

-- | Helper for retrieving necessary pricing information.
fst5 :: (a,  b,  c, d, e) -> a
fst5 (a, _, _, _, _) = a

-- | Checker for valid order types. FL is not a valid order type as it MUST come from a quote.
isValidOrderType :: T.Text -> Bool
isValidOrderType t = up == "FS" || up == "FP"
  where up = T.toUpper t

-- | Checker for valid quote types. QB is not used at the moment.
isValidQuoteType :: T.Text -> Bool
isValidQuoteType t = up == "QP" || up == "QL" || up == "QB"
  where up = T.toUpper t

-- isValidRole :: T.Text -> Bool
-- isValidRole r = up == "ADMIN" || up == "RW"
--   where up = T.toUpper r

-- | Helper for purchase orders. Q for quantity and Pe for price each. TODO needs to handle negatives/0 quantity better.
calcWeightedCost :: Double -> Double -> Double -> Double -> Double
calcWeightedCost currQ currPe newQ newPe =
  let safeCurrQ = max 0.0 currQ
      tot = safeCurrQ + newQ
      currWeight = safeCurrQ / tot
      newWeight = newQ / tot
  in
    currWeight * currPe + newWeight * newPe

-- SHARED SQL --
-- | Generic SQL for getting inventory entries.
inventorySql :: Query
inventorySql = " inventory.id as invId, \
               \inventory.reference_id as refId, \
               \inventory.reference_type as refType, \
               \inventory.on_hand as onHand, \
               \inventory.on_order as onOrder, \
               \inventory.min_on_hand as minOnHand"

-- | Generic insert SQL for inventory entries.
insertInventorySql :: Query
insertInventorySql = "insert into inventory (\
                       \reference_id, \
                       \reference_type, \
                       \on_hand, \
                       \on_order, \
                       \min_on_hand \
                     \) values (?, ?, ?, ?, ?) \
                     \returning id"
