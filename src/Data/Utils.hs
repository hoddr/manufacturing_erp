{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Utils
Description: Utility and generic data type declarations.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Various types declarations for utility or generic use, as well as a number of functions intended for use
in type definitions (and possibly elsewhere).

-}

module Data.Utils
  ( DateRange(..)
  , QueryPag(..)
  , defDate
  , defQueryPag
  , formatDate
  , isFitting
  , isOvalPipe
  , isPipe
  , isStraightDuct
  , liftM6
  , liftM7
  , liftM11
  , liftM14
  , takeN
  )
  where

import Data.Aeson ( FromJSON
                  , parseJSON
                  , withObject
                  , (.:)
                  )
import Data.ByteString ( ByteString )
import Data.Text ( Text
                 , pack
                 )
import qualified Data.Text as T
import Data.Time ( UTCTime(..)
                 , secondsToDiffTime
                 )
import Data.Time.Calendar ( fromGregorian )
import Data.Time.Format ( defaultTimeLocale
                        , formatTime
                        )

-- | Querystring search (case-sensitive by default), pagination, offset fields.
newtype QueryPag = QueryPag (ByteString, ByteString, ByteString)

-- | Default querystring search and pagination values.
defQueryPag :: QueryPag
defQueryPag = QueryPag ("", "10000", "0")


-- | Helper type for holding and parsing date ranges.
data DateRange = DateRange { dr_start :: UTCTime
                           , dr_end :: UTCTime
                           } deriving (Show)

instance FromJSON DateRange where
  parseJSON = withObject "dateRange" $ \dr -> do
    s <- dr .: "start"
    e <- dr .: "end"
    pure $ DateRange { dr_start = s
                     , dr_end = e
                     }

-- | Lift variant for 6 actions.
liftM6 :: (Monad m)
       => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
       -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  x5 <- m5
  x6 <- m6
  pure (f x1 x2 x3 x4 x5 x6)

-- | Lift variant for 7 actions.
liftM7 :: (Monad m)
       => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r)
       -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m r
liftM7 f m1 m2 m3 m4 m5 m6 m7 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  x5 <- m5
  x6 <- m6
  x7 <- m7
  pure (f x1 x2 x3 x4 x5 x6 x7)

-- | Lift variant for 11 actions.
liftM11  :: (Monad m)
         => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> r)
         -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m a10 -> m a11 -> m r
liftM11 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  x5 <- m5
  x6 <- m6
  x7 <- m7
  x8 <- m8
  x9 <- m9
  x10 <- m10
  x11 <- m11
  pure (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)

-- | Lift variant for 14 actions.
liftM14  :: (Monad m)
         => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13 -> a14 -> r)
         -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m a10 -> m a11 -> m a12 -> m a13 -> m a14 -> m r
liftM14 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  x5 <- m5
  x6 <- m6
  x7 <- m7
  x8 <- m8
  x9 <- m9
  x10 <- m10
  x11 <- m11
  x12 <- m12
  x13 <- m13
  x14 <- m14
  pure (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)

-- | Simple helper to format 'UTCTime' to text.
formatDate :: UTCTime -> Text
formatDate = pack . formatTime defaultTimeLocale "%d %B %Y"

-- | Default 'UTCTime' for Maybe UTCTime fields.
defDate :: UTCTime
defDate = UTCTime (fromGregorian 2025 01 01) (secondsToDiffTime 0)

-- | Helper to take n from text (string).
takeN :: T.Text -> Int -> T.Text
takeN t n = T.toUpper $ T.take n t

-- | Helper to determine if rect duct is fitting/duct straight based on name.
isStraightDuct :: T.Text -> Bool
isStraightDuct n = takeN n 2 == "D " || takeN n 3 == "DS " || takeN n 5 == "DSCC "

-- | Helper to determine if round duct is fitting/straight pipe based on name.
isPipe :: T.Text -> Bool
isPipe n = takeN n 2 == "SP" || takeN n 3 == "SP " || takeN n 15 == "STOCK SPIRAL:SP"

-- | Helper to determine if oval duct is fitting/straight pipe based on name.
isOvalPipe :: T.Text -> Bool
isOvalPipe n = takeN n 3 == "OSP" || takeN n 4 == "OSP "

-- | Helper to determine if line item is a fitting or not.
isFitting :: T.Text -> T.Text -> Bool
isFitting categ desc = case T.toUpper categ of
                            "RECTANGULAR DUCT" -> not $ isStraightDuct desc
                            "ROUND DUCT" -> not $ isPipe desc
                            "STOCK PULL" -> not $ isPipe desc
                            "OVAL DUCT" -> not $ isOvalPipe desc
                            "MISC RECTANGULAR" -> True
                            "MISC ROUND OVAL" -> False
                            _ -> False
