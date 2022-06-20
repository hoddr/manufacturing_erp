{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Markups
Description: Logic for retrieval and updates of labor rates and markups.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions for retrieving and updating markups and margins, controlling all
line item prices. These are applied after summing all base costs but BEFORE the
customer specific markups.

There are two portions: labor and item type markups. Labor is comprised of overhead
and shop rates. There are numerous item type markups, including: purchase items, fabrication items (non-stock set items), rectangular, round, oval, stock, assembly, and general materials.
These markup factors are set to ensure a 35% markup for AJE-level customers. There
is documentation in-house detailing these numbers and calculations.

-}

module Markups
  ( -- * Functions
    getLatestMarkupRates
  , getLatestPricing
  , getLatestRate
  , updateMarkupRates
  , updateLaborRate
  )
  where

import Database.PostgreSQL.Simple ( Query )

-- LOCAL IMPORTS --
import APITypes ( APIError
                , Context
                , LaborRate(..)
                , Markup(..)
                , PricingInfo(..)
                )
import Database ( runExecute_
                , runQuery
                , runQuery_
                )
import Utils ( eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Retrieves the latest item type markup rates. Returns 404 if no current values found.
getLatestMarkupRates :: Context -> IO (Either APIError Markup)
getLatestMarkupRates c = runQuery_ c (baseSql <> baseFilterSql)
  >>= eitherPassErr (wasFound "No current markup rates found")

-- | Adds new set of item type markup rates, after first setting the previous version(s) to not current.
updateMarkupRates :: Context -> Markup -> IO (Either APIError Markup)
updateMarkupRates ctx m = runExecute_ ctx voidCurrentSql
  >>= eitherPassErr (\_ -> runQuery ctx insertSql m)
  >>= eitherPassErr (gen (\mk nid -> mk { mkupid = nid })
                              "Failure to return new markup id"
                              m
                    )

-- | Retrieves the latest labor rates. Returns 404 if no current values found.
getLatestRate :: Context -> IO (Either APIError LaborRate)
getLatestRate c = runQuery_ c baseLaborSql
  >>= eitherPassErr (wasFound "No current labor rate found")

-- | Adds new set of labor rates, after first setting the previous version(s) to not current.
updateLaborRate :: Context -> LaborRate -> IO (Either APIError LaborRate)
updateLaborRate ctx l = runExecute_ ctx voidCurrentLaborSql
  >>= eitherPassErr (\_ -> runQuery ctx insertLaborSql l)
  >>= eitherPassErr (gen (\lr nid -> lr { lrid = nid })
                              "Failure to return new labor rate id"
                              l
                    )

-- | Retrieves both markup and labor rates. Returns as encapsulate tuple.
getLatestPricing :: Context -> IO (Either APIError PricingInfo)
getLatestPricing c = getLatestRate c
  >>= eitherPassErr (\lr -> getLatestMarkupRates c
    >>= eitherPassErr (\mrs -> passSuccess $ PricingInfo (lr, mrs)))


-- SQL --
-- | Base query SQL. Changes here MUST be mirrored in "Orders".
baseSql :: Query
baseSql = "select \
          \markups.id as id, \
          \markups.purchase as purchase, \
          \markups.fabrication as fabrication, \
          \markups.rectangular_fab as rect, \
          \markups.round_fab as round, \
          \markups.oval_fab as oval, \
          \markups.stock as stock, \
          \markups.assembly as assembly, \
          \markups.material as material, \
          \markups.quote as quote, \
          \markups.project as project, \
          \markups.normal as order, \
          \markups.is_current as isCurrent, \
          \markups.added_at as addedAt \
          \from markups"

-- | Default filter SQL. Limits to the latest only.
baseFilterSql :: Query
baseFilterSql = " where markups.is_current limit 1"

-- | Base insertion SQL. Check database for default values.
insertSql :: Query
insertSql = "insert into markups (\
              \purchase, \
              \fabrication, \
              \rectangular_fab, \
              \round_fab, \
              \oval_fab, \
              \stock, \
              \assembly, \
              \material, \
              \quote, \
              \project, \
              \normal \
            \) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Sets all present markup(s) to non-current.
voidCurrentSql :: Query
voidCurrentSql = "update markups set is_current = false"

-- | Base labor query SQL. Changes MUST be reflected in "Orders".
baseLaborSql :: Query
baseLaborSql = "select \
               \labor_rate.id as id, \
               \labor_rate.shop_rate as shopRate, \
               \labor_rate.overhead_rate as overheadRate, \
               \labor_rate.rate as rate, \
               \labor_rate.is_current as isCurrent, \
               \labor_rate.base_unit as baseUnit, \
               \labor_rate.added_at as addedAt \
               \from labor_rate \
               \where labor_rate.is_current \
               \limit 1"

-- | Sets all labor entry(s) to non-current.
voidCurrentLaborSql :: Query
voidCurrentLaborSql = "update labor_rate set is_current = false"

-- | Base insertion SQL for labor.
insertLaborSql :: Query
insertLaborSql = "insert into labor_rate (\
                   \shop_rate, \
                   \overhead_rate \
                 \) values (?, ?) \
                 \returning id"
