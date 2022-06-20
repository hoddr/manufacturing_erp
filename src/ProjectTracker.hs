{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: ProjectTracker
Description: Summarizes key project tracking numbers.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides logic to sum by order, project section, or project level key figures and statistics
for project management. Check the 'ProjectTracker' type for field details.

-}

module ProjectTracker
  ( -- * Functions
    getTrackerByOrder
  , getTrackerByProject
  , getTrackerBySection
  )
  where


import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T


-- LOCAL IMPORTS --
import APITypes ( LineItem(..)
                , Order(..)
                , Project(..)
                , ProjectSection(..)
                , ProjectTracker(..)
                , takeN
                )

-- TODO may need updates
-- TODO may now use util functions to simplify logic here...
-- FUNCTIONS --
-- | Retrieves tracking totals for the 'Order'.
getTrackerByOrder :: Order -> ProjectTracker
getTrackerByOrder o = foldl' (<>) mempty (map liToPt (olineItems o))

-- | Retrieves tracking totals for the project section.
getTrackerBySection :: ProjectSection -> ProjectTracker
getTrackerBySection ps = mconcat $ map getTrackerByOrder $ psorders ps

-- | Retrieves tracking totals for the project as a whole.
getTrackerByProject :: Project -> ProjectTracker
getTrackerByProject p = mconcat $ map getTrackerBySection $ prjsections p

-- | Helper to take line item information and generate a project tracker instance appropriate
-- for the line item type.
liToPt :: LineItem -> ProjectTracker
liToPt lin =
  let tn = takeN $ ldesc lin
      l = llength lin * lquant lin
      c = lquant lin
      w = fromMaybe 0.00 (lweight lin) * lquant lin
      parseRect
        | tn 2 == "D " || tn 3 == "DS " || tn 5 == "DSCC " = rectDuctDef l w lin
        -- grille box and grille box tall
        | tn 10 == "GRILLE BOX" = grilleBoxDef c w
        -- fx com, fx tdf, fx res
        | tn 2 == "FX" = flexRectDef c w
        | tn 15 == "FABRICATED HETO" = hetoDef c
        | otherwise = rectFittingDef c w lin
      parseRound
        | tn 2 == "SP" = rndPipeDef l w lin
        -- fx com rnd
        | tn 8 == "FX" = flexRoundDef c w
        | otherwise = rndFittingDef c w lin
  in
    case T.toUpper $ lcategory lin of
         "RECTANGULAR DUCT" -> parseRect
         "ROUND DUCT" -> parseRound
         "OVAL DUCT" -> if tn 4 == "OSP " && tn 3 == "OSP"
                           then ovalPipeDef l w lin
                           else ovalFittingDef c w lin
         "STOCK PULL" -> if tn 15 == "STOCK SPIRAL:SP"
                            then rndPipeDef l w lin
                            else rndFittingDef c w lin
         "PURCHASED" -> if tn 4 == "HETO"
                           then hetoDef c
                           else mempty
         "MISC RECTANGULAR" -> rectFittingDef c w lin
         _ -> mempty

  where rectDuctDef l w li = mempty { pt_rectDuctLength = l
                                    , pt_rectWeight = w
                                    , pt_lnSqFtRect = fromMaybe 0.00 $ llinerArea li
                                    , pt_dbWallRectWeight = fromMaybe 0.00 $ lskinWeight li
                                    }
        rectFittingDef c w li = mempty { pt_rectFittingCount = c
                                       , pt_rectWeight = w
                                       , pt_lnSqFtRect = fromMaybe 0.00 $ llinerArea li
                                       , pt_dbWallRectWeight = fromMaybe 0.00 $ lskinWeight li
                                       }
        rndPipeDef l w li = mempty { pt_roundPipeLength = l
                                   , pt_roundWeight = w
                                   , pt_lnSqFtRound = fromMaybe 0.00 $ llinerArea li
                                   , pt_dbWallRoundWeight = fromMaybe 0.00 $ lskinWeight li
                                   }
        rndFittingDef c w li = mempty { pt_roundFittingCount = c
                                      , pt_roundWeight = w
                                      , pt_lnSqFtRound = fromMaybe 0.00 $ llinerArea li
                                      , pt_dbWallRoundWeight = fromMaybe 0.00 $ lskinWeight li
                                      }
        ovalPipeDef l w li = mempty { pt_ovalPipeLength = l
                                    , pt_ovalWeight = w
                                    , pt_lnSqFtOval = fromMaybe 0.00 $ llinerArea li
                                    , pt_dbWallOvalWeight = fromMaybe 0.00 $ lskinWeight li
                                    }
        ovalFittingDef c w li = mempty { pt_ovalFittingCount = c
                                       , pt_ovalWeight = w
                                       , pt_lnSqFtOval = fromMaybe 0.00 $ llinerArea li
                                       , pt_dbWallOvalWeight = fromMaybe 0.00 $ lskinWeight li
                                       }
        hetoDef c = mempty { pt_hetos = c }
        grilleBoxDef c w = mempty { pt_grilleBoxes = c
                                  , pt_rectWeight = w
                                  }
        flexRectDef c w = mempty { pt_flexRectangular = c
                                 , pt_rectWeight = w
                                 }
        flexRoundDef c w = mempty { pt_flexRound = c
                                  , pt_roundWeight = w
                                  }
