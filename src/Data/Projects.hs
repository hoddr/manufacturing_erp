{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Projects
Description: Data type declarations for project-related functions.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for project-related functions.

-}

module Data.Projects
  ( Category(..)
  , Extra(..) -- deprecated
  , Project(..)
  , ProjectBillingInfo(..)
  , ProjectSection(..)
  , ProjectStatusReport(..)
  , ProjectTracker(..)
  )
  where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , object
                  , parseJSON
                  , toJSON
                  , toJSONList
                  , withObject
                  , (.=)
                  , (.:)
                  , (.:?)
                  )
import qualified Data.Csv as C
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Database.PostgreSQL.Simple.FromRow ( FromRow
                                          , field
                                          , fromRow
                                          )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )
import Prelude hiding ( id )

-- LOCAL --
import Data.CustomersVendors ( Customer(..) )
import Data.OrdersQuotes ( LineItem(..)
                         , Order
                         , RawLineItem(..)
                         )
import Data.Utils ( liftM7 )

-- | Main project type. Stores top-level project details. Many details and figures are calculated client-side.
data Project = Project { prjid :: Int
                       , prjappId :: Int
                       , prjname :: Text
                       , prjpo :: Text
                       , prjcustomer :: Customer
                       , prjtotEstWeight :: Double
                       , prjisActive :: Bool
                       , prjquoteId :: Int
                       , prjquoteNumber :: Text
                       , prjextras :: [Extra]
                       , prjorders :: [Order]
                       , prjsections :: [ProjectSection]
                       } deriving (Show)

instance ToJSON Project where
  toJSON p = object [ "id" .= prjid p
                    , "appId" .= prjappId p
                    , "name" .= prjname p
                    , "po" .= prjpo p
                    , "customer" .= prjcustomer p
                    , "totalEstimatedWeight" .= prjtotEstWeight p
                    , "isActive" .= prjisActive p
                    , "quoteId" .= prjquoteId p
                    , "quoteNumber" .= prjquoteNumber p
                    , "sections" .= toJSONList (prjsections p)
                    , "extras" .= toJSONList (prjextras p)
                    , "orders" .= toJSONList (prjorders p)
                    ]

instance FromJSON Project where
  parseJSON = withObject "project" $ \p -> do
    pid <- p .:? "id"
    pappId <- p .: "appId"
    pname <- p .: "name"
    ppo <- p .: "po"
    pCust <- p .: "customer"
    customer <- parseJSON pCust
    totEstWeight <- p .: "totalEstimatedWeight"
    isActive <- p .: "isActive"
    quoteId <- p .: "quoteId"
    quoteNumber <- p .: "quoteNumber"
    pure $ Project { prjid = fromMaybe 0 pid
                   , prjappId = pappId
                   , prjname = pname
                   , prjpo = ppo
                   , prjcustomer = customer
                   , prjtotEstWeight = totEstWeight
                   , prjisActive = isActive
                   , prjquoteId = quoteId
                   , prjquoteNumber = quoteNumber
                   , prjextras = []
                   , prjorders = []
                   , prjsections = []
                   }

instance FromRow Project where
  fromRow = Project <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> liftM7 Customer field field field field field field field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> pure []
                    <*> pure []
                    <*> pure []

-- PROJECT EXTRAS --
-- | Project extra entry. Deprecated?
data Extra = Extra { exid :: Int
                   , excatId :: Int
                   , excatName :: Text
                   , exnameCheck :: Maybe Text
                   , exprjId :: Int
                   } deriving (Show)

instance ToJSON Extra where
  toJSON ex = object [ "id" .= exid ex
                     , "categoryId" .= excatId ex
                     , "categoryName" .= excatName ex
                     , "nameCheck" .= exnameCheck ex
                     , "projectId" .= exprjId ex
                     ]

instance FromJSON Extra where
  parseJSON = withObject "extra" $ \ex -> do
    eid <- ex .:? "id"
    catId <- ex .: "categoryId"
    catName <- ex .: "categoryName"
    nc <- ex .:? "nameCheck"
    pid <- ex .: "projectId"
    pure $ Extra { exid = fromMaybe 0 eid
                 , excatId = catId
                 , excatName = catName
                 , exnameCheck = nc
                 , exprjId = pid
                 }

instance FromRow Extra where
  fromRow = Extra <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

instance ToRow Extra where
  toRow ex = [ toField (excatId ex)
             , toField (exnameCheck ex)
             , toField (exprjId ex)
             ]

-- PROJECT SECTIONS --
-- | Sub-section of projects. Permits better project tracking (intention) and for more accurate billing by weight.
data ProjectSection = ProjectSection { psid :: Int
                                     , psname :: Text
                                     , psprice :: Double
                                     , psweight :: Double
                                     , psprojectId :: Int
                                     , psorders :: [Order]
                                     } deriving (Show)

instance ToJSON ProjectSection where
  toJSON ps = object [ "id" .= psid ps
                     , "name" .= psname ps
                     , "price" .= psprice ps
                     , "weight" .= psweight ps
                     , "projectId" .= psprojectId ps
                     , "orders" .= toJSONList (psorders ps)
                     ]

instance FromJSON ProjectSection where
  parseJSON = withObject "section" $ \ps -> do
    id <- ps .:? "id"
    name <- ps .: "name"
    price <- ps .: "price"
    weight <- ps .: "weight"
    projectId <- ps .: "projectId"
    pure $ ProjectSection { psid = fromMaybe 0 id
                          , psname = name
                          , psprice = price
                          , psweight = weight
                          , psprojectId = projectId
                          , psorders = []
                          }

instance FromRow ProjectSection where
  fromRow = ProjectSection <$> field
                           <*> field
                           <*> field
                           <*> field
                           <*> field
                           <*> pure []

instance ToRow ProjectSection where
  toRow ps = [ toField (psname ps)
             , toField (psprice ps)
             , toField (psweight ps)
             , toField (psprojectId ps)
             ]

-- PROJECT BILLING RETURN --
-- | Return structure for billing a project (extras, non-extras value, non-extras weight).
newtype ProjectBillingInfo = ProjectBillingInfo ([LineItem], Double, Double)

-- | note that the export is back to raw line items for writing to QB!
instance ToJSON ProjectBillingInfo where
  toJSON (ProjectBillingInfo (lis, tot, weight)) =
    object [ "extras" .= toJSONList (map convertLItoRLI lis)
           , "nonExtrasTotal" .= tot
           , "nonExtrasWeight" .= weight
           ]

-- PROJECT TRACKER --
-- | Holder and semigroup to sum up project tracking information over the line items.
data ProjectTracker = ProjectTracker { pt_rectDuctLength :: Double
                                     , pt_rectFittingCount :: Double
                                     , pt_rectWeight :: Double
                                     , pt_roundPipeLength :: Double
                                     , pt_roundFittingCount :: Double
                                     , pt_roundWeight :: Double
                                     , pt_ovalPipeLength :: Double
                                     , pt_ovalFittingCount :: Double
                                     , pt_ovalWeight :: Double
                                     , pt_grilleBoxes :: Double
                                     , pt_flexRectangular :: Double
                                     , pt_flexRound :: Double
                                     , pt_hetos :: Double
                                     , pt_lnSqFtRect :: Double
                                     , pt_lnSqFtRound :: Double
                                     , pt_lnSqFtOval :: Double
                                     , pt_dbWallRectWeight :: Double
                                     , pt_dbWallRoundWeight :: Double
                                     , pt_dbWallOvalWeight :: Double
                                     } deriving (Show)

instance ToJSON ProjectTracker where
  toJSON pt = object [ "rectDuctLength" .= pt_rectDuctLength pt
                     , "rectFittingCount" .= pt_rectFittingCount pt
                     , "rectWeight" .= pt_rectWeight pt
                     , "roundDuctLength" .= pt_roundPipeLength pt
                     , "roundFittingCount" .= pt_roundFittingCount pt
                     , "roundWeight" .= pt_roundWeight pt
                     , "ovalPipeLength" .= pt_ovalPipeLength pt
                     , "ovalFittingCount" .= pt_ovalFittingCount pt
                     , "ovalWeight" .= pt_ovalWeight pt
                     , "grilleBoxes" .= pt_grilleBoxes pt
                     , "flexRectangular" .= pt_flexRectangular pt
                     , "flexRound" .= pt_flexRound pt
                     , "hetos" .= pt_hetos pt
                     , "linerSqFeetRect" .= pt_lnSqFtRect pt
                     , "linerSqFeetRound" .= pt_lnSqFtRound pt
                     , "linerSqFeetOval" .= pt_lnSqFtOval pt
                     , "doubleWallRectWeight" .= pt_dbWallRectWeight pt
                     , "doubleWallRoundWeight" .= pt_dbWallRoundWeight pt
                     , "doubleWallOvalWeight" .= pt_dbWallOvalWeight pt
                     ]

instance Semigroup ProjectTracker where
  (<>) pt1 pt2 =
    ProjectTracker { pt_rectDuctLength = pt_rectDuctLength pt1 + pt_rectDuctLength pt2
                   , pt_rectFittingCount = pt_rectFittingCount pt1 + pt_rectFittingCount pt2
                   , pt_rectWeight = pt_rectWeight pt1 + pt_rectWeight pt2
                   , pt_roundPipeLength = pt_roundPipeLength pt1 + pt_roundPipeLength pt2
                   , pt_roundFittingCount = pt_roundFittingCount pt1 + pt_roundFittingCount pt2
                   , pt_roundWeight = pt_roundWeight pt1 + pt_roundWeight pt2
                   , pt_ovalPipeLength = pt_ovalPipeLength pt1 + pt_ovalPipeLength pt2
                   , pt_ovalFittingCount = pt_ovalFittingCount pt1 + pt_ovalFittingCount pt2
                   , pt_ovalWeight = pt_ovalWeight pt1 + pt_ovalWeight pt2
                   , pt_grilleBoxes = pt_grilleBoxes pt1 + pt_grilleBoxes pt2
                   , pt_flexRectangular = pt_flexRectangular pt1 + pt_flexRectangular pt2
                   , pt_flexRound = pt_flexRound pt1 + pt_flexRound pt2
                   , pt_hetos = pt_hetos pt1 + pt_hetos pt2
                   , pt_lnSqFtRect = pt_lnSqFtRect pt1 + pt_lnSqFtRect pt2
                   , pt_lnSqFtRound = pt_lnSqFtRound pt1 + pt_lnSqFtRound pt2
                   , pt_lnSqFtOval = pt_lnSqFtOval pt1 + pt_lnSqFtOval pt2
                   , pt_dbWallRectWeight = pt_dbWallRectWeight pt1 + pt_dbWallRectWeight pt2
                   , pt_dbWallRoundWeight = pt_dbWallRoundWeight pt1 + pt_dbWallRoundWeight pt2
                   , pt_dbWallOvalWeight = pt_dbWallOvalWeight pt1 + pt_dbWallOvalWeight pt2
                   }

instance Monoid ProjectTracker where
  mempty = ProjectTracker { pt_rectDuctLength = 0.00
                          , pt_rectFittingCount = 0
                          , pt_rectWeight = 0.00
                          , pt_roundPipeLength = 0.00
                          , pt_roundFittingCount = 0
                          , pt_roundWeight = 0.00
                          , pt_ovalPipeLength = 0.00
                          , pt_ovalFittingCount = 0
                          , pt_ovalWeight = 0.00
                          , pt_grilleBoxes = 0.00
                          , pt_flexRectangular = 0.00
                          , pt_flexRound = 0.00
                          , pt_hetos = 0.00
                          , pt_lnSqFtRect = 0.00
                          , pt_lnSqFtRound = 0.00
                          , pt_lnSqFtOval = 0.00
                          , pt_dbWallRectWeight = 0.00
                          , pt_dbWallRoundWeight = 0.00
                          , pt_dbWallOvalWeight = 0.00
                          }
  mappend = (<>)

-- PROJECT STATUS REPORT --
-- | Type to sum and report information regarding all open project statuses at high-level (to CSV).
data ProjectStatusReport = ProjectStatusReport { psr_appId :: Int
                                               , psr_estWeight :: Double
                                               , psr_currWeight :: Double
                                               , psr_percWeight :: Double
                                               , psr_estPrice :: Double
                                               , psr_currDBPrice :: Double
                                               , psr_percPrice :: Double
                                               } deriving (Show)

instance C.ToNamedRecord ProjectStatusReport where
  toNamedRecord psr =
    C.namedRecord [ "appId" C..= psr_appId psr
                  , "estWeight" C..= psr_estWeight psr
                  , "currWeight" C..= psr_currWeight psr
                  , "percWeight" C..= psr_percWeight psr
                  , "estPrice" C..= psr_estPrice psr
                  , "currDBPrice" C..= psr_currDBPrice psr
                  , "percPrice" C..= psr_percPrice psr
                  ]

instance C.DefaultOrdered ProjectStatusReport where
  headerOrder _ = C.header [ "appId"
                           , "estWeight"
                           , "currWeight"
                           , "percWeight"
                           , "estPrice"
                           , "currDBPrice"
                           , "percPrice"
                           ]

-- CATEGORIES (FOR EXTRAS) --
-- | Simple type for reporting extra types. Deprecated?
data Category = Category { cat_id :: Int
                         , cat_name :: Text
                         } deriving (Show)

instance FromJSON Category where
  parseJSON = withObject "category" $ \c -> do
    id <- c .: "id"
    name <- c .: "name"
    pure $ Category { cat_id = id
                    , cat_name = name
                    }

instance ToJSON Category where
  toJSON c = object [ "id" .= cat_id c
                    , "name" .= cat_name c
                    ]

instance FromRow Category where
  fromRow = Category <$> field <*> field

-- | Conversion from line items to raw line items for project billing. Empty fields not needed here.
convertLItoRLI :: LineItem -> RawLineItem
convertLItoRLI li = RawLineItem { rli_customer = ""
                                , rli_orderNumber = fromMaybe "" (lorderNumber li)
                                , rli_po = ""
                                , rli_category = lcategory li
                                , rli_quantity = lquant li
                                , rli_name = ldesc li
                                , rli_gauge = ""
                                , rli_material = ""
                                , rli_materialWeight = 0.00
                                , rli_weight = 0.00
                                , rli_linerMaterial = ""
                                , rli_linerArea = 0.00
                                , rli_skinGauge = ""
                                , rli_skinMaterial = ""
                                , rli_skinWeight = 0.00
                                , rli_accessoryCost = 0.00
                                , rli_rate = ""
                                , rli_isWrapped = False
                                , rli_priceEach = lprice li
                                , rli_length = 0.00
                                , rli_isCatalogue = False
                                , rli_labor = Just 0.00
                                }
