module Client.Pricing
  ( getEstimate
  , invalidRlis
  , invalidTimeStringRlis
  , testRlis
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( RawLineItem(..) )
import qualified Client.Common as CC


getEstimate :: [RawLineItem] -> IO [RawLineItem]
getEstimate rlis = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "estimator")
             (ReqBodyJson rlis)
             lbsResponse
             CC.opts
  let elis = (eitherDecode (responseBody res) :: Either String [RawLineItem]) in
    case elis of
         (Left e) -> liftIO (print e) >> pure []
         (Right lis) -> pure lis


-- TEST DATA --
testRlis :: [RawLineItem]
testRlis = [ RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Rectangular Duct"
                         , rli_quantity = 2
                         , rli_name = "E1"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 24.62
                         , rli_weight = 24.62
                         , rli_linerMaterial = "Acoustic x 1.000"
                         , rli_linerArea = 2.84
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 2.04
                         , rli_rate = "00:46:34"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Rectangular Duct"
                         , rli_quantity = 2
                         , rli_name = "Rectangular Straight Duct"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 33.5
                         , rli_weight = 33.5
                         , rli_linerMaterial = "Acoustic x 1.000"
                         , rli_linerArea = 4.05
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:16:58"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "R2-3"
                         , rli_quantity = 1
                         , rli_name = "Rectangular Straight Duct"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 12.99
                         , rli_weight = 12.99
                         , rli_linerMaterial = "Acoustic x 1.000"
                         , rli_linerArea = 1.46
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:32:48"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "E2"
                         , rli_quantity = 1
                         , rli_name = "Rectangular Straight Duct"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 20.14
                         , rli_weight = 20.14
                         , rli_linerMaterial = "Acoustic x 1.000"
                         , rli_linerArea = 2.28
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:40:29"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "EC"
                         , rli_quantity = 1
                         , rli_name = "Rectangular Straight Duct"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 1.91
                         , rli_weight = 1.91
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:06:39"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "6 inch A COLLAR"
                         , rli_quantity = 5
                         -- TODO move to Purchased soon
                         , rli_name = "Duct Accessory"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 0.00
                         , rli_weight = 0.00
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = ""
                         , rli_isWrapped = True
                         , rli_priceEach = 5.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "HETO 6"
                         , rli_quantity = 2
                         , rli_name = "Purchased"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 0.00
                         , rli_weight = 0.00
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = ""
                         , rli_isWrapped = True
                         , rli_priceEach = 35.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Round Duct"
                         , rli_quantity = 1
                         , rli_name = "E"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 21.0
                         , rli_weight = 21.0
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:51:54"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Round Duct"
                         , rli_quantity = 3
                         , rli_name = "Spiral Pipe (Round)"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 43.47
                         , rli_weight = 43.47
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:04:52"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Round Duct"
                         , rli_quantity = 1
                         , rli_name = "CR"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 7.13
                         , rli_weight = 7.13
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:04:51"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Round Duct"
                         , rli_quantity = 3
                         , rli_name = "PC"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 2.04
                         , rli_weight = 2.04
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:05:33"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Round Duct"
                         , rli_quantity = 1
                         , rli_name = "EC RND"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 1.53
                         , rli_weight = 1.53
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:04:22"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Stock Pull"
                         , rli_quantity = 3
                         , rli_name = "STOCK SPIRAL:E45SR-10-26GALV"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 2.84
                         , rli_weight = 2.84
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = ""
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Oval Duct"
                         , rli_quantity = 1
                         , rli_name = "OM90"
                         , rli_gauge = "20"
                         , rli_material = "GALV"
                         , rli_materialWeight = 16.37
                         , rli_weight = 16.37
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "01:06:21"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Oval Duct"
                         , rli_quantity = 2
                         , rli_name = "Spiral Pipe (Oval)"
                         , rli_gauge = "22"
                         , rli_material = "GALV"
                         , rli_materialWeight = 61.86
                         , rli_weight = 61.86
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:31:04"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Oval Duct"
                         , rli_quantity = 1
                         , rli_name = "OER"
                         , rli_gauge = "20"
                         , rli_material = "GALV"
                         , rli_materialWeight = 12.86
                         , rli_weight = 12.86
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "00:44:19"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Oval Duct"
                         , rli_quantity = 1
                         , rli_name = "BT"
                         , rli_gauge = "20"
                         , rli_material = "GALV"
                         , rli_materialWeight = 29.92
                         , rli_weight = 29.92
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "47:52" -- this format is different to catch this parse case in tests
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         , rli_category = "Oval Duct"
                         , rli_quantity = 1
                         , rli_name = "BTR"
                         , rli_gauge = "20"
                         , rli_material = "GALV"
                         , rli_materialWeight = 50.78
                         , rli_weight = 50.78
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = "01:12:27"
                         , rli_isWrapped = True
                         , rli_priceEach = 0.00
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                         , rli_orderNumber = "909090"
                         , rli_po = "foobar"
                         -- TODO this should be Purchased soon
                         , rli_category = "Duct Accessory"
                         , rli_quantity = 10
                         , rli_name = "SOLVENT DP 1090 GALLON"
                         , rli_gauge = "26"
                         , rli_material = "GALV"
                         , rli_materialWeight = 0.00
                         , rli_weight = 0.00
                         , rli_linerMaterial = ""
                         , rli_linerArea = 0.00
                         , rli_skinGauge = ""
                         , rli_skinMaterial = ""
                         , rli_skinWeight = 0.00
                         , rli_accessoryCost = 0.00
                         , rli_rate = ""
                         , rli_isWrapped = True
                         , rli_priceEach = 5.35
                         , rli_length = 0.00
                         , rli_isCatalogue = False
                         , rli_labor = Nothing
                         }
           ]

invalidRlis :: [RawLineItem]
invalidRlis = [ RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                              , rli_orderNumber = "909090"
                              , rli_po = "foobar"
                              , rli_category = "Rectangular Duct"
                              , rli_quantity = 1
                              , rli_name = "E1"
                              , rli_gauge = ""
                              , rli_material = ""
                              , rli_materialWeight = 21.00
                              , rli_weight = 21.00
                              , rli_linerMaterial = ""
                              , rli_linerArea = 0.00
                              , rli_skinGauge = ""
                              , rli_skinMaterial = ""
                              , rli_skinWeight = 0.00
                              , rli_accessoryCost = 0.00
                              , rli_rate = "00:32:12"
                              , rli_isWrapped = True
                              , rli_priceEach = 0.00
                              , rli_length = 0.00
                              , rli_isCatalogue = False
                              , rli_labor = Nothing
                              }
              ] <> testRlis

invalidTimeStringRlis :: [RawLineItem]
invalidTimeStringRlis = [ RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                                      , rli_orderNumber = "909090"
                                      , rli_po = "foobar"
                                      , rli_category = "Oval Duct"
                                      , rli_quantity = 1
                                      , rli_name = "BTR"
                                      , rli_gauge = "20"
                                      , rli_material = "GALV"
                                      , rli_materialWeight = 50.78
                                      , rli_weight = 50.78
                                      , rli_linerMaterial = ""
                                      , rli_linerArea = 0.00
                                      , rli_skinGauge = ""
                                      , rli_skinMaterial = ""
                                      , rli_skinWeight = 0.00
                                      , rli_accessoryCost = 0.00
                                      , rli_rate = "32"
                                      , rli_isWrapped = True
                                      , rli_priceEach = 0.00
                                      , rli_length = 0.00
                                      , rli_isCatalogue = False
                                      , rli_labor = Nothing
                                      }
                        , RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                                      , rli_orderNumber = "909090"
                                      , rli_po = "foobar"
                                      , rli_category = "Oval Duct"
                                      , rli_quantity = 1
                                      , rli_name = "BTR"
                                      , rli_gauge = "20"
                                      , rli_material = "GALV"
                                      , rli_materialWeight = 50.78
                                      , rli_weight = 50.78
                                      , rli_linerMaterial = ""
                                      , rli_linerArea = 0.00
                                      , rli_skinGauge = ""
                                      , rli_skinMaterial = ""
                                      , rli_skinWeight = 0.00
                                      , rli_accessoryCost = 0.00
                                      , rli_rate = "ab:12"
                                      , rli_isWrapped = True
                                      , rli_priceEach = 0.00
                                      , rli_length = 0.00
                                      , rli_isCatalogue = False
                                      , rli_labor = Nothing
                                      }
                        ]
