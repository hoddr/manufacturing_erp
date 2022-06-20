module Client.Quotes
  ( addQuote
  , addQuoteNoLineItems
  , deleteQuote
  , editQuote
  , listQuotes
  , quoteToOrder
  , quoteToProject
  , testOrder
  , testProject
  , testQuote
  , testQuoteNoLineItems
  , testRlis
  )
  where


import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Data.Text ( Text )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( Customer(..)
                , Order(..)
                , Project(..)
                , Quote(..)
                , RawLineItem(..)
                )
import qualified Client.Common as CC


listQuotes :: IO [Quote]
listQuotes = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "quotes")
             NoReqBody
             lbsResponse
             CC.opts
  let eqs = (eitherDecode (responseBody res) :: Either String [Quote]) in
    case eqs of
         (Left e) -> liftIO (print e) >> pure []
         (Right qs) -> pure qs

deleteQuote :: Quote -> IO ()
deleteQuote q = liftIO $ runReq defaultHttpConfig $ do
  _ <- req DELETE
           (http CC.host /: "quotes" /~ qid q)
           NoReqBody
           lbsResponse
           CC.opts
  pure ()


addQuote :: Text -> [RawLineItem] -> IO (Maybe [RawLineItem])
addQuote quoteType rlis = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "quotes" /~ quoteType)
             (ReqBodyJson rlis)
             lbsResponse
             CC.opts
  let erlis = (eitherDecode (responseBody res) :: Either String [RawLineItem]) in
    case erlis of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right pricedRlis) -> pure $ Just pricedRlis

addQuoteNoLineItems :: Quote -> IO (Maybe Quote)
addQuoteNoLineItems q = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "quotes")
             (ReqBodyJson q)
             lbsResponse
             CC.opts
  let eq = (eitherDecode (responseBody res) :: Either String Quote) in
    case eq of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right newQ) -> pure $ Just newQ

editQuote :: Quote -> IO (Maybe Quote)
editQuote q = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "quotes" /~ qid q)
             (ReqBodyJson q)
             lbsResponse
             CC.opts
  let eq = (eitherDecode (responseBody res) :: Either String Quote) in
    case eq of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right _) -> pure $ Just q

quoteToOrder :: Quote -> Order -> IO ()
quoteToOrder q o = liftIO $ runReq defaultHttpConfig $ do
  _ <- req POST
           (http CC.host /: "quotes" /~ qid q /: "order")
           (ReqBodyJson o)
           lbsResponse
           CC.opts
  pure ()

quoteToProject :: Quote -> Project -> IO (Maybe Project)
quoteToProject q p = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "quotes" /~ qid q /: "project")
             (ReqBodyJson p)
             lbsResponse
             CC.opts
  let ep = (eitherDecode (responseBody res) :: Either String Project) in
    case ep of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right newP) -> pure $ Just newP

-- TEST DATA --
testCustomer :: Customer
testCustomer = Customer { cid = 13
                        , cname = "Andy J. Egan Co., Inc."
                        , ccompany = Just "Andy J. Egan Co., Inc."
                        , ctype = Just "Volume Wholesale"
                        , cmarkup = 1.00
                        }

testQuote :: Quote
testQuote = Quote { qid = 0
                  , qtype = "QL"
                  , qcreated = Nothing
                  , qnum = "SP-555"
                  , qordId = Nothing
                  , qordNum = Nothing
                  , qprjId = Nothing
                  , qprjNum = Nothing
                  , qsetPrice = Nothing
                  , qcust = testCustomer
                  , qtotal = 0.00
                  , qisWrapped = False
                  , qlineItems = []
                  }

testQuoteNoLineItems :: Text -> Quote
testQuoteNoLineItems qn = Quote { qid = 0
                                , qtype = "QP"
                                , qcreated = Nothing
                                , qnum = qn
                                , qordId = Nothing
                                , qordNum = Nothing
                                , qprjId = Nothing
                                , qprjNum = Nothing
                                , qsetPrice = Just 1500.00
                                , qcust = testCustomer
                                , qtotal = 0.00
                                , qisWrapped = False
                                , qlineItems = []
                                }

testOrder :: Order
testOrder = Order { oid = 0
                  , otype = "FL"
                  , opo = "PO 123"
                  , ocreated = Nothing
                  , ocust = testCustomer
                  , onum = "213456"
                  , ototal = 0.00
                  , oisWrapped = False
                  , oisBilled = False
                  , oprojectId = Nothing
                  , osectionId = Nothing
                  , oisProgressiveBill = False
                  , oisComplete = False
                  , olineItems = []
                  }

testProject :: Quote -> Text -> Int -> Project
testProject q pname appid = Project { prjid = 0
                                    , prjappId = appid
                                    , prjname = pname
                                    , prjpo = "PO 123"
                                    , prjcustomer = testCustomer
                                    , prjtotEstWeight = 1000
                                    , prjisActive = True
                                    , prjquoteId = qid q
                                    , prjquoteNumber = qnum q
                                    , prjextras = []
                                    , prjorders = []
                                    , prjsections = []
                                    }

testRlis :: Text -> [RawLineItem]
testRlis on = [ RawLineItem { rli_customer = "Andy J. Egan Co., Inc."
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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
                            , rli_orderNumber = on
                            , rli_po = "PO 123"
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

