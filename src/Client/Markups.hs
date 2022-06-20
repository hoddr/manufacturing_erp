module Client.Markups
  ( getLatestMarkups
  , getUTCTime
  , updateLabor
  , updateMargins
  , testLabor
  , testMargins
  )
  where


import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Data.Time ( UTCTime
                 , getCurrentTime
                 )
import Network.HTTP.Req
import Prelude hiding ( pi )


-- LOCAL IMPORTS --
import APITypes ( LaborRate(..)
                , Markup(..)
                , PricingInfo(..)
                )
import qualified Client.Common as CC


getLatestMarkups :: IO (Maybe PricingInfo)
getLatestMarkups = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "markups")
             NoReqBody
             lbsResponse
             CC.opts
  let epi = (eitherDecode (responseBody res) :: Either String PricingInfo) in
    case epi of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right pi) -> pure $ Just pi

updateLabor :: LaborRate -> IO (Maybe LaborRate)
updateLabor l = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "markups" /: "labor")
             (ReqBodyJson l)
             lbsResponse
             CC.opts
  let elab = (eitherDecode (responseBody res) :: Either String LaborRate) in
    case elab of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right lab) -> pure $ Just lab

updateMargins :: Markup -> IO (Maybe Markup)
updateMargins m = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "markups" /: "margins")
             (ReqBodyJson m)
             lbsResponse
             CC.opts
  let emark = (eitherDecode (responseBody res) :: Either String Markup) in
    case emark of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right mark) -> pure $ Just mark

getUTCTime :: IO UTCTime
getUTCTime = getCurrentTime

testLabor :: UTCTime -> LaborRate
testLabor utc = LaborRate { lrid = 0
                          , lrshopRate = 30.0
                          , lroverheadRate = 30.0
                          , lrrate = 60.0
                          , lrisCurrent = True
                          , lrbaseUnit = "h"
                          , lraddedAt = utc
                          }

testMargins :: UTCTime -> Markup
testMargins utc = Markup { mkupid = 0
                         , mkuppurchase = 1.30
                         , mkupfabrication = 1.35
                         , mkuprectangular = 1.35
                         , mkupround = 1.35
                         , mkupoval = 1.35
                         , mkupstock = 1.50
                         , mkupassembly = 2.00
                         , mkupmaterial = 1.30
                         , mkupisCurrent = True
                         , mkupaddedAt = Just utc
                         }
