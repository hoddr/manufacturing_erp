{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.MarkupLabor
Description: Controllers and handles for all markup and labor rate routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all markup and labor rate routes.

-}
module Controllers.MarkupLabor
  ( -- * Markups and Labor Rates
    gMarkups
  , pLaborRate
  , pMargins
  )
  where

import Control.Monad ( (>=>) )
import Network.Wai ( Response )
import Network.HTTP.Types ( status200
                          , status201
                          )

-- LOCAL --
import APITypes ( Context(..) )
import Controllers.Utils ( errReqBody
                         , reqBodyReader
                         , sendResp
                         )
import Markups ( getLatestPricing
               , updateMarkupRates
               , updateLaborRate
               )

-- | GET List current markups and labor rates. Returns 200 on success.
gMarkups :: Context -> IO Response
gMarkups c = getLatestPricing c >>= sendResp status200

-- | POST Creates a new labor rate entry. Returns 201 on success.
pLaborRate :: Context -> IO Response
pLaborRate c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (updateLaborRate c >=> sendResp status201)

-- | POST Creates a new markup entry. Returns 201 on success.
pMargins :: Context -> IO Response
pMargins c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (updateMarkupRates c >=> sendResp status201)
