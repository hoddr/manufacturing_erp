module Spec.Common
  ( isHttpException
  )
  where

import Network.HTTP.Req ( HttpException(..) )
import Test.Hspec ( Selector )

isHttpException :: Selector HttpException
isHttpException (VanillaHttpException _) = True
isHttpException (JsonHttpException _) = True
