module Spec.Auth
  ( spec
  )
  where

import Test.Hspec ( Spec
                  , describe
                  , expectationFailure
                  , it
                  , shouldThrow
                  )


-- LOCAL IMPORTS --
import qualified Client.Auth as CA
import Spec.Common ( isHttpException )


spec :: Spec
spec = do
  describe "Auth" $ do
    it "user add -> delete should work" $ do
      res <- CA.addUser CA.testUser
      case res of
           Nothing -> expectationFailure "Failed to create new user"
           _ -> do
             CA.deleteUser 3
             pure ()
    it "should edit a user" $ do
      _ <- CA.addUser CA.testUser
      us <- CA.listUsers
      res <- CA.editUser 4 $ CA.NewUser $ last us
      case res of
           Nothing -> expectationFailure "Failed to edit user"
           _ -> pure ()
    it "should throw HttpException if user body id mismatch with sent uri id" $ do
      CA.editUser (-3) CA.testUser `shouldThrow` isHttpException
    it "login and logout should work with new user" $ do
      _ <- CA.addUser CA.testUser2
      mtk <- CA.login CA.testUsername CA.testUserPassword
      case mtk of
           Nothing -> expectationFailure "Failed to login"
           (Just tk) -> do
             CA.logout tk
             pure ()
