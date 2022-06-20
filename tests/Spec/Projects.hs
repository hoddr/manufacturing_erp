module Spec.Projects
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
import qualified Client.Projects as CP
import qualified Client.Quotes as CQ
import Spec.Common ( isHttpException )


spec :: Spec
spec = do
  describe "Projects" $ do
    describe "'add' and delete project" $ do
      it "should come from quote" $ do
        res <- CQ.addQuoteNoLineItems $ CQ.testQuoteNoLineItems "XX-123"
        case res of
             Nothing -> expectationFailure "Failed to add quote w/o line items"
             _ -> do
               qs <- CQ.listQuotes
               mp <- CQ.quoteToProject (head qs) $ CQ.testProject (head qs) "POE 321" 6013
               case mp of
                    Nothing -> expectationFailure "Failed to turn quote into project"
                    Just p -> do
                      _ <- CP.deleteProject p
                      pure ()
    describe "listProjects" $ do
      it "should return list without failure" $ do
        _ <- CP.listProjects
        pure ()
    describe "edit project" $ do
      it "should edit project without failure" $ do
        ps <- CP.listProjects
        res <- CP.editProject (last ps)
        case res of
             Nothing -> expectationFailure "Failed to edit project"
             _ -> pure ()
    describe "get individual project" $ do
      it "should grab a project that exists" $ do
        ps <- CP.listProjects
        mp <- CP.getProject (last ps)
        case mp of
             Nothing -> expectationFailure "Failed to get project by id"
             _ -> pure ()
      it "should throw HttpException if retrieving by id that doesn't exist" $ do
        CP.getProjectById (-3) `shouldThrow` isHttpException
  describe "Project Sections" $ do
    describe "add and delete section" $ do
      it "should work for valid section and valid project" $ do
        ps <- CP.listProjects
        mps <- CP.addSection (last ps) $ CP.testSection (last ps)
        case mps of
             Nothing -> expectationFailure "Failed to create new section"
             Just newPs -> do
               _ <- CP.deleteSection (last ps) newPs
               pure ()
    describe "edit section" $ do
      it "should fail for different uri params from body" $ do
        res <- CQ.addQuoteNoLineItems $ CQ.testQuoteNoLineItems "ZZ-001"
        case res of
             Nothing -> expectationFailure "Failed to add quote w/o line items"
             _ -> do
               qs <- CQ.listQuotes
               mp <- CQ.quoteToProject (head qs) $ CQ.testProject (head qs) "POE 6135" 6135
               case mp of
                    Nothing -> expectationFailure "Failed to turn quote into project"
                    Just p -> do
                      mps <- CP.addSection p $ CP.testSection p
                      case mps of
                           Nothing -> expectationFailure "Failed to create new section"
                           Just newPs -> do
                             ps <- CP.listProjects
                             CP.editSection (last ps) newPs `shouldThrow` isHttpException
      it "should work for valid section edit" $ do
        ps <- CP.listProjects
        mps <- CP.addSection (last ps) $ CP.testSection (last ps)
        case mps of
             Nothing -> expectationFailure "Failed to create new section"
             Just newPs -> do
               meps <- CP.editSection (last ps) newPs
               case meps of
                    Nothing -> expectationFailure "Failed to edit section"
                    _ -> pure ()
    describe "categories" $ do
      it "should list categories from database list" $ do
        res <- CP.listCategories
        case res of
             [] -> expectationFailure "Returned empty list; expected non-empty list"
             _ -> pure ()
