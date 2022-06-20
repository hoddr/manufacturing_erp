module Client.Auth
  ( NewUser(..)
  , addUser
  , deleteUser
  , editUser
  , listUsers
  , login
  , logout
  , testUser
  , testUser2
  , testUsername
  , testUserPassword
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Data.ByteString ( ByteString )
import Data.Text.Encoding ( decodeUtf8 )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( NewUser(..)
                , Token(..)
                , User(..)
                )
import qualified Client.Common as CC

listUsers :: IO [User]
listUsers = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "users")
             NoReqBody
             lbsResponse
             CC.opts
  let eus = (eitherDecode (responseBody res) :: Either String [User]) in
    case eus of
         (Left e) -> liftIO (print e) >> pure []
         (Right us) -> pure us

addUser :: NewUser -> IO (Maybe User)
addUser u = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "users")
             (ReqBodyJson u)
             lbsResponse
             CC.opts
  let eu = (eitherDecode (responseBody res) :: Either String User) in
    case eu of
         (Left e) -> liftIO (pure e) >> pure Nothing
         (Right newU) -> liftIO (CC.printJsonRes newU) >> pure (Just newU)

editUser :: Int -> NewUser -> IO (Maybe User)
editUser uid u = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "users" /~ uid)
             (ReqBodyJson u)
             lbsResponse
             CC.opts
  let eu = (eitherDecode (responseBody res) :: Either String User) in
    case eu of
         (Left e) -> liftIO (pure e) >> pure Nothing
         (Right editedU) -> pure $ Just editedU

deleteUser :: Int -> IO ()
deleteUser uid = liftIO $ runReq defaultHttpConfig $ do
  _ <- req DELETE
           (http CC.host /: "users" /~ uid)
           NoReqBody
           lbsResponse
           CC.opts
  pure ()

login :: ByteString -> ByteString -> IO (Maybe Token)
login uname upass = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "login")
             NoReqBody
             lbsResponse
             (CC.loginOpts uname upass)
  let etk = (eitherDecode (responseBody res) :: Either String Token) in
    case etk of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right t) -> pure $ Just t

logout :: Token -> IO ()
logout tk = liftIO $ runReq defaultHttpConfig $ do
  _ <- req PUT
           (http CC.host /: "logout")
           NoReqBody
           lbsResponse
           (CC.logoutOpts $ tknuuid tk)
  pure ()

testUsername :: ByteString
testUsername = "foobar123"

testUserPassword :: ByteString
testUserPassword = "testpassword123"

testUser :: NewUser
testUser = NewUser $ User { userid = 5555
                          , userfirst = "foo"
                          , userlast = "bar"
                          , userusername = "foobarbiz"
                          , userpassword = decodeUtf8 testUserPassword
                          , userrole = "OFFICE"
                          }

testUser2 :: NewUser
testUser2 = NewUser $ User { userid = 5151
                           , userfirst = "foo"
                           , userlast = "bar123"
                           , userusername = decodeUtf8 testUsername
                           , userpassword = decodeUtf8 testUserPassword
                           , userrole = "OFFICE"
                           }
