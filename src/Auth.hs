{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Authorization, Users, Tokens
Description: Login/logout, user management and permissions, authentication tokens.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Functions related to authorization, users, user permissions, and auth tokens. Also contains functions
for web UI/UX 'login' and 'logout'. User passwords are encrypted with BCrypt prior to storage in the database.

Authorization tokens are simple UUID V4s and leverage the Authorization: Bearer <token> header in HTTP requests. User login is done via Basic Auth. All requests should/must be done over HTTPS to help ensure security.

User permissions are set at a 0, 1, 2, or 3 level, with 0 being none, 1 being "read-only", 2 being "read + write", and 3 being "read + write + special", where special is often delete or other non-standard actions. New users and their permission can be set via the web UI/UX.

-}

module Auth
  ( -- * Users and Permissions
    addUser
  , editUser
  , editUserPassword
  , editUserPermissions
  , getUserByUsername
  , getUserPermissions
  , listUsers
  , removeUser
  , verifyPermissions
  -- * Tokens
  , checkToken
  , removeUserTokens
  -- * Login/Logout
  , login
  , logout
  )
  where


import Crypto.KDF.BCrypt ( hashPassword
                         , validatePasswordEither
                         )
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )
import Database.PostgreSQL.Simple.ToField ( ToField )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8
                          , encodeUtf8
                          )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Context(..)
                , PermissionSet(..)
                , Token(..)
                , User(..)
                , UserPermissions(..)
                )
import Database ( runExecute
                , runQuery
                , runQuery_
                )
import Utils ( apiMsg200
             , apiMsg204
             , badReq
             , eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Creates and returns a new bearer token for the provided user.
addToken :: Context -> User -> IO (Either APIError Token)
addToken c u = runQuery c insertTokenSql [ userid u ]
  >>= eitherPassErr (gen (\_ nid -> nid)
                              "Failure to return new token id"
                              (123 :: Int) -- placeholder
                    )
  >>= eitherPassErr (getNewToken c)

-- | Query to retrieve valid token by id.
getNewToken :: Context -> Int -> IO (Either APIError Token)
getNewToken c tid = runQuery c (baseTokenSql <> " where tokens.id = ?") [tid]
                  >>= eitherPassErr (wasFound "New token not returned")

-- | Verifies valid token and returns both token and user permissions.
checkToken :: Context -> Text -> IO (Either APIError (Token, UserPermissions))
checkToken c token = runQuery c (baseTokenSql <> " where tokens.token = ?") [token]
  >>= eitherPassErr (wasFound "Unauthorized")
  >>= eitherPassErr (\t -> getUserPermissions c (userid $ tknuser t)
    >>= eitherPassErr (\up -> passSuccess (t, up)))

-- | Deletes all tokens for the given user.
removeUserTokens :: Context -> Token -> IO (Either APIError ())
removeUserTokens c t = runExecute c removeUserTokensSql [ userid $ tknuser t ]
                     >>= eitherPassErr (\_ -> passSuccess ())

-- | Creates new user entry, encoding the password for more secure storage. Also creates default user permission entry.
addUser :: Context -> User -> IO (Either APIError User)
addUser c u = do
  hash <- hashPassword 12 (encodeUtf8 $ userpassword u) :: IO BS.ByteString
  runQuery c insertUserSql ( userfirst u
                           , userlast u
                           , userusername u
                           , decodeUtf8 hash
                           , userrole u
                           )
  >>= eitherPassErr (gen (\us nid -> us { userid = nid })
                              "Failure to return new user id"
                              u
                    )
  >>= eitherPassErr (addUserPermissions c)

-- | Alters the user information.
editUser :: Context -> User -> Int -> IO (Either APIError User)
editUser c u uid
  | userid u == uid =
    runExecute c updateUserSql ( userfirst u
                               , userlast u
                               , userusername u
                               , userrole u
                               , userid u
                               )
    >>= eitherPassErr (\_ -> passSuccess u)
  | otherwise = badReq 400 "User uri id and body id mismatch"

-- | Updates the user password.
editUserPassword :: Context -> User -> IO (Either APIError User)
editUserPassword c u
  | userpassword u /= "" =
    (hashPassword 12 (encodeUtf8 $ userpassword u) :: IO BS.ByteString)
    >>= (\hash -> runExecute c updateUserPasswordSql ( decodeUtf8 hash, userid u )
        >>= eitherPassErr (\_ -> passSuccess u))
  | otherwise = passSuccess u

-- | Retrieves user by username.
getUserByUsername :: Context -> Text -> IO (Either APIError User)
getUserByUsername c n = getUserBy c n " where users.username = ?"

-- | Retrieves user by query type (higher-order function). Returns 404 on no results.
getUserBy :: ToField a => Context -> a -> Query -> IO (Either APIError User)
getUserBy c x filterSql = runQuery c (baseUserSql <> filterSql) [x]
                        >>= eitherPassErr (wasFound "User not found")

-- | List users. No querystring search/paginations are provided.
listUsers :: Context -> IO (Either APIError [User])
listUsers c = runQuery_ c (baseUserSql <> sortUserSql)

-- | Deletes the user.
removeUser :: Context -> Int -> IO (Either APIError APIMsg)
removeUser c uid = runExecute c permDelSql [uid]
  >>= eitherPassErr (\_ -> runExecute c dropUserSql [uid])
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "User deleted")

-- | Login. Retrieves user, checks password, generates and passes new token.
login :: Context -> BS.ByteString -> BS.ByteString -> IO (Either APIError Token)
login c username password = getUserByUsername c (decodeUtf8 username)
  >>= eitherPassErr (\u ->
    let ep = validatePasswordEither password (encodeUtf8 $ userpassword u) in
      case ep of
           (Left s) -> print s >> badReq 401 "Incorrect username or password"
           (Right True) -> addToken c u
           (Right False) -> badReq 401 "Incorrect username or password")

-- | Logout. Removes all user tokens to enforce logout.
logout :: Context -> Token -> IO (Either APIError APIMsg)
logout c t = removeUserTokens c t
           >>= eitherPassErr (\_ -> passSuccess $ apiMsg200 "Logout successful")

-- | Retrieves user permissions for given user.
getUserPermissions :: Context -> Int -> IO (Either APIError UserPermissions)
getUserPermissions ctx usid = runQuery ctx permBaseSql [usid]
  >>= eitherPassErr (wasFound "User (id) not found")

-- | Edits and updates the user permission levels.
editUserPermissions :: Context -> Int -> UserPermissions -> IO (Either APIError UserPermissions)
editUserPermissions ctx usid up
  | usid /= up_userId up = badReq 400 "User uri id and permissions user id mismatch"
  | otherwise = runExecute ctx permModSql ( up_assemblies up
                                          , up_customers up
                                          , up_fabrication up
                                          , up_feedbackComplaints up
                                          , up_inventory up
                                          , up_materials up
                                          , up_orders up
                                          , up_pricing up
                                          , up_pricingLists up
                                          , up_projects up
                                          , up_purchase up
                                          , up_quotes up
                                          , up_users up
                                          , up_vendors up
                                          , up_po up
                                          , up_errLog up
                                          , up_curbCo up
                                          , usid
                                          , up_id up
                                          )
    >>= eitherPassErr (\_ -> passSuccess up)

-- | Adds user permissions with default values (all 0, no permission).
addUserPermissions :: Context -> User -> IO (Either APIError User)
addUserPermissions ctx u = runQuery ctx permAddSql [userid u]
  >>= eitherPassErr (\(_ :: [Only Int]) -> passSuccess u)

-- | Checker for verifying sufficient user permissions.
verifyPermissions :: PermissionSet -> UserPermissions -> Bool
verifyPermissions (PermissionSet (minp, fn)) up = fn up >= minp

-- SQL --
-- | Base query SQL for users.
baseUserSql :: Query
baseUserSql = "select " <> innerUserSql <> " from users"

-- | Fields to retrieve for base SQL.
innerUserSql :: Query
innerUserSql = " users.id as id, \
               \users.fname as first, \
               \users.lname as last, \
               \users.username as username, \
               \users.password as password, \
               \users.role as role"

-- | Default sort SQL.
sortUserSql :: Query
sortUserSql = " order by users.id asc"

-- | Edit/update SQL.
updateUserSql :: Query
updateUserSql = "update users set \
                \fname = ?, \
                \lname = ?, \
                \username = ?, \
                \role = ? \
                \where users.id = ?"

-- | Edit user password SQL.
updateUserPasswordSql :: Query
updateUserPasswordSql = "update users set password = ? where users.id = ?"

-- | Main insertion SQL for user. Check database for default values.
insertUserSql :: Query
insertUserSql = "insert into users (\
                \fname, \
                \lname, \
                \username, \
                \password, \
                \role \
                \) values (?, ?, ?, ?, ?) \
                \returning id"

-- | Delete user SQL.
dropUserSql :: Query
dropUserSql = "delete from users where users.id = ?"

-- | Base SQL for retrieving token.
baseTokenSql :: Query
baseTokenSql = "select \
               \tokens.id as tknid, "
            <> innerUserSql
            <> ", tokens.token as tkn, \
               \tokens.expires as exp, \
               \tokens.is_one_time as isOneTime \
               \from tokens \
               \inner join users \
               \on tokens.fk_user_id = users.id"

-- | Insertion SQL for tokens. Check database for default values.
insertTokenSql :: Query
insertTokenSql = "insert into tokens (\
                 \fk_user_id \
                 \) values (?) \
                 \returning id"

-- | Delete user token SQL.
removeUserTokensSql :: Query
removeUserTokensSql = "delete from tokens where tokens.fk_user_id = ?"

-- | Insertion SQL for new user permission set. All values default to 0.
permAddSql :: Query
permAddSql = "insert into m_user_permissions (fk_user_id) values (?) returning id"

-- | Query SQL for user permissions.
permBaseSql :: Query
permBaseSql = "select \
              \m_user_permissions.id as id, \
              \m_user_permissions.fk_user_id as userId, \
              \m_user_permissions.assemblies as assemblies, \
              \m_user_permissions.customers as customers, \
              \m_user_permissions.fabrication as fabrication, \
              \m_user_permissions.feedback_complaints as feedbackComplaints, \
              \m_user_permissions.inventory as inventory, \
              \m_user_permissions.materials as materials, \
              \m_user_permissions.orders as orders, \
              \m_user_permissions.pricing as pricing, \
              \m_user_permissions.pricing_lists as pricingLists, \
              \m_user_permissions.projects as projects, \
              \m_user_permissions.purchase as purchase, \
              \m_user_permissions.quotes as quotes, \
              \m_user_permissions.users as users, \
              \m_user_permissions.vendors as vendors, \
              \m_user_permissions.purchase_orders as pos, \
              \m_user_permissions.error_log as elog, \
              \m_user_permissions.cc as curbCo \
              \from m_user_permissions \
              \where m_user_permissions.fk_user_id = ?"

-- | Edit SQL for user permissions.
permModSql :: Query
permModSql = "update m_user_permissions set \
             \assemblies = ?, \
             \customers = ?, \
             \fabrication = ?, \
             \feedback_complaints = ?, \
             \inventory = ?, \
             \materials = ?, \
             \orders = ?, \
             \pricing = ?, \
             \pricing_lists = ?, \
             \projects = ?, \
             \purchase = ?, \
             \quotes = ?, \
             \users = ?, \
             \vendors = ?, \
             \purchase_orders = ?, \
             \error_log = ?, \
             \cc = ? \
             \where fk_user_id = ? \
             \and id = ?"

-- | User permission delete SQL.
permDelSql :: Query
permDelSql = "delete from m_user_permissions where fk_user_id = ?"
