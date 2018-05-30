{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-#  LANGUAGE ImpredicativeTypes #-}
{-#  LANGUAGE NoMonomorphismRestriction #-}

module Lib
    ( someFunc
    ) where

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)

import Control.Lens

import Database.SQLite.Simple

data UserT f
    = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

{- ------------------------------------------- -}

data AddressT f = Address
                { _addressId    :: C f Int
                , _addressLine1 :: C f Text
                , _addressLine2 :: C f (Maybe Text)
                , _addressCity  :: C f Text
                , _addressState :: C f Text
                , _addressZip   :: C f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving Generic
type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int) deriving Generic
    primaryKey = AddressId . _addressId
type AddressId = PrimaryKey AddressT Identity -- For convenience

instance Beamable AddressT
instance Beamable (PrimaryKey AddressT)

{- ------------------------------------------- -}

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) 
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT) }
                        deriving Generic

instance Database be ShoppingCartDb

{- ------------------------------------------- -}

someFunc :: IO ()
someFunc = let 

               Address (LensFor addressId)    (LensFor addressLine1)
                       (LensFor addressLine2) (LensFor addressCity)
                       (LensFor addressState) (LensFor addressZip)
                       (UserId (LensFor addressForUserId)) =
                       tableLenses

               User (LensFor userEmail)    (LensFor userFirstName)
                    (LensFor userLastName) (LensFor userPassword) =
                    tableLenses

               ShoppingCartDb (TableLens shoppingCartUsers)
                              (TableLens shoppingCartUserAddresses) =
                              dbLenses

               shoppingCartDb :: DatabaseSettings be ShoppingCartDb
               shoppingCartDb = defaultDbSettings `withDbModification`
                                dbModification {
                                  _shoppingCartUserAddresses =
                                    modifyTable (\_ -> "addresses") $
                                    tableModification {
                                      _addressLine1 = fieldNamed "address1",
                                      _addressLine2 = fieldNamed "address2"
                                    }
                                }

               james = User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
               betty = User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
               sam = User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"

               addresses = [ 
                    Address default_ (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (pk james)
                  , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (pk betty)
                  , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (pk betty) ]

               allUsers = all_ (_shoppingCartUsers shoppingCartDb)
               sortUsersByFirstName = orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) allUsers
               boundedQuery = limit_ 1 $ offset_ 1 $ sortUsersByFirstName
               userCount = aggregate_ (\u -> as_ @Int countAll_) allUsers
               numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int countAll_)) allUsers

               in do
                 conn <- open "shoppingcart1.db"

                 runBeamSqliteDebug putStrLn {- for debug output -} conn $ do
{-
                   runInsert $  
                     insert (_shoppingCartUsers shoppingCartDb) $
                     insertValues [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                                  , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                                  , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                                  , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                                  , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

                   Just c <- runSelectReturningOne $ select userCount
                   liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")

                   countedByName <- runSelectReturningList $ select numberOfUsersByName
                   mapM_ (liftIO . putStrLn . show) countedByName

                   users <- runSelectReturningList (select boundedQuery)
                   mapM_ (liftIO . putStrLn . show) users

                   liftIO $ putStrLn "======================================"
-}
                   runInsert $  
                     insert (_shoppingCartUsers shoppingCartDb) $
                     insertValues [ james, betty, sam ]

                   runInsert $
                     insert (_shoppingCartUserAddresses shoppingCartDb) $
                     insertExpressions addresses

{- !!!! It's OK -}
                   addresses' <- runSelectReturningList $ select (all_ (_shoppingCartUsers shoppingCartDb))
                   liftIO $ mapM_ print addresses'

                   liftIO $ putStrLn "======================================"

{- 
!!! It's wrong: 

SELECT "t0"."id" AS "res0", "t0"."address1" AS "res1", "t0"."address2" AS "res2", "t0"."city" AS "res3", "t0"."state" AS "res4", "t0"."zip" AS "res5", "t0"."for_user__email" AS "res6" FROM "addresses" AS "t0";
-- With values: []
simple-exe: ConversionFailed {errSQLType = "NULL", errHaskellType = "Int", errMessage = "need an int"}

-}
                   addresses'' <- runSelectReturningList $ select (all_ (shoppingCartDb ^. shoppingCartUserAddresses))
                   liftIO $ mapM_ print addresses''



