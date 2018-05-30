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
module Lib
    ( someFunc
    ) where

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)

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

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving Generic

instance Database be ShoppingCartDb

{- ------------------------------------------- -}

someFunc :: IO ()
someFunc = let u = User "john@example.com" "John" "Smith" "password!" :: User 
               userKey = (UserId "john@doe.org") :: UserId
               shoppingCartDb :: DatabaseSettings be ShoppingCartDb
               shoppingCartDb = defaultDbSettings
               allUsers = all_ (_shoppingCartUsers shoppingCartDb)
               sortUsersByFirstName = orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) allUsers
               boundedQuery = limit_ 1 $ offset_ 1 $ sortUsersByFirstName
               userCount = aggregate_ (\u -> as_ @Int countAll_) allUsers
               numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int countAll_)) allUsers
               in do
                 conn <- open "shoppingcart1.db"

                 runBeamSqliteDebug putStrLn {- for debug output -} conn $ do
                   runInsert $  
                     insert (_shoppingCartUsers shoppingCartDb) $
                     insertValues [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                                  , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                                  , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                                  , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                                  , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

                   users <- runSelectReturningList (select boundedQuery)
                   mapM_ (liftIO . putStrLn . show) users

                 
                   Just c <- runSelectReturningOne $ select userCount
                   liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")

                   countedByName <- runSelectReturningList $ select numberOfUsersByName
                   mapM_ (liftIO . putStrLn . show) countedByName


