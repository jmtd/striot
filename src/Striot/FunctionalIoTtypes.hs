{-# LANGUAGE DeriveGeneric #-}
module Striot.FunctionalIoTtypes where
import           Data.Store
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)
import           Test.Framework

data Event alpha = Event { eventId :: Int
                         , time    :: Maybe Timestamp
                         , value   :: Maybe alpha}
     deriving (Eq, Ord, Show, Read, Generic)

type Timestamp       = UTCTime
type Stream alpha    = [Event alpha]

instance (Store alpha) => Store (Event alpha)

dataEvent :: Event alpha -> Bool
dataEvent (Event eid t (Just v)) = True
dataEvent (Event eid t Nothing)  = False

timedEvent :: Event alpha -> Bool
timedEvent (Event eid (Just t) v) = True
timedEvent (Event eid Nothing  v) = False

instance Arbitrary a => Arbitrary (Event a) where
    arbitrary = do
        eid <- arbitrary
        i <- arbitrary
        return $ Event eid Nothing (Just i)
