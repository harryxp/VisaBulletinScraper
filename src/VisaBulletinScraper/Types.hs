module VisaBulletinScraper.Types (Table(..),TableType(..),VisaAvailability(..)) where

import Data.List (intercalate)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (defaultTimeLocale,formatTime)

type Row = [String]
data TableType = TypeA | TypeB deriving (Eq,Ord,Show)
data Table = Table LocalTime TableType [Row] deriving (Eq,Ord)

csvSeparator = "|"

instance Show Table where
  show (Table yearMonth tType rows) =
    intercalate csvSeparator [formatTime defaultTimeLocale "%D" yearMonth,show tType] ++ "\n" ++
    (intercalate "\n" . fmap (intercalate csvSeparator)) rows

data VisaAvailability = VisaAvailability { yearMonth :: LocalTime
                                         , tableType :: TableType
                                         , visaCategory :: String
                                         , country :: String
                                         , availability :: String
                                         }

instance Show VisaAvailability where
  show (VisaAvailability yearMonth tType visaCategory country availability) =
    intercalate csvSeparator
      [formatTime defaultTimeLocale "%D" yearMonth,show tType,visaCategory,country,availability]
