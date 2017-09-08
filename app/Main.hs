module Main (main) where

import Control.Applicative ((<|>))
import Data.List (intercalate,sort)
import Data.Time.Format (defaultTimeLocale,formatTime,parseTimeM)
import Data.Time.LocalTime (LocalTime)
import Network.Curl (CurlCode(CurlOK),curlGetString)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,stderr)
import Text.HTML.Scalpel (scrapeStringLike)
import Text.Printf (printf)

import VisaBulletinScraper.Scrapers (extractTables,pageScraper,pageScraper2)
import VisaBulletinScraper.Types (Table(..),VisaAvailability(..))

-- Options and Constants
visaBulletinFile = "VisaBulletin.csv"
visaBulletinFileUnpivoted = "VisaBulletinUnpivoted.csv"

applyEb23Extractor = False    -- keep EB2 and EB3 rows only
applyChinaExtractor = False   -- keep China column only
applyDateReformatter = True   -- 01MAR13 -> 03/01/2013

urlFormat = "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/%d/visa-bulletin-for-%s-%d.html"
years = [2012..2017]
months = ["october","november","december"
         ,"january","february","march"
         ,"april","may","june"
         ,"july","august","september"
         ]

-- Main
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of
    ["-y",year,"-m",month] -> scrapePageThenWriteFiles [read year] [month] True
    []                     -> scrapePageThenWriteFiles years months  False
    _                      -> hPutStrLn stderr ("Usage: " ++ prog ++ " [-y year -m month]") >>
                              hPutStrLn stderr ("Examples:") >>
                              hPutStrLn stderr ("    " ++ prog ++ " -y 2017 -m october    # fetch 10/2017 and append it to data files") >>
                              hPutStrLn stderr ("    " ++ prog ++ "                       # fetch everything and overwrite the data files") >>
                              exitFailure

scrapePageThenWriteFiles :: [Int] -> [String] -> Bool -> IO ()
scrapePageThenWriteFiles years months shouldAppend = do
  tables <- concat <$> mapM scrapePage [(y,m) | y <- years,m <- months] :: IO [Table]
  refinedTables <- (return . sort . fmap tableTransformer) tables :: IO [Table]
  let output = if shouldAppend then appendFile else writeFile
  ( output visaBulletinFile .
    concatMap ((++ "\n\n") . show))
    refinedTables
  ( output visaBulletinFileUnpivoted .
    concatMap ((++ "\n") . show) .
    (concatMap unpivotTable))
    refinedTables

scrapePage :: (Int,String) -> IO [Table]
scrapePage (year,month) =
  let fiscalYear :: Int
      fiscalYear = if elem month ["october","november","december"]
                   then year+1
                   else year
      url :: String
      url = printf urlFormat fiscalYear month year
      parseTime :: String -> Maybe LocalTime
      parseTime = parseTimeM False defaultTimeLocale "%C%y %B %d"
      yearMonth :: LocalTime
      yearMonth =
        case parseTime (intercalate " " [show year, month, "01"]) of
          Just yearMonth -> yearMonth
          Nothing        -> error "Couldn't parse date."
  in do
    (code,content)       <- curlGetString url []
    maybePageContent     <- return (if code == CurlOK
                                    then Just content
                                    else Nothing) :: IO (Maybe String)
    employmentHtmlTables <- return $ do
      content <- maybePageContent :: Maybe String
      scrapeStringLike content pageScraper <|> scrapeStringLike content pageScraper2
    (return . extractTables yearMonth) employmentHtmlTables

-- Transformers
unpivotTable :: Table -> [VisaAvailability]
unpivotTable (Table yearMonth tType rows) =
  let
    countries :: [String]
    countries = (tail . head) rows
    convertRow :: [String] -> [VisaAvailability]
    convertRow (category:dates) =
      (\(c,d) -> VisaAvailability yearMonth tType category c d) <$> (zip countries dates)
    convertRow _ = error "Empty row!"
  in concatMap convertRow (tail rows)

tableTransformer :: Table -> Table
tableTransformer =
  (if applyEb23Extractor   then eb23Extractor   else id) .
  (if applyChinaExtractor  then chinaExtractor  else id) .
  (if applyDateReformatter then dateReformatter else id)


eb23Extractor :: Table -> Table
eb23Extractor (Table yearMonth tType rows)
  = Table yearMonth tType (selectElementsByIndices [0,2,3] rows)

chinaExtractor :: Table -> Table
chinaExtractor (Table yearMonth tType rows)
  = Table yearMonth tType ((selectElementsByIndices [0,2]) <$> rows)

dateReformatter :: Table -> Table
dateReformatter (Table yearMonth tType rows)
  = Table yearMonth tType (fmap reformatDate <$> rows)
  where reformatDate :: String -> String
        reformatDate d = case parseTime d of
          Just date -> formatTime defaultTimeLocale "%D" date
          Nothing -> d
        parseTime :: String -> Maybe LocalTime
        parseTime = parseTimeM False defaultTimeLocale "%d%h%y"

selectElementsByIndices :: [Int] -> [a] -> [a]
selectElementsByIndices indices = fmap snd . filter (\(i,_) -> elem i indices) . zip [0..]

