import Control.Applicative ((<|>))
import Data.List (intercalate,isInfixOf,sort)
import Data.List.Split (chunksOf)
import Data.String.Utils (strip)
import Data.Time.Format (defaultTimeLocale,formatTime,parseTimeM)
import Data.Time.LocalTime (LocalTime)
import Network.Curl (CurlCode(CurlOK),curlGetString)
import Text.HTML.Scalpel
  (AttributeName(AttributeString),Scraper,TagName(TagString)
  ,chroot,hasClass,htmls,match,scrapeStringLike,tagSelector,texts
  ,(@=),(@:),(//))
import Text.Printf (printf)

-- Options
visaBulletinFile = "VisaBulletin.csv"
visaBulletinFileUnpivoted = "VisaBulletinUnpivoted.csv"

csvSeparator = "|"

applyEb23Extractor = False    -- keep EB2 and EB3 rows only
applyChinaExtractor = False   -- keep China column only
applyDateReformatter = True   -- 01MAR13 -> 03/01/2013

urlFormat = "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/%d/visa-bulletin-for-%s-%d.html"
fiscalYears = [2012,2013,2014,2015,2016,2017]
months = ["october","november","december"
         ,"january","february","march"
         ,"april","may","june"
         ,"july","august","september"
         ]

--
tableContentTransformer :: TableContent -> TableContent
tableContentTransformer =
  (if applyEb23Extractor then eb23Extractor else id) .
  (if applyChinaExtractor then chinaExtractor else id) .
  (if applyDateReformatter then dateReformatter else id)

data TableType = TypeA | TypeB deriving (Eq,Ord,Show)
data TableContent = TableContent LocalTime TableType [[String]] deriving (Eq,Ord)

instance Show TableContent where
  show (TableContent yearMonth tType content) =
    intercalate csvSeparator [formatTime defaultTimeLocale "%D" yearMonth,show tType] ++ "\n" ++
    (intercalate "\n" . map (intercalate csvSeparator)) content

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

unpivotTableContent :: TableContent -> [VisaAvailability]
unpivotTableContent (TableContent yearMonth tType content) =
  let
    countries :: [String]
    countries = (tail . head) content
    convertRow :: [String] -> [VisaAvailability]
    convertRow (category:dates) =
      map (\(c,d) -> VisaAvailability yearMonth tType category c d) (zip countries dates)
    convertRow _ = error "Empty row!"
  in concatMap convertRow (tail content)

main :: IO ()
main = mapM scrapePage [(y,m) | y <- fiscalYears,m <- months] >>=
  return . sort . (map tableContentTransformer) . concat >>= \tables ->
  (writeFile visaBulletinFile . intercalate "\n\n" . map show) tables >>
  (writeFile visaBulletinFileUnpivoted . intercalate "\n" . map show . (concatMap unpivotTableContent)) tables

scrapePage :: (Int,String) -> IO [TableContent]
scrapePage (fiscalYear,month) =
  let
    year :: Int
    year = if elem month ["october","november","december"] then fiscalYear-1 else fiscalYear
    url :: String
    url = printf urlFormat fiscalYear month year
    pageContent :: IO (Maybe String)
    pageContent = curlGetString url [] >>= \(code,content) ->
      return (if code == CurlOK then Just content else Nothing)
    employmentHtmlTables :: IO (Maybe [String])
    employmentHtmlTables = pageContent >>= \maybeContent ->
      return (
        maybeContent >>= \content ->
        scrapeStringLike content pageScraper <|>
        scrapeStringLike content pageScraper2
      )
    buildYearMonth :: LocalTime
    buildYearMonth =
      case parseTimeM False defaultTimeLocale "%C%y %B %d" (intercalate " " [show year, month, "01"]) of
        Just yearMonth -> yearMonth
        Nothing -> error "No this cannot happen."
  in employmentHtmlTables >>= return . extractTables buildYearMonth

-- works for >= 2016 may
pageScraper :: Scraper String [String]
pageScraper = chroot (TagString "div" @: [hasClass "Visa_Contentpage_Category"]) tableScraper

tableScraper :: Scraper String [String]
tableScraper = htmls selector >>= return . filter (isInfixOf "Employ")
  where selector = (TagString "div" @: [hasClass "simple_richtextarea"]) //
                   (TagString "table" @: [hasClass "grid"])

-- works for < 2016 may
pageScraper2 :: Scraper String [String]
pageScraper2 = chroot (TagString "div" @: [AttributeString "id" @= "main"]) tableScraper2

tableScraper2 :: Scraper String [String]
tableScraper2 = htmls selector >>= return . filter (isInfixOf "Employ")
  where selector = (TagString "div" @: [match matcher])
                   // tagSelector "table"
        matcher "class" "visabulletinemploymenttable parbase employment_table_data" = True
        matcher "class" "third_richtext_area simple_richtextarea" = True
        matcher _ _ = True

--

extractTables :: LocalTime -> Maybe [String] -> [TableContent]
extractTables _ Nothing = []
extractTables yearMonth (Just tables)
  | length tables <= 2 =
      fmap (\(tType,table) -> extractTable yearMonth tType table) (zip [TypeA,TypeB] tables)
  | otherwise = error "Unknown situation detected!"

extractTable :: LocalTime -> TableType -> String -> TableContent
extractTable yearMonth tType table = case scrapeStringLike table cellScraper of
  Nothing -> TableContent yearMonth tType []
  Just content ->
    let cleanContent = map (filter (\x -> x /= '\n' && x /= '\160') . strip) content
        nRows = getNumRows yearMonth content
        nCols = length content `div` nRows
    in (TableContent yearMonth tType . chunksOf nCols) cleanContent

cellScraper :: Scraper String [String]
cellScraper = texts (tagSelector "tbody" // tagSelector "tr" // tagSelector "td")

getNumRows :: LocalTime -> [String] -> Int
getNumRows _ content | length content == 54 = 9
                              | length content == 48 = 8
                              | otherwise = 8
--

eb23Extractor :: TableContent -> TableContent
eb23Extractor (TableContent yearMonth tType content)
  = TableContent yearMonth tType (selectElementsByIndices [0,2,3] content)

chinaExtractor :: TableContent -> TableContent
chinaExtractor (TableContent yearMonth tType content)
  = TableContent yearMonth tType (map (selectElementsByIndices [0,2]) content)

dateReformatter :: TableContent -> TableContent
dateReformatter (TableContent yearMonth tType content)
  = TableContent yearMonth tType (map (map reformatDate) content)
  where reformatDate :: String -> String
        reformatDate d = case parseTime d of
          Just date -> formatTime defaultTimeLocale "%D" date
          Nothing -> d
        parseTime :: String -> Maybe LocalTime
        parseTime = parseTimeM False defaultTimeLocale "%d%h%y"

selectElementsByIndices :: [Int] -> [a] -> [a]
selectElementsByIndices indices = (map snd) . (filter (\(i,_) -> elem i indices)) . zip [0..]

