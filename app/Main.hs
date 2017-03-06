import Control.Applicative ((<|>))
import Data.List (intercalate,isInfixOf)
import Data.List.Split (chunksOf)
import Data.String.Utils (strip)
import Data.Time.Format (defaultTimeLocale,formatTime,parseTimeM)
import Data.Time.LocalTime (LocalTime)
import Network.Curl (CurlCode(CurlOK), curlGetString)
import Text.HTML.Scalpel
  (AttributeName(AttributeString),Scraper,TagName(TagString)
  ,chroot,hasClass,htmls,match,scrapeStringLike,tagSelector,texts
  ,(@=),(@:),(//))
import Text.Printf (printf)

-- Options
csvSeparator = "|"

useEb23Extractor = True     -- keep EB2 and EB3 rows only
useChinaExtractor = True    -- keep China column only
useDateReformatter = True   -- 01MAR13 -> 03/01/2013

urlFormat = "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/%d/visa-bulletin-for-%s-%d.html"
fiscalYears = [2012,2013,2014,2015,2016,2017]
months = ["october","november","december"
         ,"january","february","march"
         ,"april","may","june"
         ,"july","august","september"
         ]

--
tableContentTransformer =
  (if useEb23Extractor then eb23Extractor else id) .
  (if useChinaExtractor then chinaExtractor else id) .
  (if useDateReformatter then dateReformatter else id)

data TableType = TypeA | TypeB
  deriving Show
data TableContent = TableContent Int String TableType [[String]]

instance Show TableContent where
  show (TableContent year month ttype content) = intercalate csvSeparator [show year, month, show ttype] ++ "\n" ++
    (intercalate "\n" . map (intercalate csvSeparator)) content

main :: IO ()
main = mapM scrapePage [(y,m) | y <- fiscalYears,m <- months] >>=
  putStrLn . intercalate "\n\n" . map (show . tableContentTransformer) . concat

scrapePage :: (Int,String) -> IO [TableContent]
scrapePage (fiscalYear,month) =
  let
    year :: Int
    year = if elem month ["october","november","december"] then fiscalYear-1 else fiscalYear
    url :: String
    url = printf urlFormat fiscalYear month year
    pageContent :: IO (Maybe String)
    pageContent = curlGetString url [] >>= \(code, content) ->
      return (if code == CurlOK then Just content else Nothing)
    employmentHtmlTables :: IO (Maybe [String])
    employmentHtmlTables = pageContent >>= \maybeContent ->
      return (
        maybeContent >>= \content ->
        scrapeStringLike content pageScraper <|>
        scrapeStringLike content pageScraper2
      )
  in employmentHtmlTables >>= return . extractTables year month

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

extractTables :: Int -> String -> Maybe [String] -> [TableContent]
extractTables _ _ Nothing = []
extractTables year month (Just tables)
  | length tables <= 2 = fmap (\(ttype, table) -> extractTable year month ttype table) (zip [TypeA, TypeB] tables)
  | otherwise = error "Unknown situation detected!"

extractTable :: Int -> String -> TableType -> String -> TableContent
extractTable year month ttype table = case scrapeStringLike table cellScraper of
  Nothing -> TableContent year month ttype []
  Just content ->
    let cleanContent = map (filter (\x -> x /= '\n' && x /= '\160') . strip) content
        nRows = getNumRows year month content
        nCols = length content `div` nRows
    in (TableContent year month ttype . chunksOf nCols) cleanContent

cellScraper :: Scraper String [String]
cellScraper = texts (tagSelector "tbody" // tagSelector "tr" // tagSelector "td")

getNumRows :: Int -> String -> [String] -> Int
getNumRows year month content | length content == 54 = 9
                              | length content == 48 = 8
                              | otherwise = 8
--

eb23Extractor :: TableContent -> TableContent
eb23Extractor (TableContent year month ttype content)
  = TableContent year month ttype (selectElementsByIndices [0,2,3] content)

chinaExtractor :: TableContent -> TableContent
chinaExtractor (TableContent year month ttype content)
  = TableContent year month ttype (map (selectElementsByIndices [0,2]) content)

dateReformatter :: TableContent -> TableContent
dateReformatter (TableContent year month ttype content)
  = TableContent year month ttype (map (map reformatDate) content)
  where reformatDate :: String -> String
        reformatDate d = case parseTime d of
          Just date -> formatTime defaultTimeLocale "%D" date
          Nothing -> d
        parseTime :: String -> Maybe LocalTime
        parseTime = parseTimeM False defaultTimeLocale "%d%h%y"

selectElementsByIndices :: [Int] -> [a] -> [a]
selectElementsByIndices indices = (map snd) . (filter (\(i,_) -> elem i indices)) . zip [0..]

