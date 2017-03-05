import Data.List (intercalate,isInfixOf)
import Data.List.Split (chunksOf)
import Data.String.Utils (strip)
import Text.HTML.Scalpel
  (AttributeName(AttributeString),Scraper,TagName(TagString)
  ,chroot,hasClass,htmls,scrapeStringLike,scrapeURL,tagSelector,texts
  ,(@=),(@:),(//))
import Text.Printf (printf)

urlFormat = "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/%d/visa-bulletin-for-%s-%d.html"
--fiscalYears = [2014,2015,2016,2017]
--months = ["october","november","december","january","february","march","april","may","june","july","august","september"]
fiscalYears = [2016]
months = ["april"]

data TableContent = TableContent Int String [[String]]

instance Show TableContent where
  show (TableContent year month content) = show year ++ "|" ++ month ++ "\n" ++
    intercalate "\n" (map (intercalate "|") content)

main :: IO ()
main = mapM scrapePage [ (y,m) | y <- fiscalYears,m <- months]
  >>= putStrLn . intercalate "\n\n" . map show . concatMap id

scrapePage :: (Int,String) -> IO [TableContent]
scrapePage (fiscalYear,month) =
  let
    year :: Int
    year = if elem month ["october","november","december"] then fiscalYear-1 else fiscalYear
    url :: String
    url = printf urlFormat fiscalYear month year
    employmentHtmlTables :: IO (Maybe [String])
    employmentHtmlTables = scrapeURL url pageScraper >>= \tables -> case tables of
      Nothing -> scrapeURL url pageScraper2
      v -> return v
  in employmentHtmlTables >>= return . extractTables year month

-- used for >= 2016 may
pageScraper :: Scraper String [String]
pageScraper = chroot (TagString "div" @: [hasClass "Visa_Contentpage_Category"]) tableScraper

tableScraper :: Scraper String [String]
tableScraper = htmls selector >>= return . filter (isInfixOf "Employ")
  where selector = (TagString "div" @: [hasClass "simple_richtextarea"]) //
                   (TagString "table" @: [hasClass "grid"])

extractTables :: Int -> String -> Maybe [String] -> [TableContent]
extractTables _ _ Nothing = []
extractTables year month (Just tables) = fmap (extractTable year month) tables

extractTable :: Int -> String -> String -> TableContent
extractTable year month table = case scrapeStringLike table cellScraper of
  Nothing -> TableContent year month []
  Just content ->
    let cleanContent = map (filter (\x -> x /= '\n' && x /= '\160') . strip) content
        nRows = 9
        nCols = length content `div` nRows
    in TableContent year month $ chunksOf nCols cleanContent

cellScraper :: Scraper String [String]
cellScraper = texts (tagSelector "tbody" // tagSelector "tr" // tagSelector "td")

-- used for < 2016 may
pageScraper2 :: Scraper String [String]
pageScraper2 = chroot (TagString "div" @: [AttributeString "id" @= "main"]) tableScraper2

tableScraper2 :: Scraper String [String]
tableScraper2 = htmls selector >>= return . filter (isInfixOf "Employ")
  where selector = (TagString "div" @: [hasClass "visabulletinemploymenttable"])
                   // tagSelector "table"

