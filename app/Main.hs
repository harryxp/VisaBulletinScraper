import Data.List (intercalate,isInfixOf)
import Data.List.Split (chunksOf)
import Text.HTML.Scalpel
  (Scraper,TagName(TagString)
  ,chroot,hasClass,htmls,scrapeStringLike,scrapeURL,tagSelector,texts
  ,(@:),(//))
import Text.Printf (printf)

urlFormat = "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/%d/visa-bulletin-for-%s-%d.html"
fiscalYears = [2014,2015,2016,2017]
months = ["october","november","december","january","february","march","april","may","june","july","august","september"]
--fiscalYears = [2017]
--months = ["march"]

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
    employmentHtmlTables = scrapeURL url pageScraper
  in employmentHtmlTables >>= return . extractTables year month

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
    let cleanContent = map (filter (\x -> x /= '\n' && x /= '\160')) content
        nRows = 9
        nCols = length content `div` nRows
    in TableContent year month $ chunksOf nCols cleanContent

cellScraper :: Scraper String [String]
cellScraper = texts (tagSelector "tbody" // tagSelector "tr" // tagSelector "td")

