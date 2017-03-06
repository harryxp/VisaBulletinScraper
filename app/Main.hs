import Control.Applicative ((<|>))
import Data.List (intercalate,isInfixOf)
import Data.List.Split (chunksOf)
import Data.String.Utils (strip)
import Network.Curl (CurlCode(CurlOK), curlGetString)
import Text.HTML.Scalpel
  (AttributeName(AttributeString),Scraper,TagName(TagString)
  ,chroot,hasClass,htmls,match,scrapeStringLike,tagSelector,texts
  ,(@=),(@:),(//))
import Text.Printf (printf)

csvSeparator = ","
extractEb23Only = False
extractChinaOnly = False

urlFormat = "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/%d/visa-bulletin-for-%s-%d.html"
fiscalYears = [2012,2013,2014,2015,2016,2017]
months = ["october","november","december"
         ,"january","february","march"
         ,"april","may","june"
         ,"july","august","september"
         ]

data TableContent = TableContent Int String [[String]]

instance Show TableContent where
  show (TableContent year month content) = show year ++ csvSeparator ++ month ++ "\n" ++
    (intercalate "\n" . map (intercalate csvSeparator)) content

main :: IO ()
main = mapM scrapePage [(y,m) | y <- fiscalYears,m <- months] >>=
  putStrLn . intercalate "\n\n" . map (show . chinaFilter . eb23Filter) . concat

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
extractTables year month (Just tables) = fmap (extractTable year month) tables

extractTable :: Int -> String -> String -> TableContent
extractTable year month table = case scrapeStringLike table cellScraper of
  Nothing -> TableContent year month []
  Just content ->
    let cleanContent = map (filter (\x -> x /= '\n' && x /= '\160') . strip) content
        nRows = getNumRows year month content
        nCols = length content `div` nRows
    in (TableContent year month . chunksOf nCols) cleanContent

cellScraper :: Scraper String [String]
cellScraper = texts (tagSelector "tbody" // tagSelector "tr" // tagSelector "td")

getNumRows :: Int -> String -> [String] -> Int
getNumRows year month content | length content == 54 = 9
                              | length content == 48 = 8
                              | otherwise = 8
--

eb23Filter :: TableContent -> TableContent
eb23Filter tc@(TableContent year month content)
  | extractEb23Only = TableContent year month (selectElementsByIndices [0,2,3] content)
  | otherwise = tc

chinaFilter :: TableContent -> TableContent
chinaFilter tc@(TableContent year month content)
  | extractChinaOnly = TableContent year month (map (selectElementsByIndices [0,2]) content)
  | otherwise = tc

selectElementsByIndices :: [Int] -> [a] -> [a]
selectElementsByIndices indices = (map snd) . (filter (\(i,_) -> elem i indices)) . zip [0..]

