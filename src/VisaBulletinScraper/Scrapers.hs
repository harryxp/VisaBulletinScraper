module VisaBulletinScraper.Scrapers (extractTables,pageScraper,pageScraper2) where

import Data.List (isInfixOf)
import Data.List.Split (chunksOf)
import Data.String.Utils (strip)
import Data.Time.LocalTime (LocalTime)
import Text.HTML.Scalpel
  (AttributeName(AttributeString),Scraper,TagName(TagString)
  ,chroot,hasClass,htmls,match,scrapeStringLike,tagSelector,texts
  ,(@=),(@:),(//))

import VisaBulletinScraper.Types (Table(..),TableType(..))

-- works for >= 2016 May
pageScraper :: Scraper String [String]
pageScraper = chroot (TagString "div" @: [hasClass "Visa_Contentpage_Category"]) tableScraper

tableScraper :: Scraper String [String]
tableScraper = htmls selector >>= return . filter (isInfixOf "Employ")
  where selector = (TagString "div" @: [hasClass "simple_richtextarea"]) //
                   (TagString "table" @: [hasClass "grid"])

-- works for >= 2012 April && < 2016 May
pageScraper2 :: Scraper String [String]
pageScraper2 = chroot (TagString "div" @: [AttributeString "id" @= "main"]) tableScraper2

tableScraper2 :: Scraper String [String]
tableScraper2 = htmls selector >>= return . filter (isInfixOf "Employ")
  where selector = (TagString "div" @: [match matcher]) // tagSelector "table"
        matcher "class" "visabulletinemploymenttable parbase employment_table_data" = True
        matcher "class" "third_richtext_area simple_richtextarea" = True
        matcher _ _ = True

--
extractTables :: LocalTime -> Maybe [String] -> [Table]
extractTables _ Nothing = []
extractTables yearMonth (Just tables)
  | length tables <= 2 =
      (\(tType,table) -> extractTable yearMonth tType table) <$> zip [TypeA,TypeB] tables
  | otherwise = error "Unknown situation detected!"

extractTable :: LocalTime -> TableType -> String -> Table
extractTable yearMonth tType table =
  case (
    scrapeStringLike table rowScraper >>= \rows ->
    scrapeStringLike table cellScraper >>= \content ->
      let cleanContent = (strip . filter (\x -> x /= '\n' && x /= '\r' && x /= '\160')) <$> content
          nRows = length rows
          nCols = length content `div` nRows
      in (Just . Table yearMonth tType . chunksOf nCols) cleanContent
  ) of
  Just tc -> tc
  Nothing -> Table yearMonth tType []

rowScraper :: Scraper String [String]
rowScraper = texts (tagSelector "tbody" // tagSelector "tr")

cellScraper :: Scraper String [String]
cellScraper = texts (tagSelector "tbody" // tagSelector "tr" // tagSelector "td")
