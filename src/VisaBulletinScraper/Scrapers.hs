module VisaBulletinScraper.Scrapers (extractTables,pageScraper) where

import Data.List (isInfixOf)
import Data.List.Split (chunksOf)
import Data.String.Utils (strip)
import Data.Time.LocalTime (LocalTime)
import Text.HTML.Scalpel
  (Scraper,TagName(TagString)
  ,chroot,htmls,match,scrapeStringLike,tagSelector,texts
  ,(@:),(//))

import VisaBulletinScraper.Types (Table(..),TableType(..))

pageScraper :: Scraper String [String]
pageScraper = chroot (TagString "div" @: [match matcher]) tableScraper
  where matcher "class" "tsg-rwd-content-page-parsysxxx parsys" = True
        matcher _ _ = False

tableScraper :: Scraper String [String]
tableScraper = htmls selector >>= return . filter (isInfixOf "Employment")
  where selector = (TagString "div" @: [match matcher]) // tagSelector "table"
        matcher "class" "tsg-rwd-text parbase section" = True
        matcher _ _ = False

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
