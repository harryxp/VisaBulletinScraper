years = [2014,2015,2016,2017]
months = ["october","november","december","janurary","feburary","march","april","may","june","july","august","september"]

pairs = [ (y,m) | y <- years,m <- months]

main :: IO ()
main = undefined

processMonth :: (Int,String) -> [(String,String)]
processMonth (year,month) = undefined
