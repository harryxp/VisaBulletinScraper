VisaBulletinScraper is a program that extracts Visa Bulletin information from [Department of State website](https://travel.state.gov/content/visas/en/law-and-policy/bulletin.html).

If you are waiting for your green card, use this to get statistics may help you make decisions.

    stack setup
    stack build
    stack exec VisaBulletinScraper-exe > VisaBulletin.csv

`app/Main.hs` contains a few options to tweak at the top.  Currently this program only extracts employment-based visa information but can be easily adapted to work with other types.

TODO:

- getNumRows could be smarter?
- use a calendar library instead of strings to represent years/months
- need another scraper for <= 2012 march
