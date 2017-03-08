VisaBulletinScraper is a program that extracts Visa Bulletin information from [the Department of State website](https://travel.state.gov/content/visas/en/law-and-policy/bulletin.html).

If you are waiting for your green card, using this to get statistics may help you make sound decisions.

    stack setup
    stack build
    stack exec VisaBulletinScraper-exe

`app/Main.hs` contains a few options to tweak at the top.  Currently this program only extracts employment-based visa information but can be easily adapted to work with other types.

The program will spit out `VisaBulletin.csv` and `VisaBulletinUnpivoted.csv`.  The latter is good for importing into databases and plotting.  I implemented two ways of plotting.

1. Use [Google Charts](https://developers.google.com/chart/):

    cd plotting/google-charts/
    ./extract-for-google-charts.pl ../../VisaBulletinUnpivoted.csv > google-charts.html

    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
    <div id="chart_div"></div>
    <script type="text/javascript">
      google.charts.load('current', {'packages':['corechart']});
      google.charts.setOnLoadCallback(drawChart);

      function drawChart() {
        var data = new google.visualization.DataTable();
        data.addColumn('date', 'Month');
data.addColumn('date', 'China EB2 A');
data.addColumn('date', 'China EB2 B');
data.addColumn('date', 'China EB3 A');
data.addColumn('date', 'China EB3 B');

        data.addRows([
[new Date(2012, 03, 01), new Date(2010, 04, 01), undefined, new Date(2005, 02, 01), undefined, ],
[new Date(2012, 04, 01), new Date(2007, 07, 15), undefined, new Date(2005, 03, 01), undefined, ],
[new Date(2012, 05, 01), undefined, undefined, new Date(2005, 07, 08), undefined, ],
[new Date(2012, 06, 01), undefined, undefined, new Date(2005, 08, 22), undefined, ],
[new Date(2012, 07, 01), undefined, undefined, new Date(2005, 10, 08), undefined, ],
[new Date(2012, 08, 01), undefined, undefined, new Date(2005, 11, 15), undefined, ],
[new Date(2012, 10, 01), new Date(2007, 08, 01), undefined, new Date(2006, 03, 15), undefined, ],
[new Date(2012, 11, 01), new Date(2007, 09, 22), undefined, new Date(2006, 06, 01), undefined, ],
[new Date(2013, 00, 01), new Date(2007, 11, 08), undefined, new Date(2006, 08, 22), undefined, ],
[new Date(2013, 01, 01), new Date(2008, 00, 15), undefined, new Date(2006, 10, 15), undefined, ],
[new Date(2013, 02, 01), new Date(2008, 01, 15), undefined, new Date(2007, 00, 22), undefined, ],
[new Date(2013, 03, 01), new Date(2008, 03, 01), undefined, new Date(2007, 03, 22), undefined, ],
[new Date(2013, 04, 01), new Date(2008, 04, 15), undefined, new Date(2007, 11, 01), undefined, ],
[new Date(2013, 05, 01), new Date(2008, 06, 15), undefined, new Date(2008, 08, 01), undefined, ],
[new Date(2013, 06, 01), new Date(2008, 07, 08), undefined, new Date(2009, 00, 01), undefined, ],
[new Date(2013, 07, 01), new Date(2008, 07, 08), undefined, new Date(2009, 00, 01), undefined, ],
[new Date(2013, 08, 01), new Date(2008, 07, 08), undefined, new Date(2010, 06, 01), undefined, ],
[new Date(2013, 09, 01), new Date(2008, 08, 15), undefined, new Date(2010, 06, 01), undefined, ],
[new Date(2013, 10, 01), new Date(2008, 09, 08), undefined, new Date(2010, 09, 01), undefined, ],
[new Date(2013, 11, 01), new Date(2008, 10, 08), undefined, new Date(2011, 09, 01), undefined, ],
[new Date(2014, 00, 01), new Date(2008, 11, 08), undefined, new Date(2012, 03, 01), undefined, ],
[new Date(2014, 01, 01), new Date(2009, 00, 08), undefined, new Date(2012, 05, 01), undefined, ],
[new Date(2014, 02, 01), new Date(2009, 01, 15), undefined, new Date(2012, 08, 01), undefined, ],
[new Date(2014, 03, 01), new Date(2009, 02, 08), undefined, new Date(2012, 09, 01), undefined, ],
[new Date(2014, 04, 01), new Date(2009, 03, 15), undefined, new Date(2012, 09, 01), undefined, ],
[new Date(2014, 05, 01), new Date(2009, 04, 22), undefined, new Date(2006, 09, 01), undefined, ],
[new Date(2014, 06, 01), new Date(2009, 06, 01), undefined, new Date(2006, 09, 01), undefined, ],
[new Date(2014, 07, 01), new Date(2009, 09, 08), undefined, new Date(2008, 10, 01), undefined, ],
[new Date(2014, 08, 01), new Date(2009, 09, 08), undefined, new Date(2008, 10, 01), undefined, ],
[new Date(2014, 09, 01), new Date(2009, 10, 15), undefined, new Date(2009, 03, 01), undefined, ],
[new Date(2014, 10, 01), new Date(2009, 11, 08), undefined, new Date(2010, 00, 01), undefined, ],
[new Date(2014, 11, 01), new Date(2010, 00, 01), undefined, new Date(2010, 05, 01), undefined, ],
[new Date(2015, 00, 01), new Date(2010, 01, 01), undefined, new Date(2011, 02, 01), undefined, ],
[new Date(2015, 01, 01), new Date(2010, 02, 15), undefined, new Date(2011, 08, 01), undefined, ],
[new Date(2015, 02, 01), new Date(2010, 08, 01), undefined, new Date(2011, 09, 22), undefined, ],
[new Date(2015, 03, 01), new Date(2011, 03, 01), undefined, new Date(2011, 00, 01), undefined, ],
[new Date(2015, 04, 01), new Date(2012, 05, 01), undefined, new Date(2011, 04, 01), undefined, ],
[new Date(2015, 05, 01), new Date(2013, 05, 01), undefined, new Date(2011, 08, 01), undefined, ],
[new Date(2015, 06, 01), new Date(2013, 09, 01), undefined, new Date(2011, 08, 01), undefined, ],
[new Date(2015, 07, 01), new Date(2013, 11, 15), undefined, new Date(2004, 05, 01), undefined, ],
[new Date(2015, 08, 01), new Date(2006, 00, 01), undefined, new Date(2004, 11, 22), undefined, ],
[new Date(2015, 09, 01), new Date(2013, 00, 01), new Date(2012, 00, 01), new Date(2013, 09, 01), new Date(2011, 09, 15), ],
[new Date(2015, 10, 01), new Date(2013, 00, 01), new Date(2012, 01, 01), new Date(2013, 09, 01), new Date(2012, 00, 01), ],
[new Date(2015, 11, 01), new Date(2013, 00, 01), new Date(2012, 01, 01), new Date(2013, 09, 01), new Date(2012, 03, 15), ],
[new Date(2016, 00, 01), new Date(2013, 00, 01), new Date(2012, 01, 01), new Date(2013, 09, 01), new Date(2012, 06, 01), ],
[new Date(2016, 01, 01), new Date(2013, 00, 01), new Date(2012, 02, 01), new Date(2013, 09, 01), new Date(2012, 09, 01), ],
[new Date(2016, 02, 01), new Date(2013, 05, 01), new Date(2012, 07, 01), new Date(2015, 04, 01), new Date(2013, 05, 01), ],
[new Date(2016, 03, 01), new Date(2013, 05, 01), new Date(2012, 08, 01), new Date(2015, 04, 01), new Date(2013, 07, 15), ],
[new Date(2016, 04, 01), new Date(2012, 08, 01), new Date(2013, 05, 01), new Date(2013, 07, 15), new Date(2015, 04, 01), ],
[new Date(2016, 05, 01), new Date(2010, 00, 01), new Date(2013, 05, 01), new Date(2010, 00, 01), new Date(2015, 04, 01), ],
[new Date(2016, 06, 01), new Date(2010, 00, 01), new Date(2013, 05, 01), new Date(2010, 00, 01), new Date(2015, 04, 01), ],
[new Date(2016, 07, 01), new Date(2010, 00, 01), new Date(2013, 05, 01), new Date(2010, 00, 01), new Date(2015, 04, 01), ],
[new Date(2016, 08, 01), new Date(2010, 00, 01), new Date(2013, 05, 01), new Date(2010, 00, 01), new Date(2015, 04, 01), ],
[new Date(2016, 09, 01), new Date(2012, 01, 15), new Date(2013, 02, 01), new Date(2013, 00, 22), new Date(2014, 04, 01), ],
[new Date(2016, 10, 01), new Date(2012, 06, 15), new Date(2013, 02, 01), new Date(2013, 03, 15), new Date(2014, 04, 01), ],
[new Date(2016, 11, 01), new Date(2012, 08, 22), new Date(2013, 02, 01), new Date(2013, 06, 01), new Date(2014, 04, 01), ],
[new Date(2017, 00, 01), new Date(2012, 09, 15), new Date(2013, 02, 01), new Date(2013, 08, 08), new Date(2014, 04, 01), ],
[new Date(2017, 01, 01), new Date(2012, 10, 15), new Date(2013, 02, 01), new Date(2013, 09, 01), new Date(2014, 04, 01), ],
[new Date(2017, 02, 01), new Date(2012, 11, 15), new Date(2013, 02, 01), new Date(2014, 02, 15), new Date(2014, 04, 01), ],

        ]);

        var options = {
          title: 'Green Card Visa Availability - China',
          width: 900,
          height: 500,
          hAxis: {
            format: 'MMM dd, yyyy',
          },
        };

        var chart = new google.visualization.LineChart(document.getElementById('chart_div'));
        chart.draw(data, options);
      };
    </script>

2. Use `gnuplot` and `perl`:

    cd plotting/gnuplot/
    ./extract-for-gnuplot.pl ../../VisaBulletinUnpivoted.csv
    gnuplot plot-script

![Green Card Visa Availability - China](plotting/VisaAvailabilityChina.png)

TODO:
- Plotting using Haskell?
- Oct 2012 is skipped - it has an irregular URL.
- `getNumRows` could be smarter?
- Need another scraper for <= 2012 march.
