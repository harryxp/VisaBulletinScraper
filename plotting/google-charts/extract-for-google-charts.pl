#!/usr/bin/env perl

use strict;
use warnings;

my @patterns_and_columns = (
    [qr/TypeA\|2nd\|china/i, 'China EB2 A'],
    [qr/TypeB\|2nd\|china/i, 'China EB2 B'],
    [qr/TypeA\|3rd\|china/i, 'China EB3 A'],
    [qr/TypeB\|3rd\|china/i, 'China EB3 B'],
);
my @columns = sort(map { $_->[1] } @patterns_and_columns);

sub build_js_date($)
{
    my $month = shift(@_);
    if ($month =~ /(\d{2})\/(\d{2})\/(\d{2})/) {
        my ($m, $d, $y) = ($1-1, $2, $3);
        $m = sprintf("%02d", $m);
        $d = sprintf("%02d", $d);
        return "new Date(20$y, $m, $d)"; # TODO 20 is fragile
    } else {
        return "undefined";
    }
}

my $availability_by_column_by_month = {};
while (<>) {
    foreach my $pc (@patterns_and_columns) {
        my ($pattern, $column) = @$pc;
        if (m/$pattern/) {
            chomp;
            my ($month, $table_type, $category, $country, $availability) = split(/\|/);
            $month = build_js_date($month);
            $availability = build_js_date($availability);
            $availability_by_column_by_month->{$month}->{$column} = $availability;
        }
    }
}

my $data = '';
foreach my $month (sort(keys(%$availability_by_column_by_month))) {
    my $availability_by_column = $availability_by_column_by_month->{$month};
    $data .= "[$month, ";
    foreach my $column (@columns) {
        my $availability = $availability_by_column->{$column} // "undefined";
        $data .= "$availability, ";
    }
    $data .= "],\n";
}

my $define_availability_columns = join("", map { "data.addColumn('date', '$_');\n" } @columns);
my $google_charts_code = <<JavaScript;
    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
    <div id="chart_div"></div>
    <script type="text/javascript">
      google.charts.load('current', {'packages':['corechart']});
      google.charts.setOnLoadCallback(drawChart);

      function drawChart() {
        var data = new google.visualization.DataTable();
        data.addColumn('date', 'Month');
$define_availability_columns
        data.addRows([
$data
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
JavaScript

print($google_charts_code);
