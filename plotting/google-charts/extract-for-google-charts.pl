#!/usr/bin/env perl

use strict;
use warnings;

my @countries_patterns_columns = (
    ['China',       qr/TypeA\|2nd\|CHINA/i,       'EB2 A'],
    ['China',       qr/TypeB\|2nd\|CHINA/i,       'EB2 B'],
    ['China',       qr/TypeA\|3rd\|CHINA/i,       'EB3 A'],
    ['China',       qr/TypeB\|3rd\|CHINA/i,       'EB3 B'],
    ['India',       qr/TypeA\|2nd\|INDIA/i,       'EB2 A'],
    ['India',       qr/TypeB\|2nd\|INDIA/i,       'EB2 B'],
    ['India',       qr/TypeA\|3rd\|INDIA/i,       'EB3 A'],
    ['India',       qr/TypeB\|3rd\|INDIA/i,       'EB3 B'],
    ['Mexico',      qr/TypeA\|2nd\|Mexico/i,      'EB2 A'],
    ['Mexico',      qr/TypeB\|2nd\|Mexico/i,      'EB2 B'],
    ['Mexico',      qr/TypeA\|3rd\|Mexico/i,      'EB3 A'],
    ['Mexico',      qr/TypeB\|3rd\|Mexico/i,      'EB3 B'],
    ['Philippines', qr/TypeA\|2nd\|Philippines/i, 'EB2 A'],
    ['Philippines', qr/TypeB\|2nd\|Philippines/i, 'EB2 B'],
    ['Philippines', qr/TypeA\|3rd\|Philippines/i, 'EB3 A'],
    ['Philippines', qr/TypeB\|3rd\|Philippines/i, 'EB3 B'],
);

my %columns = map { $_->[2] => 1 } @countries_patterns_columns;
my @sorted_columns = sort(keys(%columns));

sub main
{
    my $availability_by_column_by_month_by_country = parse_file();

    my @sorted_countries = sort(keys(%$availability_by_column_by_month_by_country));
    my $divs = build_js_divs(\@sorted_countries);
    my $callbacks = build_js_callbacks_code(\@sorted_countries);
    my $define_draw_functions = build_js_draw_functions_code($availability_by_column_by_month_by_country);
    my $google_charts_code = <<JavaScript;
        <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
$divs
        <script type="text/javascript">
          google.charts.load('current', {'packages':['corechart']});
$callbacks
$define_draw_functions
        </script>
JavaScript

    print($google_charts_code);
}

sub parse_file
{
    my $availability_by_column_by_month_by_country = {};
    while (<>) {
        foreach my $cpc (@countries_patterns_columns) {
            my ($country, $pattern, $column) = @$cpc;
            if (m/$pattern/) {
                chomp;
                my ($month, $table_type, $category, $country_str, $availability) = split(/\|/);
                $month = build_js_date($month);
                $availability = build_js_date($availability);
                $availability_by_column_by_month_by_country->{$country}->{$month}->{$column} = $availability;
            }
        }
    }
    return $availability_by_column_by_month_by_country;
}

sub build_js_date
{
    my $month = shift(@_);
    if ($month =~ /(\d{2})\/(\d{2})\/(\d{2})/) {
        my ($m, $d, $y) = ($1-1, $2, $3);
        $m = sprintf("%02d", $m);           # TODO this is to make sorting work but there's gotta be a better way
        $d = sprintf("%02d", $d);
        return "new Date(20$y, $m, $d)";    # TODO 20 is fragile
    } else {
        return "undefined";
    }
}

sub build_js_divs
{
    my $countries = shift(@_);
    my @js_divs_code = ();
    foreach my $country (@$countries) {
        push(@js_divs_code, "<div id=\"${country}_chart_div\"></div>");
    }
    return join("\n", @js_divs_code);
}

sub build_js_callbacks_code
{
    my $countries = shift(@_);
    my @js_callbacks_code = ();
    foreach my $country (@$countries) {
        push(@js_callbacks_code, "google.charts.setOnLoadCallback(draw${country}Chart);");
    }
    return join("\n", @js_callbacks_code);
}

sub build_js_draw_functions_code
{
    my $availability_by_column_by_month_by_country = shift(@_);
    my @js_draw_functions_code = ();
    foreach my $country (keys(%$availability_by_column_by_month_by_country)) {
        push(@js_draw_functions_code, build_js_draw_function_code(
            $country, $availability_by_column_by_month_by_country->{$country}));
    }
    return join("\n", @js_draw_functions_code);
}

sub build_js_draw_function_code
{
    my $country = shift(@_);
    my $availability_by_column_by_month = shift(@_);

    my $define_availability_columns = join("", map { "data.addColumn('date', '$_');\n" } @sorted_columns);
    my $rows = '';
    foreach my $month (sort(keys(%$availability_by_column_by_month))) {
        my $availability_by_column = $availability_by_column_by_month->{$month};
        $rows .= "[$month, ";
        foreach my $column (@sorted_columns) {
            my $availability = $availability_by_column->{$column} // "undefined";
            $rows .= "$availability, ";
        }
        $rows .= "],\n";
    }
    my $js_draw_function_code = <<JavaScript;
function draw${country}Chart() {
  var data = new google.visualization.DataTable();
  data.addColumn('date', 'Month');
$define_availability_columns
  data.addRows([
$rows
  ]);

  var options = {
    title: 'Green Card Visa Availability - $country',
    width: 900,
    height: 500,
    hAxis: {
      format: 'MMM dd, yyyy',
    },
  };

  var chart = new google.visualization.LineChart(document.getElementById('${country}_chart_div'));
  chart.draw(data, options);
};
JavaScript
}

main();
