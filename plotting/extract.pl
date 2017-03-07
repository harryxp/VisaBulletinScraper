#!/usr/bin/env perl

use strict;
use warnings;

my @patterns_and_filenames = (
    [qr/TypeA\|2nd\|china/i, 'china-2nd-A.dat'],
    [qr/TypeA\|3rd\|china/i, 'china-3rd-A.dat'],
    [qr/TypeB\|2nd\|china/i, 'china-2nd-B.dat'],
    [qr/TypeB\|3rd\|china/i, 'china-3rd-B.dat'],
);

my %handles_by_filename = ();

while (<>) {
    foreach my $pf (@patterns_and_filenames) {
        my ($pattern, $filename) = @$pf;
        if (m/$pattern/) {
            chomp;
            my ($month, $table_type, $category, $country, $availability) = split(/\|/);
            my $handle = $handles_by_filename{$filename};
            unless ($handle) {
                open($handle, '>', $filename);
                $handles_by_filename{$filename} = $handle;
            }
            print {$handle} ("$month\t$availability\n");
            next;
        }
    }
}

close() foreach values(%handles_by_filename);
