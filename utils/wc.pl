#!/usr/bin/env perl
use strict;
use utf8;
use open qw(:std :utf8);

my $words = 0;
my $hanzi = 0;
while (my $ctnt = <>) {
    $hanzi += $ctnt =~ s/\p{Han}/ i /g;
    $words += () = $ctnt =~ /\b\w[\w'.]*\b/g;
}
print "words: $words\nhanzi: $hanzi\n";

