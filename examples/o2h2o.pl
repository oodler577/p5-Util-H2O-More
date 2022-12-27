#!/usr/bin/env perl

use strict;
use warnings;
use FindBin qw/$Bin/;
use Util::H2O::More qw/o2h2o ini2o o2ini/;
use Config::Tiny qw//;

my $c = o2h2o(Config::Tiny->read(qq{$Bin/test.ini}));

require Data::Dumper;
print Data::Dumper::Dumper($c);

my $c2 = ini2o(qq{$Bin/test.ini});
$c2->section1->var1(q{oof});

print Data::Dumper::Dumper($c2);
o2ini $c2, qq{$Bin/test2.ini};
