#!/usr/bin/perl

use strict;
use warnings;

# Population size
my $sp = 8;
# Genome size
my $gs = 4;

sub randbool {
    my $r = rand();
    my $third = 1/3;
    if($r < $third) {
        return 0;
    } elsif($r < 2*$third) {
        return 1;
    } else {
        return 2;
    }
}

for (my $i = 0; $i < $sp; $i++) {
    for (my $j = 0; $j < $gs; $j++) {
        print randbool();
        print " ";
    }
    print "\n";
}

