#!/usr/bin/perl

use strict;
use warnings;

# Population size
my $sp = $ARGV[0];
# Genome size
my $gs = $ARGV[1];

sub randbool {
    my $r = rand();
    if($r < 0.25) {
        return 0;
    } elsif($r < 0.5) {
        return 1;
    } else {
        return 2;
    }
}

print "s $gs\n";
for (my $i = 0; $i < $sp; $i++) {
    for (my $j = 0; $j < $gs; $j++) {
        print randbool();
        print " ";
    }
    print "\n";
}

