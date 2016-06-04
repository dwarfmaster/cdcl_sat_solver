#!/usr/bin/bash

execs=(./minisat_perso_no ./minisat_vsids_no ./minisat_vsids_luby ./minisat_vsids_geom ./minisat_pvsids_no ./minisat_pvsids_geom ./minisat_vsids_luby)

for e in ${execs[*]}; do
    for pb in $(ls problems); do
        t1=$(date +%s.%N)
        timeout 60s $e problems/$pb > /dev/null
        t2=$(date +%s.%N)
        dt=$(echo "$t2-$t1" | bc)
        echo "($e,$pb,$dt)"
    done
done

