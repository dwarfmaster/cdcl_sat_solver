#!/bin/bash
# Convert format

echo "trial,gen_size,pop_size,time" > data_file
sed 's#(./minisat_\([a-zA-Z_]*\),pb_\([0-9]*\)_\([0-9]*\)_[0-9]*,\([^,]*\))#\1,\2,\3,\4#g' ../results >> data_file

