#!/bin/bash
mkdir test_output
for i in {1..10}; do
    ./markov_generator test1.txt $i 10000 > test_output/test_output${i}.txt
done
