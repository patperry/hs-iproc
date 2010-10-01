#!/bin/bash

for i in {100..199}
do
    echo "./Boot $i > boot$i.R"
    ./Boot $i > boot$i.R
done
