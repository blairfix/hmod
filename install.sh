#!/bin/bash

# compile attribute
Rscript comp_att.R

# clean old binaries
rm ./src/*.o

# install
cd ..
R CMD INSTALL --preclean --no-multiarch --with-keep.source hmod
