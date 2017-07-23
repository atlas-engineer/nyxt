#!/bin/bash
rm -rf ./tmp
eql5 make
qmake
make
rm -rf ./tmp
