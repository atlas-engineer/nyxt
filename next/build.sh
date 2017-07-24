#!/bin/bash
# Use this script to generate an application for distribution

# Delete tmp if exists to force recompilation
rm -rf ./tmp

# Make the application bundle
eql5 make
qmake
make

# Clean up temporary directories
rm -rf ./tmp
rm libnext.a
rm Makefile
