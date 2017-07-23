nEXT Developer Guide
========================================================================
Placeholder.

Building nEXT Browser
========================================================================
Before building nEXT browser, make sure that you have installed all of
the dependencies. The build instructions for the dependencies can be
found in the parent directory.

Prepare
------------------------------------------------------------------------
  - in "make.lisp", add all of your lisp files to *lisp-files* (see "lisp/")
  - adapt main.cpp (translations, initial Lisp form to evaluate, package name).

Build
------------------------------------------------------------------------  
  0. remove "tmp/" (if present)
  1. eql5 make
  2. qmake
  3. make

  Now you should find a "nEXT" executable.

Notes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[Windows]

  You need to make sure to never use printing to *standard-output*,
  as this would crash your GUI application (without a console).
  If you experience unexpected crashes of your EXE, add:

    CONFIG += console

  to your \*.pro file, in order to see eventual console output.
