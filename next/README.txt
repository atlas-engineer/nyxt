HOWTO
=====


Prepare:

  - in "make.lisp", add all of your lisp files to *lisp-files* (see "lisp/")

  - adapt main.cpp (translations, initial Lisp form to evaluate, package name).


Build:
  
  remove "tmp/" (if present)

  eql5 make
  qmake
  make      (MSVC: nmake)


  Now you should find a "my_app" executable.


Notes:

  [Windows]
  
    You need to make sure to never use printing to *standard-output*,
    as this would crash your GUI application (without a console).
    If you experience unexpected crashes of your EXE, add:

      CONFIG += console
  
    to your *.pro file, in order to see eventual console output.
