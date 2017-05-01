Building nEXT Browser
========================================================================
NeXT browser is built with ECL and EQL, and is designed to be cross
platform compatible. To download a prebuilt-binary, please see the
"Releases" section of this repository.

OSX Compilation Instructions
------------------------------------------------------------------------
Installing Xcode Command Line Tools
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before installing anything, install the Xcode Comand line tools. To
install the Xcode Command Line tools on OSX execute the following in a
terminal:

``xcode-select --install``

To verify that the command tools were successfully installed type:

``xcode-select -p``

the output of this command should be the path at which the command
line tools are installed.

Installing QT5 (Cross Platform GUI Toolkit)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
QT5 is available from a number of sources, the easiest is to install it
via a package manager such as Macports or Brew. However you install,
ensure that you install it with webkit support.

You can install QT5 via Macports with the following command:

``port install qt5``

You can also install QT5 via the official qt installer:

https://www.qt.io/download/

To test your installation/version of QT, execute ``qmake --version``.

Installing ECL (Embeddable Common Lisp)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Compilable tarballs can be found on the ECL website:

https://common-lisp.net/project/ecl/static/files/release/

To verify your installation of ECL:

- Execute ``/usr/local/bin/ecl``, it should show an ECL Prompt
- Verify the contents of ``/usr/local/lib`` contain the ecl libraries.
- Verify the contents of ``/usr/local/include/ecl`` contain the header files

Installing EQL5 Library & Executable (Embedded QT Lisp)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The source for EQL5 is available here:

https://gitlab.com/eql/EQL5

To build and install the EQL Library/Executable:

1. Clone the Repository into a directory where you plan to keep the install
2. In ``src/`` exec: ``ecl -shell make.lisp`` This command will generate ``src/libini.a``
3. Edit ``src/eql5.pro`` commenting out all QT modules you do not require
4. Edit ``src/eql_lib.pro`` adding the directory of your ecl dylib files

   - Change: ``LIBS += -lecl -L/usr/local/lib -lini_eql5 -L.``

5. Edit ``src/eql_exe.pro`` to build an executable instead of an ".app" for OSX

   - Add: ``CONFIG -= app_bundle``

6. In ``src/`` exec: ``qmake eql5.pro``. This command will generate the makefile
7. In ``src/`` exec: ``make``
8. In ``src/`` exec: ``sudo make install``

To test your installation exec ``eql5 -qgui``, you should presented
with a REPL and a GUI.

Compiling nEXT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From the directory next/ execute the following commands to compile:

1. ``eql5 make``
2. ``qmake``
3. ``make``

Now you should have a compiled next.app, simply execute this app to
start nEXT browser.
