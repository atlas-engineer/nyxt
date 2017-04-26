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

Installing EQL5 Library (Embedded QT Lisp)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The source for EQL5 is available here:

https://gitlab.com/eql/EQL5

To build and install the EQL Library:

1. Clone the Repository into a dir where you plan to keep the install
2. In ``src/`` exec: ``ecl -shell make-eql-lib.lisp`` This command will generate ``src/libini.dylib``
3. In ``src/`` exec: ``qmake eql_lib.pro``. This command will generate the makefile
4. In ``src/`` exec: ``make``. Compiled assets will appear in parent directory
5. Create symlinks between the dylib files in the parent directory and ``/usr/local/lib``
6. Copy the header files from the ``src/`` directory to ``/usr/local/include/eql``

Installing EQL5 Executable (Embedded QT Lisp)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. In src/ exec: ``qmake eql_exe.pro``. This command will generate the makefile
2. Exec ``make``. Compiled asset will appear in the parent directory in eql5.app/Contents/MacOS/eql5
3. Create a symlink from ``eql5.app/Contents/MacOS/eql5`` to ``/opt/local/bin`` or any other dir in your path
4. Test your installation of EQL by running ``eql5 -qgui`` you should be presented with a GUI/REPL

   - If you cannot see the window, try looking for it or pressing on the dock

Installing EQL5 Webkit Module (Embedded QT Lisp)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To build and install the EQL Webkit Module:

1. In ``src/`` exec: ``qmake module_webkit.pro``. This command will generate the makefile
2. Exec ``make``. Compiled assets will appear in the parent directory
3. Create symlinks between the webkit dylib files in the parent directory and ``/usr/local/lib``
4. To verify your install of the EQL webkit module run ``eql5 -qgui``
   
   - Within the GUI's QT "Tab" the QWebView should render documentation
   - If the Webkit module is not built/missing, the documentation will not render

Compiling nEXT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From the directory next/ execute the following commands to compile:

1. ``eql5 make``
2. ``qmake``
3. ``make``

Now you should have a compiled next.app, simply execute this app to
start nEXT browser.
