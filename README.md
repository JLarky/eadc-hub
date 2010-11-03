EADC
============
**EADC**-hub is a server for [ADC network] [1] is aimed to bring OTP features like hot code swapping and clusterization.

Cloning source:
------
    $ git clone git://github.com/JLarky/eadc-hub.git
    $ cd eadc-hub
    $ ./rebar compile # this command invokes `git submodule update` to get tiger-hash lib.

Compile on Windows:
-------------------
Install needed libraries/programms mensioned in [tiger-hash instruction][4] and do:

`PATH=$PATH:/c/mingw/bin` if MinGW's binaries not in PATH

`CC=gcc ./rebar compile` or you can set environment variable `CC=gcc`

Starting hub:
-------------
   `$ ./EADC # Linux`

   `> EADC.bat # Windows`

And check it on `adc://localhost:4111`

Also try run `!help` in chat.

First registered user (by `!regme pass`) will be superuser. For futher instruction see INSTALL.txt


Requirements:
-------------
* [erlang-otp] [3]
* gcc (MinGW)

   [1]: http://en.wikipedia.org/wiki/Advanced_Direct_Connect "Advanced Direct Connect"
   [2]: http://github.com/JLarky/erlang-tiger-hash "tiger hash"
   [3]: http://erlang.org/
   [4]: http://github.com/JLarky/erlang-tiger-hash/wiki/compiling-on-windows "Compiling tiger hash on windows"
