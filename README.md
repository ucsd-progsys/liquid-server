README
======

A Generic web server for all the liquid-types based web demos.

There is nothing liquid specific of course, one can put in 
whatever binary one likes as long as there is a **binary** 
that has the following behavior:

  + **Input**   Source file `path/to/foo`
  + **Output**  JSON file   `path/to/foo.json` in the format specfied below.

JSON Format
-----------

The JSON file has data about:

    { status : "safe" + "unsafe" + "crash" + "error"
    , errors : [ { message : 
                 , start   : { line : number, col : number } 
                 , stop    : { line : number, col : number } 
                 }
               ]
    , types  : [ TODO ]
    }

The above are then reflected on the client side ace-editor based pane.


Installation 
------------

1. Build 

    cabal install liquid
    cabal install liquid-server

2. Run (make sure that the binary `liquid` is in the `$PATH`)

    liquid-server -p 8000

3. Use by pointing your web-browser to 

    http://localhost:8000/index.html

   or the relevant URL. (Don't forget the `index.html`)

Usage Notes
-----------

1. Command line parameters for the tool (e.g. include directories) 
   should be passed via pragmas (e.g. {-@ LIQUID ... @-}) in the 
   source file. 

2. Include directories don't work for remote use, only local.

3. When running in local mode, you should also be able to **save** a file.

Directory Structure
-------------------

Directory Structure
  
  + resources/
      + static/
          + index.html
          + fullpage.html
          + js/
          + css/
          + img/
          + demos/

  + custom/
       TOOLNAME1/
         config.js
         demos/
         sandbox/

       TOOLNAME2/
         config.js
         demos/
         sandbox/

       ...

Customization
-------------

To configure to a new checker called `toolName`

  1. Create a suitable `config` in `src/Language/Liquid/Server/Config.hs`

  2. Create a suitable: 
      resources/custom/toolName/config.js
      resources/custom/toolName/demos/
      resources/custom/toolName/sandbox/

     As an example, see `resources/custom/liquidhaskell/`

