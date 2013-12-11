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

    { "status" : "safe" + "unsafe" + "crash" + "error"
    , "errors" : [{ "message" : string 
                  , "start"   : { "line" : number, "col" : number } 
                  , "stop"    : { "line" : number, "col" : number } 
                  }
                 ]
    , "types"  : SEE CONCRETE EXAMPLE BELOW!
    }

In the examples below the `ann` field is a raw string that is rendered, as is,
i.e. with any present whitespace, in the tooltips.


Example 1 

    { "status":"error"
    , "types" : {}
    , "errors":[{"message":"Crash: Type ErrorsTC-ERROR Type identifier 'strin' unbound"
                ,"stop"   :{"column":31,"line":1}
                ,"start"  :{"column":25,"line":1}}
               ]
    }

Example 2

    { "status":"safe"
    , "types" :{"4":{"7": { "col":7
                          , "ident":"y_SSA_0"
                          , "row":4
                          , "ann": "{v : String | (v = lit#dog),\n              (v = lq_tmp_nano1),\n              (ttag([v]) = lit#string)}"}
                    ,"11":{"col":11
                          ,"ident":"lq_tmp_nano1"
                          ,"row":4
                          ,"ann":"{v : String | (v = lit#dog),(ttag([v]) = lit#string)}"}   
                    }
               ,"2":{"10":{"col":10
                          ,"ident":"foo"
                          ,"row":2
                          ,"ann":"(VV#0:Int) =\u003e String"}
                    ,"14":{"col":14
                          ,"ident":"x"
                          ,"row":2
                          ,"ann":"{v : Int | (ttag([v]) = lit#number)}"
                          }
                    }
               ,"6":{"10":{"col":10
                          ,"ident":"lq_tmp_nano2"
                          ,"row":6
                          ,"ann":"{v : String | (v = lit#cat),(ttag([v]) = lit#string)}" 
                          }
                    }
               } 
    ,"errors":[]
    }

**Note** you can populate the `types` field regardless of the value of the
`status` field. The types, if provided, are reflected on the client side 
ace-editor based pane with tooltips.


Installation 
------------

1. Build 

    cabal install

2. Run (make sure that the `toolName` binary, described below, is in the `$PATH`)

    liquid-server -p 8000

3. Use by pointing your web-browser to 

    http://localhost:8000/index.html

   or the relevant URL. (Don't forget the `index.html`)

Of course you can pick a port other than 8000 if you like.

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
         config.json
         config.js
         demos/
         sandbox/

       TOOLNAME2/
         config.json
         config.js
         demos/
         sandbox/

       ...

Customization
-------------

To configure to a new checker called `toolName` create a directory

    resources/custom/toolName/

and populate it with:

    config.json
    config.js
    demos/
    sandbox/

As an example, see: 

  + `resources/custom/liquidhaskell/`
  + `resources/custom/nanojs/`


Usage Notes
-----------

1. Command line parameters for the tool (e.g. include directories) 
   should be passed via pragmas (e.g. {-@ LIQUID ... @-}) in the 
   source file. 

2. Include directories don't work for remote use, only local.

3. When running in local mode, you should also be able to **save** a file.


