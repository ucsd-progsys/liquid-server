README
======

A Generic web server for all the liquid-types based web demos.

There is nothing liquid specific of course, one can put in whatever 
binary one likes as long as there is a binary that has the following
behavior:

  + INPUT   Source file `path/to/foo`
  + OUTPUT  JSON file   `path/to/foo.json` in the format specfied below.

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

Configuration [TODO]
--------------------

To configure to a new checker, you should just specify:

  1. **checker binary** specified by a shell-command string
                        `checkerCommand`

  2. **demo files**     specified by the contents of 
                        `resources/static/demos/`

  3. **js config**      editor config params, list and types of demos 
                        `resources/static/js/config.js`

Files
-----

Directory Structure
  
  + resources/
      + static/
          + index.html
          + js/
          + css/
          + demos/

      + sandbox/

      + permalink/

TODO
----

1. PHP functionality
2. File Loading
3. File Saving
4. Permalinks
5. Local Checking [HEREHERE]
    1. SERVER: Send back timestamp [as in permalink]
    2. CLIENT: isRecheck = isUnknown && haveId
    [ Re | Check ]


    -   submit local path with QUERY { path : "/path/to/foo.hs" }
    -   server files are: "/path/to/_liquid/foo.hs{.json}"
    -   [SERVER] hash "path" (not timestamp) for file name (for repeated query)
    
    i.  build liquid && liquid-server
    ii. load file [pass extra params e.g. include dirs as LIQUID pragma]
    iii.check
    iv. save file

6. Language Customization
-------------------------

To support nano-js too.
