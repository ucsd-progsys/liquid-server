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

Configuration
-------------

To configure to a new checker, you should just specify:

  + checker, specified via a path to the **binary** [TODO]
  + (optional) editor configuration parameters      [TODO]

TODO
----

1. PHP functionality
--------------------

  + routes:
      1. get  file
      2. get  hquals
      3. post json-query :: { program :: string }
      4. echo response   :: { annots  :: { status :: string } } 

     
      static/demos/foo.hs
      static/demos/foo.hs.hquals
      static/js/liquid.js etc.
      static/css/
      static/img/
        

      static/demos/ /:srcfile
      check/:query


    // var baseURL    = 'demos/';
    var srcURL        = baseURL + demo.file;
    var qualsURL      = 'demos/' + demo.file + '.hquals';
   

    $http.get(srcURL)
    $http.get(qualsURL)

2. Permalinks
-------------

3. Local Checking
-----------------

4. File Loading
---------------

Saving happens locally.

5. Language Customization
-------------------------

To support nano-js too.
