/*******************************************************************************/
/************** Configuration Parameters For Server ****************************/
/*******************************************************************************/

'use strict';

/************** Titles *********************************************************/

var demoTitle     = "NanoJS";
var demoSubtitle  = "Liquid Types for JavaScript";

/************** Header Links 8**************************************************/

var allLinks = [ { url: "https://github.com/ucsd-pl/nano-js"          , name: "Code" }]; 

/************** Editor Modes ***************************************************/
var toolName       = "nanojs";
var editorTheme    = "ace/theme/xcode";
var editorMode     = "ace/mode/javascript";
var defaultErrText = "Type Error";
 
/************** List of Demos **************************************************/

var allCategories = [ { type : "basic"    , name: "Basics" }
                    , { type : "measure"  , name: "Datatype Measures" }
                    ];

var allDemos =
  { // Basic Demos
    "blank.js"      : { "name" : "Blank"    , "type" : "basic"  },
    "abs.js"        : { "name" : "Abs"      , "type" : "basic"  },
    "while5.js"     : { "name" : "Loop"     , "type" : "basic"  },
    "minindex01.js" : { "name" : "MinIndex" , "type" : "basic"  },

    "paste1.js"     : { "name" : "PASTE 1"  , "type" : "basic"  },
    "paste2.js"     : { "name" : "PASTE 2"  , "type" : "basic"  },
    "paste3.js"     : { "name" : "PASTE 3"  , "type" : "basic"  },

    // Measure Demos
    "safeLists.js"  : { "name" : "Safe List", "type" : "measure"},
  };

