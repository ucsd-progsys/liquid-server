/*******************************************************************************/
/************** Configuration Parameters For Server ****************************/
/*******************************************************************************/

'use strict';

/************** Titles *********************************************************/

var demoTitle     = "Elsa";
var demoSubtitle  = "An Interactive Lambda Calculus Evaluator";

/************** Header Links 8**************************************************/

var allLinks = [ { url: "https://github.com/ucsd-progsys/elsa" , name: "Code" } 
               ];

/************** Editor Modes ***************************************************/

var toolName         = "elsa";
var editorTheme      = "ace/theme/xcode";
var editorMode       = "ace/mode/haskell";
var defaultErrText   = "Invalid Evaluation";
var showErrorBanners = false;

/************** List of Demos **************************************************/

var allCategories = [ { type : "basic"    , name: "Basics" }
                    ];

var allDemos =
  { // Basic Demos
    "blank.lc"              : { name : "Blank"            , type : "basic"  },
    "alpha.lc"              : { name : "Alpha Conversion" , type : "basic"  },
    "beta.lc"               : { name : "Beta Reduction"   , type : "basic"  },
    "bool.lc"               : { name : "Church Booleans"  , type : "basic"  },
    "ite.lc"                : { name : "Church Branches"  , type : "basic"  },
    "nats.lc"               : { name : "Church Numerals"  , type : "basic"  },
    "arith.lc"              : { name : "Add/Mul Numerals" , type : "basic"  },
    "compose.lc"            : { name : "Composition"      , type : "basic"  },
  };
