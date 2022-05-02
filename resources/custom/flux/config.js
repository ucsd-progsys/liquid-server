/*******************************************************************************/
/************** Configuration Parameters For Server ****************************/
/*******************************************************************************/

'use strict';

/************** Titles *********************************************************/

var demoTitle     = "Flux";
var demoSubtitle  = "Liquid Types for Rust";

/************** Header Links 8**************************************************/

var allLinks = [ { url: "https://github.com/liquid-rust/liquid-rust" , name: "Code" }
               ];

/************** Editor Modes ***************************************************/

var toolName         = "flux";
var editorTheme      = "ace/theme/xcode";
var editorMode       = "ace/mode/rust";
var defaultErrText   = "Yikes, something bad happened!";
var showErrorBanners = false;

/************** List of Demos **************************************************/

var allCategories = [ { type : "basic"    , name: "Basics" },
	              { type : "option"   , name: "Options"}, 
	              { type : "vector"   , name: "Vectors"}, 
                    ];

var allDemos =
  { // Basic Demos
    "blank.rs"    : { name : "Blank"   , type : "basic"  },
    "basics.rs"   : { name : "Basics"  , type : "basic"  },
    "pointers.rs" : { name : "Borrows" , type : "basic"  },

    // Options
    "poly-option.rs" : { name : "Standard Options" , type : "option"  },
    "ref-option.rs"  : { name : "Refined Options"  , type : "option"  },

    // Vectors 
    "vectors.rs" : { name : "Loops"               , type : "vector"},
    "kmeans.rs"  : { name : "K-Means Clustering"  , type : "vector"},
    "kmp.rs"     : { name : "KMP String Matching" , type : "vector"},
    "fft.rs"     : { name : "FFT" 	          , type : "vector"},
  };
