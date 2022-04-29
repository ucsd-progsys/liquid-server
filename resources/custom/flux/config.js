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

var allCategories = [ { type : "basic"    , name: "Basics" }
                    ];

var allDemos =
  { // Basic Demos
    "option.rs"             : { name : "Option"           , type : "basic"  },
  };
