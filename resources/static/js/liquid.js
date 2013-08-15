'use strict';

// Globals
var errorMarkers = [];

/*******************************************************************************r
/************** Extract Demo from URL ******************************************/
/*******************************************************************************/

var allDemos =
  { // Basic Demos
    "blank.hs"              : { "name" : "Blank"            , "type" : "basic"  },
    "refinements101.hs"     : { "name" : "Refinements 101"  , "type" : "basic"  },
    "refinements101reax.hs" : { "name" : "Refinements 102"  , "type" : "basic"  },
    "vectorbounds.hs"       : { "name" : "Vector Bounds"    , "type" : "basic"  },
    // Measure Demos
    "lenMapReduce.hs"       : { "name" : "Safe List"        , "type" : "measure"},
    "KMeansHelper.hs"       : { "name" : "K-Means Lib"      , "type" : "measure"},
    "KMeans.hs"             : { "name" : "K-Means"          , "type" : "measure"}, 
    "TalkingAboutSets.hs"   : { "name" : "Talk About Sets"  , "type" : "measure"},
    "UniqueZipper.hs"       : { "name" : "Unique Zippers"   , "type" : "measure"},
    "LambdaEval.hs"         : { "name" : "Lambda Eval"      , "type" : "measure"}, 
    "treesum.hs"            : { "name" : "List-Tree Sum"    , "type" : "measure"},
    // Abstract Refinement Demos
    "absref101.hs"          : { "name" : "Parametric Invariants", "type" : "absref" },
    "Order.hs"              : { "name" : "Ordered Lists"        , "type" : "absref" },
    "Map.hs"                : { "name" : "BinSearch Tree"       , "type" : "absref" },
    "Foldr.hs"              : { "name" : "Induction"            , "type" : "absref" },
    "IMaps.hs"              : { "name" : "Indexed Maps"         , "type" : "absref" },
    // HOPA Tutorial Demos
    "SimpleRefinements.hs" : { "name" : "Simple Refinements", "type" : "tutorial" },  
    "Loop.hs"              : { "name" : "HO Loop"           , "type" : "tutorial" },
    "Composition.hs"       : { "name" : "Composition"       , "type" : "tutorial" },
    "Array.hs"             : { "name" : "Finite Maps"       , "type" : "tutorial" }

    // "ListSort.hs"       : { "name" : "Sorting Lists"        , "type" : "absref" },
    // "ListLength.hs"     : { "name" : "List Lengths"      , "type" : "measure"},
    // "MapReduce.hs"      : { "name" : "Map Reduce"        , "type" : "measure"}, 
    // "ListElts.hs"           : { "name" : "List Elements" , "type" : "measure"}, 


  };

function getDemo(name){
  var d = allDemos[name];
  var res = { "name" : d.name , "file" : name };
  return res;
}

function getDemos(ty){ 
  var a = [];
  for (var k in allDemos) { 
    if (allDemos[k].type == ty) 
      a.push(getDemo(k));
  };
  return a;
}

/*******************************************************************************/
/************** Setting Up Editor **********************************************/
/*******************************************************************************/

var progEditor  = ace.edit("program");
progEditor.setTheme("ace/theme/xcode");
var SrcMode     = require("ace/mode/haskell").Mode;
progEditor.getSession().setMode(new SrcMode());
var typeTooltip = new TokenTooltip(progEditor, getAnnot);

function resizeProgEditor() {
  var w = $('#program-pane').width();
  return $('#program').width(w);
};

//listen for changes
$(window).resize(resizeProgEditor);

//set initially
resizeProgEditor();

/*******************************************************************************/
/** Markers For Errors *********************************************************/
/*******************************************************************************/

function errorRange(err){
  
  var row0 = err.start.line - 1;
  var col0 = err.start.column - 1;
  var row1 = err.stop.line - 1;
  var col1 = err.stop.column - 1;
 
  if (row0 == row1 && col0 == col1){
    return new Range(row0, col0, row0, col0 + 1);
  } else {
    return new Range(row0, col0, row1, col1);
  }
}

function errorMarker(editor, err){
  var r = errorRange(err);
  return editor.session.addMarker(r, "ace_step", "error");
}

function errorAceAnnot(err){
  var etext = "Liquid Type Error";
  if (err.message) { etext = err.message; }
  var ann = { row   : err.start.line - 1
            , column: err.start.column
            , text  : etext
            , type  : "error"
            };
  return ann;
}

function setErrors(editor, errs){
  // Add Error Markers
  errorMarkers.forEach(function(m){ editor.session.removeMarker(m); });
  errorMarkers = errs.map(function(e){ return errorMarker(editor, e);});
  
  // Add Gutter Annotations
  editor.session.clearAnnotations();
  var annotations  = errs.map(errorAceAnnot);
  editor.session.setAnnotations(annotations);
}


/*******************************************************************************/
/************** URLS ***********************************************************/
/*******************************************************************************/

function getSrcURL(file)   { return ('demos/' + file);              }
function getVerifierURL()  { return 'check/' /* 'liquid.php' */; }

/*******************************************************************************/
/************** Tracking Status and Source *************************************/
/*******************************************************************************/

function clearStatus($scope){
  $scope.isSafe       = false;
  $scope.isUnsafe     = false;
  $scope.isError      = false;
  $scope.isCrash      = false;
  $scope.isChecking   = false;
  $scope.isUnknown    = true ;
}

function setStatusChecking($scope){
  clearStatus($scope);
  $scope.isChecking = true;
  $scope.isUnknown  = false;
}

function setStatusResult($scope, result){
  debugResult = result;

  clearStatus($scope);
  $scope.isChecking   = false;
  $scope.isSafe       = (result == "safe"  );
  $scope.isUnsafe     = (result == "unsafe");
  $scope.isCrash      = (result == "crash" );
  $scope.isError      = (result == "error" );
  $scope.isUnknown    = !($scope.isSafe || $scope.isError || $scope.isUnsafe || $scope.isCrash);
}

function setSourceCode($scope, srcName, srcText){
  clearStatus($scope);
  $scope.sourceFileName = srcName; 
  progEditor.getSession().setValue(srcText);
}

/*******************************************************************************/
/************** Loading Files **************************************************/
/*******************************************************************************/

/*@ fileText :: (file, (string) => void) => void */
function fileText(file, k){
  var reader = new FileReader();
  reader.addEventListener("load", function(e){
    k(e.target.result);
  });
  reader.readAsText(file);
}

/*@ loadSourceFile :: (scope, file) => void */
function loadLocalFile($scope, file){
  if (window.File && window.FileList && window.FileReader && file) {
    if (file.type.match('text')) {
      fileText(file, function(srcText){ 
        setSourceCode($scope, file.name, srcText) 
      });
    } else { 
      alert("Can only load text files.");
    }
  } else {
    alert("Cannot load files: browser does not support File API");
  }
}


/*******************************************************************************/
/** Extracting JSON Results ****************************************************/
/*******************************************************************************/

function getResult(d) { 
  var res = "crash";
  if (d) {
    res = d.status; 
  }
  return res;
}

function getWarns(d){ 
  var ws = [];
  if (d && d.errors){
    var ws = d.errors.map(function(x){ 
               return x.message;
             });
  }
  return ws;
}


/*******************************************************************************/
/************** Top-Level Demo Controller **************************************/
/*******************************************************************************/

var debugData   = null;
var debugResult = null;
var debugResp   = 0;
var debugFiles  = null;

function LiquidDemoCtrl($scope, $http, $location) {

  // Populate list of demos
  $scope.basicDemos    = getDemos("basic")  ;  
  $scope.measureDemos  = getDemos("measure");
  $scope.abstRefDemos  = getDemos("absref") ;
  $scope.tutorialDemos = getDemos("tutorial") ;

  // Clear Status when editor is changed
  progEditor.on("change", function(e){ 
    $scope.$apply(function(){
      clearStatus($scope);
    });
  });

  // Load a particular demo
  $scope.loadSource   = function(demo){
    var srcName = demo.file;
    var srcURL  = getSrcURL(srcName);
    $http.get(srcURL)
         .success(function(srcText) { setSourceCode($scope, srcName, srcText); })
         .error(function(data, stat){ alert("Horrors: No such file! " + srcURL); })
         ;
  };

  // Initialize with Test000.hs
  $scope.loadSource($scope.basicDemos[1]);

  // Extract demo name from URL 
  $scope.$watch('location.search()', function() {
    $scope.demoName = ($location.search()).demo;
    if ($scope.demoName in allDemos) 
      $scope.loadSource(getDemo($scope.demoName));
    }, true);

  // Update demo name in URL 
  $scope.changeTarget = function(demo) {
     $location.search('demo', demo.file);
     $scope.loadSource(demo);
  };
  
  // Load a local file into editor
  $scope.readFile = function () { 
    debugFiles = $scope.localFileName;
    loadLocalFile($scope, $scope.localFileName);
    // alert ('I WANNA read a file'); 
  };

  // http://www.cleverweb.nl/javascript/a-simple-search-with-angularjs-and-php/
  $scope.verifySource = function(){ 
    var query = { "program"  : progEditor.getSession().getValue() };

    setStatusChecking($scope);

    $http.post(getVerifierURL(), query)
         .success(function(data, status) {
            debugResp        = debugResp + 1; 
            $scope.status    = status;
            debugData        = data;
            $scope.result    = getResult(data);
            debugResult      = $scope.result;
            $scope.warns     = getWarns(data); 
            $scope.annotHtml = data.annotHtml;
            setStatusResult($scope, $scope.result);
           
            // This may be "null" if liquid crashed...
            if (data) { 
              setAnnots(data.types);
              setErrors(progEditor, data.errors);
            };
            
        })
         .error(function(data, status) {
            var msg = (data || "Request failed") + status;
            alert(msg);
         });
  };
}

/************************************************************************/
/***** Initialize Angular ***********************************************/
/************************************************************************/

// directive for file upload
var fileInput = function ($parse) {
    return {
        restrict: "EA",
        template: "<input type='file' class='filestyle' data-icon='false'  />",
        replace: true,          
        link: function (scope, element, attrs) {
            var modelGet = $parse(attrs.fileInput);
            var modelSet = modelGet.assign;
            var onChange = $parse(attrs.onChange);
            var updateModel = function () {
                scope.$apply(function () {
                    modelSet(scope, element[0].files[0]);
                    onChange(scope);
                });                    
            };
            element.bind('change', updateModel);
        }
    };
};

var demo = angular.module("liquidDemo", []);
demo.controller('LiquidDemoCtrl', LiquidDemoCtrl);
demo.directive('fileInput', fileInput);

