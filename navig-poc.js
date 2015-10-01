

var handlers = [];

var parsedTree;

(function(check_pred){

var tracer_items = [];

var tracer_length = 0;
var tracer_pos = 0; 

$("#source_code").val(source_file);


function stepTo(step) {
  tracer_pos = step;
  updateSelection();
}

// Take a predicate in form of a JavaScript code (string) and returns either true or an error message (string).
function goToPred(pred) {

  function check(i){
    var item = datalog[i];
    var jsheap = jsheap_of_heap(item.heap);
    var obj = jsenv_of_env(jsheap, item.env);
    var objX = {};
    if (item.ctx !== undefined){
        objX = jsenv_of_env(jsheap, item.ctx);
    }
    objX.line = item.line;
    objX.type = item.type;
    objX.heap = jsheap;
    obj.X = objX; // If we want to change the “X” identifier, just change this line.
    try {
      if (check_pred(pred, obj)){
          stepTo(i);
          return true;
      }
    } catch(e){
      error++;
    }

    return false;
  }

  var error = 0;

  if (datalog.length === 0)
      return false;

  for (var i = (tracer_pos + 1) % datalog.length, current = tracer_pos;
       i !== current;
       i++, i %= datalog.length)
    if (check(i))
      return true;

  if (check(tracer_pos))
    return true;

  if (error === datalog.length)
    return "There was an execution error at every execution of your condition: are you sure that this is a valid JavaScript code?";

  return "Not found";
}

function button_reach_handler() {
  var pred = $("#text_condition").val();
  var res = goToPred(pred);
  if (res !== true){
    $("#action_output").html(res);
    var timeoutID =
        window.setTimeout(function() {
            $("#action_output").html(""); }, 3000);
  }
};

$('#text_condition').keypress(function(e){
	var keycode = (e.keyCode ? e.keyCode : e.which);
	if (keycode == '13') {
		button_reach_handler();
	}
});

$("#button_reach").click(button_reach_handler);


$("#navigation_step").change(function(e) {
  var n = + $("#navigation_step").val();
  n = Math.max(0, Math.min(tracer_length - 1, n));
  stepTo(n);
});

$("#button_run").click(function() {
  try {
    var code = source.getValue();
    //console.log(code);
    parsedTree = esprima.parse(code, {loc:true});
    // console.log(parsedTree);
    program = esprimaToAST(parsedTree);
    // console.log(program);
    run();
    $("#action_output").html("Run successful!");
  } catch(_){
    $("#action_output").html("Error during the run.");
  };
  var timeoutID = window.setTimeout(function() { $("#run_output").html(""); }, 1000);
});

$("#button_reset").click(function() {
 stepTo(0);
});

$("#button_prev").click(function() {
 stepTo(Math.max(0, tracer_pos-1));
});

$("#button_next").click(function() {
 stepTo(Math.min(tracer_length-1, tracer_pos+1));
});


// Assumes tracer_files to be an array of objects with two field:
// - file, containing the name of the file,
// - contents, a string containing its source code

// Assumes tracer_items to be an array with items, e.g.:
// { type: 'enter', file: 'foo.ml', start_line: 4, start_col: 0, end_line: 5, end_col: 10 },
// { type: 'exit', file: 'foo.ml', start_line: 4, start_col: 0, end_line: 5, end_col: 10 },
// { type: 'other_event', file: 'foo.ml', start_line: 4, start_col: 0, end_line: 5, end_col: 10 },

function tracer_valid_pos(i) {
  return (i >= 0 && i < tracer_length);
}

// dir is -1 or +1
function shared_step(dir) { 
  var i = tracer_pos;
  i += dir; 
  if (! tracer_valid_pos(i)) 
     return; // not found, we don’t update the tracer position.
  tracer_pos = i;
}

// dir is -1 or +1,
// target (= target depth) is 0 for (next/prev) or -1 (finish)
function shared_next(dir, target) { 
  var i = tracer_pos;
  var depth = 0;
  var ty = tracer_items[i].type;
  if (dir === +1 && ty === 'exit') {
     depth = 1;
  } else if (dir === -1 && ty === 'enter') {
     depth = -1;
  } 
  while (true) {
     if (! tracer_valid_pos(i)) {
        tracer_pos = i - dir; // just before out of range
        return; // not found
     }
     if (i !== tracer_pos && depth === target) {
        tracer_pos = i;
        return;
     }
     var ty = tracer_items[i].type;
     if (ty === 'enter') {
        depth++;
     } else if (ty === 'exit') {
        depth--;
     }
     i += dir; 
  }
}

function restart() { tracer_pos = 0; }
function step() { shared_step(+1); }
function backstep() { shared_step(-1); }
function next() { shared_next(+1, 0); }
function previous() { shared_next(-1, 0); }
function finish() { shared_next(+1, -1); } 

var curfile = '';

var docs = {};
for (var i = 0; i < tracer_files.length; i++) {
  var file = tracer_files[i].file;
  var txt = tracer_files[i].contents;
  docs[file] = CodeMirror.Doc(txt, 'js');
}

var editor = null;
var source = null;

function viewFile(file) {
  if (curfile !== file) {
     curfile = file;
     editor.swapDoc(docs[curfile]);
     editor.focus();
     updateFileList();
  }
}

function updateFileList() {
  var s = '';
  for (var i = 0; i < tracer_files.length; i++) {
     var file = tracer_files[i].file;
     s += "<span class=\"file_item" + ((curfile == file) ? '_current' : '') + "\" onclick=\"viewFile('" + file + "')\">" + file + "</span> ";
  }
  $('#file_list').html(s);
}

function text_of_cst(c) {
  switch (c.tag) {
  case "cst_bool":
    return c.bool + "";
  case "cst_number":
    return c.number + "";
  default:
    return "unrecognized cst";
  }
}

var next_fresh_id = 0;

function fresh_id() {
  return "fresh_id_" + (next_fresh_id++);
}

function show_value(heap, v, target, depth) {
  var contents_init = $("#" + target).html();
  var s = "";
  switch (v.tag) {
    case "val_cst":
      s = text_of_cst(v.cst);
      break;
    case "val_loc":
      var contents_rest = "<span class='heap_link'><a onclick=\"handlers['" + target + "']()\">&lt;Object&gt;(" + v.loc + ")</a></span>";
      var contents_default = contents_init + contents_rest;
      function handler_close() {
        handlers[target] = handler_open;
        $("#" + target).html(contents_default);
        editor.focus();
      }
      function handler_open() {
        handlers[target] = handler_close;
        var obj = heap.get(v.loc).asReadOnlyArray(); // type object
        var count = 0;
        for (var x in obj) {
          if (obj[x] === undefined) continue; // LATER remove!
          count++;
          var targetsub = fresh_id();
          $("#" + target).append("<div style='margin-left:1em' id='" + targetsub + "'></div>");
          $("#" + targetsub).html(x + ": ");
          show_value(heap, obj[x], targetsub, depth-1);
        }
        if (count === 0)
          $("#" + target).append("<div style='margin-left:1em'>(empty object)</div>");
        editor.focus();
      };
      handlers[target] = handler_open;
      $("#" + target).html(contents_default);
      if (depth > 0)
        handler_open();
      return;
    case "val_abs":
      s = "&lt;Closure&gt;";
      break;
    default:
      s = "<pre style='margin:0; padding: 0; margin-left:1em'>" + JSON.stringify(v, null, 2) + "</pre>";
      break;
  }
  $("#" + target).append(s);
}

function updateContext(targetid, heap, env) {
  $(targetid).html("");
  if (env === undefined)
    return;
  array_of_env(env).map(function(env){
    var target = fresh_id();
    $(targetid).append("<div id='" + target + "'></div>");
    $("#" + target).html(env.name + ": ");
    var depth = 1;
    show_value(heap, env.val, target, depth);
  });
}


var source_select = undefined;

function updateSourceSelection() {
  if (source_select === undefined) {
    return; 
  }
  // TODO: rename column into col
  var anchor = {line: source_select.start.line-1 , ch: source_select.start.column };
  var head = {line: source_select.end.line-1, ch: source_select.end.column };
  source.setSelection(anchor, head);
  /* deprecated:
  var anchor = {line: source_select-1, ch: 0 };
  var head = {line: source_select-1, ch: 100 } */;
}

function updateSelection() {
  var item = tracer_items[tracer_pos];
  source.setSelection({line: 0, ch:0}, {line: 0, ch:0}); // TODO: fct reset

  if (item !== undefined) {
    // console.log(item);
    // $("#disp_infos").html();
    if (item.line === undefined)
      alert("missing line in log event");

    // source panel
    source_select = item.source_select;
    // console.log(source_select);
    updateSourceSelection();

    // ctx panel
    updateContext("#disp_ctx", item.heap, item.ctx);

    // file panel
    viewFile(item.file);
    //console.log("pos: " + tracer_pos);
    // var color = (item.type === 'enter') ? '#F3F781' : '#CCCCCC';
    var color = '#F3F781';
    $('.CodeMirror-selected').css({ background: color });
    $('.CodeMirror-focused .CodeMirror-selected').css({ background: color });
    var anchor = {line: item.start_line-1 , ch: item.start_col };
    var head = {line: item.end_line-1, ch: item.end_col };
    editor.setSelection(anchor, head);

    // env panel
    updateContext("#disp_env", item.heap, item.env);

    // navig panel
    $("#navigation_step").val(tracer_pos);
  }
  updateFileList();
  editor.focus();
}

source = CodeMirror.fromTextArea(document.getElementById('source_code'), {
  mode: 'js',
  lineNumbers: true,
  lineWrapping: true,
});
source.setSize(300, 150);

editor = CodeMirror.fromTextArea(document.getElementById('interpreter_code'), {
  mode: 'js',
  lineNumbers: true,
  lineWrapping: true,
  readOnly: true,
  extraKeys: {
     'R': function(cm) { restart(); updateSelection(); },
     'S': function(cm) { step(); updateSelection(); },
     'B': function(cm) { backstep(); updateSelection(); },
     'N': function(cm) { next(); updateSelection(); },
     'P': function(cm) { previous(); updateSelection(); },
     'F': function(cm) { finish(); updateSelection(); },
  },
});
editor.setSize(600,250);

/* ==> try in new version of codemirror*/
try {
  $(editor.getWrapperElement()).resizable({
    resize: function() {
      editor.setSize($(this).width(), $(this).height());
    }
  }); 
} catch(e) { }

editor.on('dblclick', function() {
  var line = editor.getCursor().line;
  var txt = editor.getLine(line);
  var prefix = "#sec-";
  var pos_start = txt.indexOf(prefix);
  if (pos_start === -1)
     return;
  var pos_end = txt.indexOf("*", pos_start);
  if (pos_end === -1)
     return;
  var sec = txt.substring(pos_start, pos_end);
  var url = "http://www.ecma-international.org/ecma-262/5.1/" + sec;
  window.open(url, '_blank');
});

editor.focus();



// used to ensure that most events have an associated term
function completeTermsInDatalog(items) {
  var last = undefined;
  for (var k = 0; k < datalog.length; k++) {
    var item = datalog[k];
    item.source_select = last;
    if (item.ctx !== undefined) {
      var ctx_as_array = array_of_env(item.ctx);
      if (ctx_as_array.length > 0 && ctx_as_array[0].name === "t") {
        var t = ctx_as_array[0].val;
        if (! (t === undefined || t.start === undefined || t.end === undefined)) {
          item.source_select = { start: t.start, end: t.end };
          // TODO: avoir un t.location
          last = t;
        }
      } 
    }
  }   
}



function run() {
  run_program(program);
  completeTermsInDatalog(datalog);
  tracer_items = datalog;
  tracer_length = tracer_items.length
  $("#navigation_total").html(tracer_length - 1);
  stepTo(0); // calls updateSelection(); 
}


// Note: for the demo
// run();
// stepTo(78);



/* demo
var j = jsheap_of_heap(heap);

for (var k = 0; k < datalog.length; k++) {
var item = datalog[k];
var jsheap = jsheap_of_heap(item.heap);
item.heap = jsheap;
item.env = jsenv_of_env(jsheap, item.env);
if (item.ctx !== undefined) {
item.ctx = jsenv_of_env(jsheap, item.ctx);
}
}
*/

}(function check_pred(p, obj) {
  with (obj){
    return eval(p)
  }
}));

