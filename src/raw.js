// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");

function fail(param) {
  return Curry._1((() => { throw "rescript-mpst-fail"; }), undefined);
}

function dontknow(param) {
  return null;
}

function assertfalse(param) {
  return Curry._1((() => { throw "rescript-mpst-assert-false"}), undefined);
}

function todo(param) {
  return Curry._1((() => { throw "rescript-mpst-todo"; }), undefined);
}

function make_polyvar(tag, v) {
  return ((tag, v) => ({"NAME":tag, "VAL":v}))(tag, v);
}

function destruct_polyvar($$var) {
  return ((var_) => ([var_.NAME, var_.VAL]))($$var);
}

exports.fail = fail;
exports.dontknow = dontknow;
exports.assertfalse = assertfalse;
exports.todo = todo;
exports.make_polyvar = make_polyvar;
exports.destruct_polyvar = destruct_polyvar;
/* No side effect */
