// external make_variant: (Transport.variant_tag, 't) => 'var = "make_variant";

let fail: 'a. unit => 'a = () => %raw(` () => { throw "rescript-mpst-fail"; }`)()

let dontknow: 'a. unit => 'a = () => %raw(`null`)

let assertfalse: 'a. unit => 'a = () => %raw(` () => { throw "rescript-mpst-assert-false"}`)()

let todo: 'a. unit => 'a = () => %raw(` () => { throw "rescript-mpst-todo"; }`)()

// let guarded_receive: 'a. (~guard: 'a => bool) => 'a = "guarded_receive"

let make_polyvar: 'a 'var. (Types.polyvar_tag, 'a) => 'var = (tag, v) =>
  %raw(` (tag, v) => ({"NAME":tag, "VAL":v})`)(tag, v)

let destruct_polyvar: 'a 'var. 'var => (Types.polyvar_tag, 'a) = var =>
  %raw(`(var_) => ([var_.NAME, var_.VAL])`)(var)
