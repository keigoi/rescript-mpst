type session<'a> = {
  dummy_witness: 'a,
  mpchan: RawTransport.mpchan,
}

// [`Bob; `Alice] : [> `Bob | `Alice] list
// 多相ヴァリアントのコンストラクタ. v => #Bob(v) : open_variant<#Bob('v), 'v>
type open_variant<'var, 'v> = 'v => 'var

// #Bob(v) => v
type closed_variant<'var, 'v> = {
  closed_match: 'var => 'v,
  closed_make: 'v => 'var,
}

type disj<'lr, 'l, 'r> = {
  concat: (list<'l>, list<'r>) => list<'lr>,
  split: list<'lr> => (list<'l>, list<'r>),
}

type global<'a, 'b, 'c> = (session<'a>, session<'b>, session<'c>)

type lens<'a, 'b, 's, 't> = {
  get: 's => session<'a>,
  put: ('s, session<'b>) => 't,
}

let lens_a = {
  get: ((a, _, _)) => a,
  put: ((_, b, c), a) => (a, b, c),
}
let lens_b = {
  get: ((_, b, _)) => b,
  put: ((a, _, c), b) => (a, b, c),
}
let lens_c = {
  get: ((_, _, c)) => c,
  put: ((a, b, _), c) => (a, b, c),
}

type role<'a, 'b, 's, 't, 'obj, 'v> = {
  role_label: closed_variant<'obj, 'v>,
  role_lens: lens<'a, 'b, 's, 't>,
}
//

let alice = {
  role_label: {closed_match: (#Alice(v)) => v, closed_make: v => #Alice(v)},
  role_lens: lens_a,
}

let bob = {
  role_label: {closed_match: (#Bob(v)) => v, closed_make: v => #Bob(v)},
  role_lens: lens_b,
}

let carol = {
  role_label: {closed_match: (#Carol(v)) => v, closed_make: v => #Carol(v)},
  role_lens: lens_c,
}

type label<'obj, 't, 'var, 'u> = {
  label_closed: closed_variant<'obj, 't>,
  label_open: open_variant<'var, 'u>,
}

let hello = {
  label_closed: {closed_match: (#hello(v)) => v, closed_make: v => #hello(v)},
  label_open: v => #hello(v),
}

let goodbye = {
  label_closed: {closed_match: (#goodbye(v)) => v, closed_make: v => #goodbye(v)},
  label_open: v => #goodbye(v),
}

type out<'lab> = {__out_witness: 'lab}

type inp<'lab> = {__inp_witness: 'lab}

let list_match: ('a => 'b, list<'a>) => 'b = (_, _) => Raw.assertfalse()

let hello_or_goodbye = {
  split: lr => (list{#hello(list_match(x =>
          switch x {
          | #hello(v) => v
          | #goodbye(_) => Raw.dontknow()
          }
        , lr))}, list{#goodbye(list_match(x =>
          switch x {
          | #goodbye(v) => v
          | #hello(_) => Raw.dontknow()
          }
        , lr))}),
  concat: (l, r) => list{
    #hello(list_match((#hello(v)) => v, l)),
    #goodbye(list_match((#goodbye(v)) => v, r)),
  },
}

let to_bob = disj => {
  concat: (l, r) =>
    List.map(
      v => #Bob({__out_witness: v}),
      disj.concat(
        List.map((#Bob(v)) => v.__out_witness, l),
        List.map((#Bob(v)) => v.__out_witness, r),
      ),
    ),
  split: lr => {
    let (l, r) = disj.split(List.map((#Bob(v)) => v.__out_witness, lr))
    (List.map(v => #Bob({__out_witness: v}), l), List.map(v => #Bob({__out_witness: v}), r))
  },
}

let role_to_tag: role<_, _, _, _, _, _> => RawTypes.polyvar_tag = role => {
  let (roletag, _) = Raw.destruct_polyvar(role.role_label.closed_make(Raw.dontknow()))
  roletag
}

let open_variant_to_tag: 'var. open_variant<'var, _> => RawTypes.polyvar_tag = var => {
  let (roletag, _) = Raw.destruct_polyvar(var(Raw.dontknow()))
  roletag
}

let send: 'var 'lab 'v 'c. (
  session<'var>,
  open_variant<'var, out<'lab>>,
  open_variant<'lab, ('v, session<'c>)>,
  'v,
) => session<'c> = (sess, role, label, v) => {
  let roletag = open_variant_to_tag(role)
  let labeltag = open_variant_to_tag(label)
  RawTransport.raw_send(sess.mpchan, roletag, labeltag, v)
  {mpchan: sess.mpchan, dummy_witness: Raw.dontknow()}
}

let receive: 'var 'lab. (session<'var>, open_variant<'var, inp<'lab>>) => Js.Promise.t<'lab> = (
  sess,
  role,
) => {
  let roletag = open_variant_to_tag(role)
  RawTransport.raw_receive(sess.mpchan, ~from=roletag)->Promise.thenResolve(((labeltag, val)) => {
    let cont = {mpchan: sess.mpchan, dummy_witness: Raw.dontknow()}
    Raw.make_polyvar(labeltag, (val, cont))
  })
}

let close: session<unit> => unit = _ => ()

let x: 'a = 1

let \"-->": 'from 'to_ 'outlab 'inplab 's 't 'v 'next 'mid 'cur. (
  role<'s, 'to_, 'mid, 'cur, 'from, inp<'inplab>>,
  role<'t, 'from, 'next, 'mid, 'to_, out<'outlab>>,
  label<'outlab, ('v, session<'s>), 'inplab, ('v, session<'t>)>,
  'next,
) => 'cur = (_from, _to, _label, _next) => Raw.dontknow()

let finish: global<unit, unit, unit> = Raw.dontknow()

let choice_at: 'cur 'a 'b 'c 'left 'right 'lr 'l 'r 'x. (
  role<unit, 'lr, global<'a, 'b, 'c>, 'cur, 'x, _>,
  disj<'lr, 'l, 'r>,
  (role<'l, unit, 'left, global<'a, 'b, 'c>, 'x, _>, 'left),
  (role<'r, unit, 'right, global<'a, 'b, 'c>, 'x, _>, 'right),
) => 'cur = (_alice, _disj, (_alice1, _left), (_alice2, _right)) => Raw.dontknow()

let extract: 'a 'b 'c. (
  global<'a, 'b, 'c>,
  role<'t, _, global<'a, 'b, 'c>, _, _, _>,
) => session<'t> = (_g, _role) => Raw.todo()

// Example

let g = choice_at(
  alice,
  to_bob(hello_or_goodbye),
  (
    alice,
    \"-->"(alice, bob)(hello, \"-->"(bob, carol)(hello, \"-->"(carol, alice)(hello, finish))),
  ),
  (alice, \"-->"(alice, bob)(goodbye, \"-->"(bob, carol)(goodbye, finish))),
)

let a = () => {
  let ch = extract(g, alice)
  // send(ch["bob"]["hello"], 123)
  let ch = send(ch, x => #Bob(x), x => #hello(x), 123)
  receive(ch, x => #Carol(x))->Promise.thenResolve((#hello(_v, ch)) => close(ch))
}

let b = () => {
  let ch = extract(g, bob)
  receive(ch, x => #Alice(x))->Promise.thenResolve(ret => {
    let ch = switch ret {
    | #hello(_v, ch) => send(ch, x => #Carol(x), x => #hello(x), 123)
    | #goodbye(_v, ch) => send(ch, x => #Carol(x), x => #goodbye(x), "foo")
    }
    close(ch)
  })
}

let c = () => {
  let ch = extract(g, carol)
  receive(ch, x => #Bob(x))->Promise.thenResolve(ret => {
    let ch = switch ret {
    | #hello(_v, ch) => send(ch, x => #Alice(x), x => #hello(x), 123)
    | #goodbye(_v, ch) => ch
    }
    close(ch)
  })
}
