

type method<'obj,'v> = {call_obj:'obj => 'v, make_obj:'v => 'obj}
type variant<'var,'v> = {match_var:'var => option<'v>, make_var:'v => 'var}

type global<'a,'b,'c> = ('a, 'b, 'c)

type lens<'a,'b,'s,'t> =  {get: 's => 'a, put: ('s, 'b) => 't}

let lens_a = {get:(a,_,_) => a, put:((_,b,c), a) => (a,b,c)}
let lens_b = {get:(_,b,_) => b, put:((a,_,c), b) => (a,b,c)}
let lens_c = {get:(_,_,c) => c, put:((a,b,_), c) => (a,b,c)}

type role<'obj,'v,'a,'b,'s,'t> = 
  {role_meth:method<'obj,'v>, role_lens:lens<'a,'b,'s,'t>}

let a = {
  role_meth: {
    call_obj: obj => obj["a"],
    make_obj: v => {"a":v}
  },
  role_lens: lens_a
}
let b = {
  role_meth: {
    call_obj: obj => obj["b"],
    make_obj: v => {"b":v}
  },
  role_lens: lens_b
}
let c = {
  role_meth: {
    call_obj: obj => obj["c"],
    make_obj: v => {"c":v}
  },
  role_lens: lens_c
}

type label<'obj,'t,'var,'u> = {meth:method<'obj,'t>, var:variant<'var,'u>}

let add = 
  {
    meth: {
      call_obj: obj => obj["add"], 
      make_obj: v => {"add":v}
    },
    var: {
      match_var: var => switch var { | (#add(v)) => Some(v) | _ => None },
      make_var: v => #add(v)
    }
  }

let res = 
  {
    meth: {
      call_obj: obj => obj["res"], 
      make_obj: v => {"res":v}
    },
    var: {
      match_var: var => switch var { | (#res(v)) => Some(v) | _ => None },
      make_var: v => #res(v)
    }
  }

let bye = 
  {
    meth: {
      call_obj: obj => obj["bye"], 
      make_obj: v => {"bye":v}
    },
    var: {
      match_var: var => switch var { | (#bye(v)) => Some(v) | _ => None },
      make_var: v => #bye(v)
    }
  }

let finish = ((), (), ())

// type inp<'var> = {}

// let comm = (from,to_,label,next) => {
//   let to_ch = to_.get(next)
//   let next = to_.put(next, from.make_obj()

// }


