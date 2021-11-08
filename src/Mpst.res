

// OCaml-MPST の出力(送信) の型: {"alice": {"hello": out<int, next> } }

// let send : (out<'v, 'cont>, 'v) => 'cont = ...

// 何か送りたいときは. もし、 ch : {"alice": {"hello": out<int, next> } } のとき
// alice に hello(100) を送る:
// let ch = send(ch["alice"]["hello"], 100)

// 「メソッド」(フィールド) を表す型
type method<'obj,'v> = {call_obj:'obj => 'v, make_obj:'v => 'obj}

// 例: hello というフィールド
let hello_field = { call_obj: obj => obj["hello"], make_obj: v => {"hello": v} }

/// フィールド？
/* Java だと
class Apple {
  int weight; <-- こいつ
}
Apple a = new Apple();
a.weight // <-- フィールド weight を取り出す

Python
apple = { \
  weight:  \ # <-- こいつ
  100
}
apple["weight"] <-- フィールド weight を取り出す
apple["weight"] = 200 <- フィールド weight に書き込む

JavaScript
apple = {
  "weight": // ここの "" はなくてもいい
  100
}
apple["weight"] <-- フィールド weight を取り出す
apple["weight"] = 200 <- フィールド weight に書き込む
要するに JS のフィールドとは辞書の「キー」のこと
*/


// 使い方：
let myhello = {"hello": 123}
let hellonum = hello_field.call_obj( myhello ) // 123

// というインタフェースを実現するために.
// {"alice": {"hello": out<int, next> } }
// というオブジェクトを生成する手段が必要.
// method は、 method< {"hello": 'v}, 'v> .. という形を持っている
// これは、「 "hello" というフィールド 」を表す

// その中身は：
// { call_obj: {"hello": 'v} => 'v, make_obj: 'v => {"hello": 'v} }
// という型を持つ、
// { call_obj: obj => obj["hello"],
//   make_obj: v => {"hello": v}}

// ちなみに ReScript の {foo:int} は OCaml のレコード相当で、 {"foo":int} はオブジェクト相当
// どちらも JavaScript の オブジェクトに翻訳される。
// つまり ReScript {foo:100} ==> JavaScript {"foo":100}
// つまり ReScript {"foo":100} ==> JavaScript {"foo":100}

/*
最終的に「フィールド」はここで使う：

let g = (alice --> bob) hello @@ (bob --> carol) hello @@ (carol --> alice) hello finish

let (cha, chb, chc) = extract g

この hello が↑のように定義されているおかげで

send(cha["bob"]["hello"], 100)

のように書ける (ここで bob にも フィールド "bob" が入っている )

cha の中身はだいたいこんな感じになる：

{"bob": {"hello" : {cont: {"carol": {wrapper: [#hello(_, ())]} }} }}

こんなふうに使う：
let cha = send(cha["bob"]["hello"], 100)
let #hello(v, cha) = receive(cha["carol"])
console.log(v);
close(cha)

この

{"bob": {"hello" : {cont: {"carol": {wrapper: [#hello(_, ())]} }} }}

を

(alice --> bob) hello @@ (bob --> carol) hello @@ (carol --> alice) hello finish

で作るために

let bob = {role_label={call_obj:obj=>obj["bob"], make_obj:v => {"bob":v}}, ...}
let hello = {call_obj: obj=>obj["hello"], make_obj: v => {"hello", v}}

を前もって保持しておくのが、 method<..., ...> という型。

*/

// こちらはヴァリアント (のコンストラクタ) を表す (保持する) 型。
type variant<'var,'v> = {make_var:'v => 'var, match_var:'var => option<'v>, }

// 先ほどの hello は送信に使われていたが、受信側でも #hello を受け取れるようにする必要がある。
// そのため
// let hello = {make_var: v => #hello(v), match_var: ...}
// のように、#hello(データ) という形の値を作るための関数を保持する必要がある

// 略。 'l, 'r のフィールドが互いに疎である前提で、それらを結合したり分離したりする
type disj<'lr,'l,'r> =
  {concat: (('l, 'r) => 'lr), split: 'lr => ('l, 'r)}

// セッションを保持する型. 再帰のために lazy_tという型を使っており、そのため二つの宣言に分けてつくられている
type session0<'t> = {session:'t, merge:('t,'t)=>'t}
type session<'t> = lazy_t<session0<'t>>
// 例：
// 
/*
できれば、 session<'t> みたいな型は作りたくないのだが、…
't同士の「マージ」のための関数と、 'tそれ自体をペアにしておくことが必要なので、ここで作っている
let cha = 
  lazy {session: {"bob": {"hello" : {cont: {"carol": {wrapper: [#hello(_, ())]} }} }}, 
        merge: ( ...) => .. 略}

どういう意味か？
session （フィールドのほう）は、そのまま↑のような {"bob": ...} のようなチャンネルの構造を保持する
merge は、2つのチャンネルを「マージ」する関数

マージとは？

これは、分岐するプロトコルにおいて、受動的な参加者が使うチャンネルを矛盾なく合成する方法
例：
choice_at alice 
  ((alice --> bob) hello @@ finish)
  ((alice --> bob) goodbye @@ finish)
ここで、 bob は alice が hello, goodbye のどちらかを送るのを待つ「外部選択」(external choice: 受動的な選択)を行う。
より細かくみていくと、
  (alice --> bob) hello
の部分で、 bob のために {"alice": [#hello(_, ())]} というチャンネルが
  ((alice --> bob) goodbye @@ finish)
の部分で、 bob のために {"alice": [#goodbye(_, ())]} というチャンネルができる。
そして、 choice_at という関数は、「マージ」により、これら
{"alice": [#hello(_, ())]}
{"alice": [#goodbye(_, ())]}
というチャンネルから、
{alice:[#hello(_, ()), #goodbye(_, ())]}
という、hello, goodbye の両方を受信できる (受信させる) チャネルを作る
*/

type global<'a,'b,'c> = (session<'a>, session<'b>, session<'c>)


type lens<'a,'b,'s,'t> =  {get: 's => session<'a>, put: ('s, session<'b>) => 't}

let lens_a = {get:((a,_,_)) => a, put:((_,b,c), a) => (a,b,c)}
let lens_b = {get:((_,b,_)) => b, put:((a,_,c), b) => (a,b,c)}
let lens_c = {get:((_,_,c)) => c, put:((a,b,_), c) => (a,b,c)}

type role<'a,'b,'s,'t,'obj,'v> = 
  {role_meth:method<'obj,'v>, role_lens:lens<'a,'b,'s,'t>}

let alice = {
  role_meth: {
    call_obj: obj => obj["alice"],
    make_obj: v => {"alice":v}
  },
  role_lens: lens_a
}
let bob = {
  role_meth: {
    call_obj: obj => obj["bob"],
    make_obj: v => {"bob":v}
  },
  role_lens: lens_b
}
let carol = {
  role_meth: {
    call_obj: obj => obj["carol"],
    make_obj: v => {"carol":v}
  },
  role_lens: lens_c
}

type label<'obj,'t,'var,'u> = {label_meth:method<'obj,'t>, label_var:variant<'var,'u>}

let hello = 
  {
    label_meth: {
      call_obj: obj => obj["hello"], 
      make_obj: v => {"hello":v}
    },
    label_var: {
      match_var: var => switch var { | (#hello(v)) => Some(v) | _ => None },
      make_var: v => #hello(v)
    }
  }

let add = 
  {
    label_meth: {
      call_obj: obj => obj["add"], 
      make_obj: v => {"add":v}
    },
    label_var: {
      match_var: var => switch var { | (#add(v)) => Some(v) | _ => None },
      make_var: v => #add(v)
    }
  }

let res = 
  {
    label_meth: {
      call_obj: obj => obj["res"], 
      make_obj: v => {"res":v}
    },
    label_var: {
      match_var: var => switch var { | (#res(v)) => Some(v) | _ => None },
      make_var: v => #res(v)
    }
  }

let bye = 
  {
    label_meth: {
      call_obj: obj => obj["bye"], 
      make_obj: v => {"bye":v}
    },
    label_var: {
      match_var: var => switch var { | (#bye(v)) => Some(v) | _ => None },
      make_var: v => #bye(v)
    }
  }


// 受信のための型
// variant<...> は、ヴァリアントのコンストラクタを表すと↑で書いた。
// 具体的には
// variant<#hello(int,next), (int,next)>

type rec wrapper<_> = 
  Wrap(variant<'var,('v,'c)>, session<'c>) : wrapper<'var>

// これはヴァリアント型 (#が無いほうのヴァリアント型. )
// ReScript では、ヴァリアント型は
// type fruit = Apple(int) | Orange(string) | Banana(float)
// のように宣言する。
// ↑の type rec は、 GADT (Generalised Algebraic Data Type) の構文を使っており、
// fruit を GADT で書くと次のようになる:
// type rec fruit =
//    Apple(int) : fruit
//  | Orange(string) : fruit
//  | Banana(float) : fruit
// 要するに wrapper は
// - 「コンストラクタが Wrap 一つしかない」ヴァリアント
// - Wrap のパラメータは variant<'var,('v,'c)> と session<'c>
// これは、  wrapper<#hello(int, next)> .. のような使われ方になり、
// 「hello(int) を受信したら next に続く」の部分を表す
// なのだが、重要な役割が1つ
// variant<'var,('v,'c)>, session<'c>
// Wrap(variant<'var,('v,'c)>, session<'c>) : wrapper<'var>
// の、wrapper<'var> の 'var の部分が、「外から見える」型変数なのだが、
// それ以外に、 'v (int とかのペイロードに相当), 'c (nextの部分に相当) という、「外から見えない」(ある意味プライベートな)型変数が現れている
// このような型を 「存在型」という。 wrapper は存在型を使いたいがためにここで定義されている

// （コピー）
// 先ほどの hello は送信に使われていたが、受信側でも #hello を受け取れるようにする必要がある。
// そのため
// let hello = {make_var: v => #hello(v), match_var: ...}
// のように、#hello(データ) という形の値を作るための関数を保持する必要がある

// ここで wrapper という型は、session と variant の両方を持っておくためのデータ構造.
// 例えば、 bob が alice から hello を受信するとき
// Wrapper({make_var:v => #hello(v)}, {session: {"carol":{"hello":...}}})

// 受信のための型
type inp<'var> = 
  {wrappers: list<wrapper<'var>>, /* port: message_port<> */}

// 単に wrapper のリストになっている。
// 例えば hello, goodbye の2つを受信できるケースでは
// {wrappers: list{ Wrap({make_var: v => #hello(v), ..}, next1), Wrap({make_var: v => #goodbye(v), ..}, next2) }}
// という2つの wrapper を持っている

// 使う側からはこんなふうに見える
// let receive : inp<'var> => 'var = ...
// 例: bobの、aliceからの hello/goodbye の受信
// switch(receive(chb["alice"])) {
// | #hello(v, chb) => ...
// | #goodbye(v, chb) => ...
// } 
// ここで、 #hello とか #goodbye とかのコンストラクタを receive が返せるようにするために、
// variant<#hello('v), 'v> とかの型が必要になっている

type out<'v,'s> = 
  {out_cont:session<'s>, /* port: message_port<> */}

// 入力チャネル ( {"carol": {wrappers: list{...}}} ) をマージする関数
let merge_inp : 'a. (method<'a, inp<'var>>, 'a,'a) => 'a = (meth/*carolとかのフィールド*/, l, r) => {
  let l = meth.call_obj( l ) // {"carol": ... } をひっぺがす
  let r = meth.call_obj( r ) // おなじ
  let inp = {wrappers: List.append(l.wrappers, r.wrappers) } // wrappers の #hello と #goodby を一つのリストに
  meth.make_obj( inp ) // もういちど {"carol": ...} をかぶせる
}

let closed : session<unit> = lazy ({session:(), merge: ((),())=>()})

let finish : global<unit,unit,unit> = (closed, closed, closed)

// OCaml-MPST でいう --> 
// ReScript には --> などの演算子を定義する機能が (いまのところ?) ない　（復活はするらしい？)
// そこで
// (a --> b) label g
// は
// comm(a,b,label,g)
// というふうに書くことにする
// これは、「次にやること」プロトコル g から、今やること (a --> b) label g を作る関数 
// 例えば (alice --> alice) hello g のように self-sent なプロトコルを書いたとき
// aliceが送信→受信という順番のプロトコルにならないと、次に進めない
let comm 
: 'from 'to_ 'outlab 'inplab 's 't 'v 'next 'mid 'cur.
  role<'s, 'to_, 'mid, 'cur, 'from, inp<'inplab>> =>
  role<'t, 'from,'next, 'mid, 'to_, 'outlab> =>
  label<'outlab, out<'v,'s>, 'inplab, ('v,'t)> =>
  'next => 'cur
  = (alice, bob, hello, next_triple) => {

  let bob_next = bob.role_lens.get( next_triple )
  let bob_inp /* inp<#hello('v,'bob_next)> */ = {wrappers: list{ Wrap(hello.label_var, bob_next) }}
  let bob_inp /* {"alice": inp< #hello<'v, 'bob_next>>}*/ = alice.role_meth.make_obj/*{"alice":..}をかぶせる関数*/( bob_inp )
  let bob_inp =
    lazy {
      session: bob_inp,
      merge: merge_inp( alice.role_meth )
    }
  let mid_triple = bob.role_lens.put(next_triple, bob_inp)
  let alice_next = alice.role_lens.get( mid_triple )
  let alice_out = {out_cont: alice_next}
  let alice_out /* {"bob": {"hello": out<'v, 'alice_next>}} */= 
      bob.role_meth.make_obj( hello.label_meth.make_obj( alice_out ) )
  let alice_out =
    lazy {
      session: alice_out,
      merge: (x,_) => x
    }
  let current_triple = alice.role_lens.put(mid_triple, alice_out)
  current_triple
}

// let fix
// : 'a 'b 'c. (global<'a,'b,'c> => global<'a,'b,'c>) => global<'a,'b,'c>
//  = f => {
//   let rec self = 
//     lazy {
//       let a = (lazy (switch Lazy.force(self) { | (a,_,_) => a}))
//       let b = (lazy (switch Lazy.force(self) { | (_,b,_) => b}))
//       let c = (lazy (switch Lazy.force(self) { | (_,_,c) => c}))
//       f((a,b,c))
//     }
//   Lazy.force(self)
// }

let merge_global : 'a 'b 'c. (global<'a,'b,'c>, global<'a,'b,'c>) => global<'a,'b,'c> = (left, right) => {
  let (a1,b1,c1) = left
  let (a2,b2,c2) = right
  let a = lazy {
    let a1 = Lazy.force(a1)
    let a2 = Lazy.force(a2)
    {session: a1.merge( a1.session, a2.session ), merge:a1.merge}
  }
  let b = lazy {
    let b1 = Lazy.force(b1)
    let b2 = Lazy.force(b2)
    {session:b1.merge(b1.session,b2.session), merge:b1.merge}
  }
  let c = lazy {
    let c1 = Lazy.force(c1)
    let c2 = Lazy.force(c2)
    {session:c1.merge(c1.session,c2.session), merge:c1.merge}
  }
  (a,b,c)
}

// choice_at は分岐の構文。
/*
  choice_at alice disj
  (alice, (alice --> bob) hello finish)
  (alice, (alice --> bob) goodbye finish)

  * ２回 alice を書かないとだめ
  * disj という謎の引数がある

  やっていることは、
  * 受動的な参加者用チャネルのマージ (↑の merge_global を使う)
  * 能動的な参加者(alice)用チャネルの「結合」

  結合とは？
  left(alice) = {"bob": {"hello": {cont:closed}}}
  right(alice) = {"bob": {"goodbye": {cont:closed}}}
  これらを一つにする。マージとの違いは、オブジェクト {"hello": ...} {"goodbye": ...} を一つにしていること
  lr(alice) = {"bob": {"hello":.., "goodbye":..}}

  なぜこれが特別扱いされているか？
  出力チャネルは オブジェクト型 {"hello":...} {"goodbye":...} で表されていて
  これらは「まったく違う型」
  つまり、 以下の left と right は (aliceの部分で) 違う型をもつ
  left:  global<alice_left, bob, carol>
  right: global<alice_right, bob, carol>
    なぜ？
    実は、「受信」は、サブタイピングにより、同じ型に アップキャストできるため。
    たとえば、 inp<#hello(int,next1)> と inp<#goodbye(float,next2)> は、
    同じ型 inp<#hello(int,next1) | #goodbye(float,next2)> にアップキャストできる
  そこで、left, right の両方の alice の部分に unit (closed) を入れてやる
  で、disj を使って alice_left, alice_right を結合し、
  もともと　alice があった部分に戻してやる
*/
let choice_at 
: 'cur 'a 'b 'c 'left 'right 'lr 'l 'r 'x.
  (role<unit, 'lr, global<'a,'b,'c>, 'cur, 'x, _>,
   disj<'lr, 'l, 'r>,
   (role<'l, unit, 'left, global<'a,'b,'c>, 'x, _>, 'left),
   (role<'r, unit, 'right, global<'a,'b,'c>, 'x, _>, 'right))
   => 'cur
 = (alice, disj, (alice1,left), (alice2,right)) => {
  let alice_l = alice1.role_lens.get(left)
  let alice_r = alice2.role_lens.get(right)
  // alice の部分に 同じ型 (とりあえずclosed) を入れる --> left と right は同じ型になり、マージ可能になる ()
  let left = alice1.role_lens.put(left, closed)
  let right = alice2.role_lens.put(right, closed)
  let mid = merge_global(left, right)
  let alice_lr = lazy {
    let alice_l = Lazy.force(alice_l)
    let alice_r = Lazy.force(alice_r)
    let merge_disj = (s1,s2) => {
      let (s1l, s1r) = disj.split(s1)
      let (s2l, s2r) = disj.split(s2)
      let sl = alice_l.merge(s1l, s2l)
      let sr = alice_r.merge(s1r, s2r)
      disj.concat(sl, sr)
    } 
    let alice_lr = 
      {session: disj.concat(alice_l.session, alice_r.session),
       merge: merge_disj}
    alice_lr
  }
  let cur = alice.role_lens.put(mid, alice_lr)
  cur
}

