
%%raw(`
function hello0(str) {
    console.log("Hello, " + str + "!")
}
`)

type rec t<'a> =
| A : t<int>
| B : t<string>

let f : type a. t<a> => a = (x) => 
  switch(x) {
  | A => 1
  | B => "abc"
  }
