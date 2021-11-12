type t<'v>
type msg<'v> = {"data": 'v}

@send external postMessage: (t<'v>, 'v) => unit = "postMessage"
@set external setOnmessage: (t<'v>, msg<'v> => unit) => unit = "onmessage"
