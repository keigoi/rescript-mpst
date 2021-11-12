type t<'v, 'w>

@send external postMessage: (t<'v, 'w>, 'v) => unit = "postMessage"
@set external setOnmessage: (t<'v, 'w>, {"data": 'w} => unit) => unit = "onmessage"

type message_channel<'v, 'w> = {port1: t<'v,'w>, port2: t<'w,'v>}

@new external newMessageChannel: unit => message_channel<'v, 'w> = "MessageChannel"
