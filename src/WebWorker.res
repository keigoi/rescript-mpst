module MainSide = {
  type transfer
  external transfer: 'a => transfer = "%identity"
  type worker<'v, 'w>
  @send external postMessage: (worker<'v, 'w>, 'v, array<transfer>) => unit = "postMessage"
  @set external setOnmessage: (worker<'v, 'w>, {"data": 'w} => unit) => unit = "onmessage"
  external newWorker: 'a => worker<'v, 'w> = "%identity"
}

module WorkerSide = {
  external postMessage: 'v => unit = "postMessage"
  let setOnMessage: ({"data": 'v} => unit) => unit = f =>
    %raw(`
    f => { onmessage = f }
  `)(f)
}
