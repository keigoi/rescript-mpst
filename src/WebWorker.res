module MainSide = {
  type transfer
  external transfer: 'a => transfer = "%identity"
  type worker<'v, 'w>
  @send external postMessage: (worker<'v, 'w>, 'v, array<transfer>) => unit = "postMessage"
  @set external setOnmessage: (worker<'v, 'w>, {"data": 'w} => unit) => unit = "onmessage"
  external newWorker: 'a => worker<'v, 'w> = "%identity"
}

module WorkerSide = {
  let postMessage: 'v. 'v => unit = v => %raw(`v => self.postMessage(v)`)(v) 
  let setOnMessage: ({"data": 'v} => unit) => unit = f =>
    %raw(`
    f => { onmessage = f }
  `)(f)
}
