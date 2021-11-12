module MainSide = {
  open WebWorker.MainSide
  
  type mpstport = MessagePort.t<RawTransport.mpst_msg, RawTransport.mpst_msg>
  type mpstworker = worker<array<mpstport>, unit>

  let make_ports: (
    ~workers: int,
  ) => array<array<MessagePort.t<RawTransport.mpst_msg, RawTransport.mpst_msg>>> = (
    ~workers as cnt,
  ) => {
    let arr = Belt.Array.makeBy(cnt, i =>
      Belt.Array.makeBy(cnt - i, _ => {
        MessagePort.createMessageChannel()
      })
    )
    Belt.Array.makeBy(cnt, i =>
      Belt.Array.makeBy(cnt, j => {
        if i >= j {
          arr[i][j].port1
        } else {
          arr[j][i].port2
        }
      })
    )
  }

  let initWorkers: array<mpstworker> => unit = workers => {
    let cnt = Belt.Array.length(workers)
    let ports_array = make_ports(~workers=cnt)
    Js.Array.mapi((worker, i) => {
      let ports = ports_array[i]
      worker->postMessage(ports, Js.Array.map(transfer, ports))
    }, workers)->ignore
    ()
  }
}
