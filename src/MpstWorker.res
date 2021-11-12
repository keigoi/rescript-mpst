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

  let initWorkers: (RawTypes.polyvar_tag, array<(RawTypes.polyvar_tag, mpstworker)>) => unit = (
    mainrole,
    workers,
  ) => {
    let cnt = Belt.Array.length(workers) + 1
    let ports_array = make_ports(~workers=cnt)
    let ports_map = Js.Array.mapi((ports, i) => {
        let port_map = %raw(`{}`)
        Js.Array.mapi(((role, _worker), j) => {
            %raw(`(ports, port_map,role,j) => {
                port_map[role] = ports[j];
            }`)(ports, port_map, role, j)
        }, workers) -> ignore
        port_map
    }, ports_array)
    Js.Array.mapi(((_role, worker), i) => {
      let ports = ports_array[i]
      worker->postMessage(ports, Js.Array.map(transfer, ports))
    }, workers)->ignore
    ()
  }
}
