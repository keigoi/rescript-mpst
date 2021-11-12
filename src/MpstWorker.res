module MainSide = {
  open WebWorker.MainSide

  type mpstport = MessagePort.t<RawTransport.mpst_msg, RawTransport.mpst_msg>
  type mpstworker = worker<RawTransport.mpchan, unit>

  let make_ports: (~roles: array<RawTransport.role_tag>) => Js.Dict.t<RawTransport.mpchan> = (
    ~roles,
  ) => {
    let cnt = Js.Array.length(roles)
    let arr = Belt.Array.makeBy(cnt, i =>
      Belt.Array.makeBy(cnt - i, _ => {
        MessagePort.newMessageChannel()
      })
    )
    Js.List.init(cnt, (. i) => {
      let portlist = Js.List.init(cnt, (. j) => {
        let port = if i >= j {
          arr[i][j].port1
        } else if i <= j {
          arr[j][i].port2
        } else {
          arr[i][j].port1 // FIXME self-sent message
        }
        (roles[j], port)
      })
      (roles[i], Js.Dict.fromList(portlist))
    })->Js.Dict.fromList
  }

  let newWorker = WebWorker.MainSide.newWorker

  let initWorkers: (
    RawTypes.polyvar_tag,
    array<(RawTypes.polyvar_tag, mpstworker)>,
  ) => RawTransport.mpchan = (mainrole, workers) => {
    let worker_roles = Array.map(((r, _)) => r, workers)
    let roles = Array.append([mainrole], worker_roles)
    let ports_map = make_ports(~roles)
    Js.Array.map(((role, worker)) => {
      let ports = Js.Dict.unsafeGet(ports_map, role)
      let transfers = Js.Array.map(WebWorker.MainSide.transfer, Js.Dict.values(ports))
      worker->postMessage(ports, transfers)
    }, workers)->ignore
    Js.Dict.unsafeGet(ports_map, mainrole)
  }
}

