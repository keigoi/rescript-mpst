module MainSide = {
  open WebWorker.MainSide

  type mpstport = MessagePort.t<RawTransport.mpst_msg, RawTransport.mpst_msg>
  type mpstworker = worker<RawTransport.mpchan, unit>

  let make_ports: (~roles: array<RawTransport.role_tag>) => Js.Dict.t<RawTransport.mpchan> = (
    ~roles,
  ) => {
    let cnt = Js.Array.length(roles)
    let arr = Belt.Array.makeBy(cnt, i =>
      Belt.Array.makeBy(i + 1, _ => {
        MessagePort.newMessageChannel()
      })
    )
    Js.List.init(cnt, (. i) => {
      let portlist = Js.List.init(cnt, (. j) => {
        let port = if j < i {
          arr[i][j].port1
        } else if j > i {
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

  let initWorkers0: (
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

  let initWorkers: (
    Mpst.global<'a, 'b, 'c>,
    Mpst.role<'t, _, Mpst.global<'a, 'b, 'c>, _, _, _>,
    array<(RawTypes.polyvar_tag, mpstworker)>,
  ) => Mpst.session<'t> = (_g, mainrole, workers) => {
    let (mainrole_tag, _) = Raw.destruct_polyvar(mainrole.role_label.closed_make(Raw.dontknow()))
    let mpchan = initWorkers0(mainrole_tag, workers)
    {mpchan: mpchan, dummy_witness: Raw.dontknow()}
  }
}

module WorkerSide = {
  let init: (
    Mpst.global<'a, 'b, 'c>,
    Mpst.role<'t, _, Mpst.global<'a, 'b, 'c>, _, _, _>,
  ) => Promise.t<Mpst.session<'t>> = (_g, _role) => {
    Promise.make((resolve, _reject) => {
      WebWorker.WorkerSide.setOnMessage(e => {
        let sess = {Mpst.mpchan: e["data"], dummy_witness: Raw.dontknow()}
        resolve(. sess)
      })
    })
  }
}
