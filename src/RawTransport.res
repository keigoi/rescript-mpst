type role_tag = RawTypes.polyvar_tag

type label_tag = RawTypes.polyvar_tag

type payload

type mpst_msg = (label_tag, payload)

type mpchan = Js.Dict.t<MessagePort.t<mpst_msg, mpst_msg>>

external payload_cast: 'v => payload = "%identity"
external payload_uncast: payload => 'v = "%identity"

let raw_send: 'v. (mpchan, role_tag, label_tag, 'v) => unit = (mpchan, role, label, v) => {
  let ch = Js.Dict.unsafeGet(mpchan, role)
  ch->MessagePort.postMessage((label, payload_cast(v)))
}

let raw_receive: 'v. (mpchan, ~from: role_tag) => Promise.t<(label_tag, 'v)> = (
  mpchan,
  ~from as role,
) => {
  let ch = Js.Dict.unsafeGet(mpchan, role)
  Promise.make((resolve, _reject) =>
    ch->MessagePort.setOnmessage(e => {
      let (label, payload) = e["data"]
      resolve(. (label, payload_uncast(payload)))
    })
  )
}
