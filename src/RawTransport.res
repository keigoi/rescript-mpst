type role_tag = Types.polyvar_tag

type label_tag = Types.polyvar_tag

type payload

type mpst_msg = (label_tag, payload)

type mpchan = {
  self: role_tag,
  channels: Js.Dict.t<MessagePort.t<mpst_msg>>,
}

external payload_cast: 'v => payload = "%identity"

let raw_send: 'v. (mpchan, role_tag, label_tag, 'v) => unit = (mpchan, role, label, v) => {
  let ch = Js.Dict.unsafeGet(mpchan.channels, role)
  ch->MessagePort.postMessage((label, payload_cast(v)))
}

let raw_receive: 'v. (mpchan, ~from: role_tag) => Promise.t<(label_tag, 'v)> = (
  mpchan,
  ~from as role,
) => {
  let ch = Js.Dict.unsafeGet(mpchan.channels, role)
  Promise.make((_resolve, _reject) =>
    ch->MessagePort.setOnmessage(_e => {
      ()
    })
  )
}
