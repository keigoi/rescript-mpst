// Generated by ReScript, PLEASE EDIT WITH CARE


var MainSide = {};

function postMessage(v) {
  return (v => self.postMessage(v))(v);
}

function setOnMessage(f) {
  return (f => { onmessage = f })(f);
}

var WorkerSide = {
  postMessage: postMessage,
  setOnMessage: setOnMessage
};

export {
  MainSide ,
  WorkerSide ,
  
}
/* No side effect */
