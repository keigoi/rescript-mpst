// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function raw_send(mpchan, role, label, v) {
  var ch = mpchan[role];
  ch.postMessage([
        label,
        v
      ]);
  
}

function raw_receive(mpchan, role) {
  var ch = mpchan[role];
  return new Promise((function (resolve, _reject) {
                ch.onmessage = (function (e) {
                    var match = e.data;
                    return resolve([
                                match[0],
                                match[1]
                              ]);
                  });
                
              }));
}

exports.raw_send = raw_send;
exports.raw_receive = raw_receive;
/* No side effect */
