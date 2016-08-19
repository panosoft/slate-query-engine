// compile Test.App.elm with:
//		elm make Test/App.elm --output test.js

// load Elm module
const test = require('./test.js');

// get Elm ports
const ports = test.Test.App.worker().ports;

// // every second send Elm the string 'testing'
// // first see subscription function in Elm
// // where the message DisplayInput will be sent when data is received by Elm code
// // then see DisplayInput message in the update function in Elm
// setInterval(_ => ports.testIn.send('testing'), 1000);
//
// // subscribe to the output of the Elm code
// // see: Tick message in the update function in Elm
// ports.testOut.subscribe(time => console.log('time from Elm:', time));

// this gets printed first
console.log('done');
