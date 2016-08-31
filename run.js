// compile Test.App.elm with:
//		elm make Test/App.elm --output test.js

// load Elm module
const elm = require('./test.js');

// get Elm ports
const ports = elm.Test.App.worker().ports;

// keep our app alive until we get an exitCode from Elm or SIGINT or SIGTERM (see below)
const keepAlive = new Promise((resolve, reject) => {
	ports.node.subscribe(exitCode => {
		console.log('exit code from Elm:', exitCode);
		resolve(exitCode);
	});
}).then(exitCode => process.exit(exitCode));

process.on('uncaughtException', err => {
	// logger.error({err: err}, `Uncaught exception:`);
	console.log({err: err}, `Uncaught exception:`);
	process.exit(1);
});
process.on('unhandledRejection', (reason, p) => {
	// logger.error("Unhandled Rejection at: Promise ", p, " reason: ", reason);
	console.log("Unhandled Rejection at: Promise ", p, " reason: ", reason);
	process.exit(1);
});
process.on('SIGINT', () => {
	// logger.info(`SIGINT received.`);
	console.log(`SIGINT received.`);
	process.exit(0);
});
process.on('SIGTERM', () => {
	// logger.info(`SIGTERM received.`);
	console.log(`SIGTERM received.`);
	process.exit(0);
});
