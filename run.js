// compile Test.App.elm with:
//		elm make Test/App.elm --output test.js

// load Elm module
const elm = require('./test.js');

// get Elm ports
const ports = elm.Test.App.worker().ports;

// keep our app alive until we get an exitCode from Elm or SIGINT or SIGTERM (see below)
setInterval(id => id, 86400);

ports.node.subscribe(exitCode => {
	// logger.info('exit code from Elm:', exitCode);
	console.log('exit code from Elm:', exitCode);
	process.exit(exitCode);
});

process.on('uncaughtException', err => {
	// logger.error({err: err}, `Uncaught exception:`);
	console.log(`Uncaught exception:`, {err: err});
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
