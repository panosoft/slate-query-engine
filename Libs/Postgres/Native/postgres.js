// Elm globals
const E = {
	Scheduler: {
		nativeBinding: _elm_lang$core$Native_Scheduler.nativeBinding,
		succeed:  _elm_lang$core$Native_Scheduler.succeed,
		fail: _elm_lang$core$Native_Scheduler.fail,
		rawSpawn: _elm_lang$core$Native_Scheduler.rawSpawn
	},
	Maybe: {
		Nothing: _elm_lang$core$Maybe$Nothing,
		Just: _elm_lang$core$Maybe$Just
	},
	Result: {
		Err: _elm_lang$core$Result$Err,
		Ok: _elm_lang$core$Result$Ok
	}
};
const cmd = require('@panosoft/elm-native-helpers/cmd')(E);
const pg = require('pg');

var _user$project$Native_Postgres = function() {
	const createConnectionUrl = (host, port, database, user, password) => `postgres://${user}:${password}@${host}:${port}/${database}`;

	const _disconnect = (dbClient, discardConnection, cb) => {
		try {
			// pooled client
			// passing truthy err will destroy client rather than returning client to pool.
			if (dbClient.releaseClient)
				dbClient.releaseClient(discardConnection);
			// non-pooled client
			else
				dbClient.client.end();
			cb();
		}
		catch (err) {
			cb(err.message);
		}
	};
	const _connect = (timeout, host, port, database, user, password, cb) => {
		var expired = false;
		const timer = setTimeout(_ => {
			expired = true;
			cb(`Connection timeout after ${timeout/1000} seconds to ${host}:${port}/${database}`);
		}, timeout);
		pg.connect(createConnectionUrl(host, port, database, user, password), (err, client, done) => {
			try {
				clearTimeout(timer);
				if (expired)
					_disconnect(settings, dbClient);
				else {
					if (err)
						cb(`Attempt to retrieve pooled connection for ${host}:${port}/${database}.  Failed with: ${err.message}`);
					else {
						const dbClient = {client: client, releaseClient: done};
						cb(null, dbClient);
					}
				}
			}
			catch(err) {
				cb(err);
			}
		});
	};
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const connect = cmd.cmdCall6_1(_connect);
	const disconnect = cmd.cmdCall2_0(_disconnect, cmd.unwrap({1:'_0'}));

	return {
		connect: F7(connect),
		disconnect: F3(disconnect)
	};

}();
