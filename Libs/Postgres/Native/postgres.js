// Elm globals
const E = {
	A2: A2,
	A3: A3,
	A4: A4,
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
const read = require('stream-read');
const cmd = require('@panosoft/elm-native-helpers/cmd')(E);
const pg = require('pg');
const QueryStream = require('pg-query-stream');

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
	const _startQuery = (dbClient, sql, recordCount, cb) => {
		const stream = dbClient.client.query(new QueryStream(sql));
		return _nextQuery(dbClient, stream, recordCount, cb);
	};
	const _nextQuery = (dbClient, stream, recordCount, cb) => {
		var records = [];
		var count = 0;
		const processData = (err, data) => {
			if (err)
				cb(err);
			else {
				if (data)
					records[records.length] = JSON.stringify(data);
				if (!data || ++count >= recordCount) {
					cb(null, stream, records);
					return;
				}
				read(stream, processData);
			}
		};
		read(stream, processData);
	};
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const connect = cmd.cmdCall6_1(_connect);
	const disconnect = cmd.cmdCall2_0(_disconnect, cmd.unwrap({1:'_0'}));
	const startQuery = cmd.cmdCall3_2(_startQuery, cmd.unwrap({1:'_0'}));
	const nextQuery = cmd.cmdCall3_2(_nextQuery, cmd.unwrap({1:'_0'}));

	return {
		connect: F7(connect),
		disconnect: F3(disconnect),
		startQuery: F4(startQuery),
		nextQuery: F4(nextQuery)
	};

}();
