// Elm globals (some for elm-native-helpers and some for us)
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
	List: {
		fromArray: _elm_lang$core$Native_List.fromArray
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
// This module is in the same scope as Elm but all modules that are required are NOT
// So we must pass elm globals to it
const cmd = require('@panosoft/elm-native-helpers/cmd')(E);
const read = require('stream-read');
const pg = require('pg');
const QueryStream = require('pg-query-stream');

// HACK to keep pool from throwing uncatchable exeception on connection errors
// god I hate the pg library
pg.on('error', err => err);

var _user$project$Native_Postgres = function() {
	const createConnectionUrl = (host, port, database, user, password) => `postgres://${user}:${password}@${host}:${port}/${database}`;

	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const _disconnectInternal = (dbClient, discardConnection, nativeListener) => {
		if (nativeListener)
			dbClient.client.removeListener('error', nativeListener);
		// pooled client
		// passing truthy err will destroy client rather than returning client to pool.
		if (dbClient.releaseClient)
			dbClient.releaseClient(discardConnection);
		// non-pooled client
		else
			dbClient.client.end();
	};
	const _disconnect = (dbClient, discardConnection, nativeListener, cb) => {
		try {
			_disconnectInternal(dbClient, discardConnection, nativeListener);
			cb();
		}
		catch (err) {
			cb(err.message);
		}
	};
	const _connect = (timeout, host, port, database, user, password, connectionLostCb, cb) => {
		var expired = false;
		const timer = setTimeout(_ => {
			expired = true;
			cb(`Connection timeout after ${timeout/1000} seconds to ${host}:${port}/${database}`);
		}, timeout);
		pg.connect(createConnectionUrl(host, port, database, user, password), (err, client, done) => {
			try {
				clearTimeout(timer);
				if (expired)
					_disconnectInternal(dbClient, false);
				else {
					if (err)
						cb(`Attempt to retrieve pooled connection for ${host}:${port}/${database}.  Failed with: ${err.message}`);
					else {
						const dbClient = {client: client, releaseClient: done};
						const nativeListener = err => {
							try {
								_disconnectInternal(dbClient, true, nativeListener);
								E.Scheduler.rawSpawn(connectionLostCb(err.message));
							}
							catch (err) {
								// eat this error since we're have a bad connection anyway
								console.error("SHOULD NEVER GET HERE");
							}
						};
						dbClient.client.on('error', nativeListener);
						cb(null, dbClient, nativeListener);
					}
				}
			}
			catch(err) {
				cb(err);
			}
		});
	};
	const _query = (dbClient, sql, recordCount, nativeListener, cb) => {
		const options = {
			highWaterMark: 16 * 1024, // total number of rows buffered per DB access (used by readable-stream)
			batchSize: 1024 // number of rows read from underlying stream at a time (used by pg)
		};
		const stream = dbClient.client.query(new QueryStream(sql, null, options));
		stream.on('error', errMsg => {
			nativeListener('Stream error: ' + errMsg);
		});
		return _moreQueryResults(dbClient, stream, recordCount, cb);
	};
	const _moreQueryResults = (dbClient, stream, recordCount, cb) => {
		var records = [];
		var count = 0;
		const processData = (err, data) => {
			if (err)
				cb(err.message);
			else {
				try {
					if (data)
						records[records.length] = JSON.stringify(data);
					if (!data || ++count >= recordCount) {
						cb(null, stream, E.List.fromArray(records));
						return;
					}
					read(stream, processData);
				}
				catch (err) {
					cb(err.message);
				}
			}
		};
		read(stream, processData);
	};
	const _executeSQL = (dbClient, sql, cb) => {
		try {
			dbClient.client.query(sql, (err, result) => {
				if (err)
					cb(err.message);
				else
					cb(null, result.rowCount);
			});
		}
		catch(err) {
			cb(err.message);
		}
	};
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Subs
	const _listen = (dbClient, sql, routeCb, cb) => {
		_executeSQL(dbClient, sql, (err, _) => {
			const nativeListener = message => {
				E.Scheduler.rawSpawn(routeCb(message.payload));
			};
			dbClient.client.on('notification', nativeListener);
			cb(null, nativeListener);
		});
	};
	const _unlisten = (dbClient, sql, nativeListener, cb) => {
		_executeSQL(dbClient, sql, (err, _) => {
			dbClient.client.removeListener('notification', nativeListener);
			cb();
		});
	};
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const connect = cmd.cmdCall7_2(_connect);
	const disconnect = cmd.cmdCall3_0(_disconnect, cmd.unwrap({1:'_0', 3:'_0'}));
	const query = cmd.cmdCall4_2(_query, cmd.unwrap({1:'_0', 4:'_0'}));
	const moreQueryResults = cmd.cmdCall3_2(_moreQueryResults, cmd.unwrap({1:'_0'}));
	const executeSQL = cmd.cmdCall2_1(_executeSQL, cmd.unwrap({1:'_0'}));
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Subs
	const listen = cmd.cmdCall3_1(_listen, cmd.unwrap({1:'_0'}));
	const unlisten = cmd.cmdCall3_0(_unlisten, cmd.unwrap({1:'_0'}));

	return {
		///////////////////////////////////////////
		// Cmds
		connect: F8(connect),
		disconnect: F4(disconnect),
		query: F5(query),
		moreQueryResults: F4(moreQueryResults),
		executeSQL: F3(executeSQL),
		///////////////////////////////////////////
		// Subs
		listen: F4(listen),
		unlisten: F4(unlisten)
	};

}();
