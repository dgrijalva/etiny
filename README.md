### Example implementation of tinyurl using erlang and cassandra.

**This is not meant for production use**.  It's just an example.  I've left out lots of important stuff that's not required for the example.  The purpose of this application is to demonstrate what's required to talk to [cassandra][c] from [Erlang][e] via its [Thrift][t] [interface][te_doc].

The [API Document][c_api] contains all of the data types and methods.  The data types are generated as record definitions via thrift code generation, see `cassandra/cassandra_types.hrl`.

Things that it does:

* Connects to cassandra
* Reads records `get`
* Writes records `batch_mutate`

Things that it doesn't do:

* Connection problem handling
* Connection pooling
* Handle race conditions around token generation

Implementation notes:

* Use `framed` mode for Cassandra 0.7 or greater
* Do not use `framed` mode for Cassandra 0.6 or below
* The `map` type in the Cassandra IDL expects an Erlang `dict` type
* See `cassandra_types.hrl` for all of the types defined to match the IDL
* See `etiny:url_for_token/2` for an example of loading data
* See `etiny:token_for_stored_url/3` for an example of storing data

Resources:

* [Cassandra][c]
* [Thrift][t]
* [Cassandra API Documentation][c_api]

[c]: http://cassandra.apache.org/
[c_api]: http://wiki.apache.org/cassandra/API
[t]: http://incubator.apache.org/thrift/
[e]: http://www.erlang.org/
[te_doc]: http://wiki.apache.org/thrift/ThriftUsageErlang
[etiny]: http://www.github.com/dgrijalva/etiny