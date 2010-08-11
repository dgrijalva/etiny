### Example implementation of tinyurl using erlang and cassandra.

**This is not meant for production use**.  It's just an example.  I've left out lots of important stuff that's not required for the example.  The purpose of this application is to demonstrate what's required to talk to cassandra from erlang.

The [API Document][1] contains all of the data types and methods.  The data types are generated as record definitions via thrift code generation, see `cassandra/cassandra_types.hrl`.

Things that it does:

* Connects to cassandra
* Reads records `get`
* Writes records `batch_mutate`

Things that it doesn't do:

* Connection problem handling
* Connection pooling
* Handle race conditions around token generation

Resources:

* [Cassandra](http://cassandra.apache.org/)
* [Thrift](http://incubator.apache.org/thrift/)
* [Cassandra API Documentation][1]

[1]: http://wiki.apache.org/cassandra/API