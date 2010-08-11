-ifndef(_cassandra_types_included).
-define(_cassandra_types_included, yeah).

-define(cassandra_ZERO, 0).
-define(cassandra_ONE, 1).
-define(cassandra_QUORUM, 2).
-define(cassandra_DCQUORUM, 3).
-define(cassandra_DCQUORUMSYNC, 4).
-define(cassandra_ALL, 5).
-define(cassandra_ANY, 6).

-record(column, {name, value, timestamp}).

-record(superColumn, {name, columns}).

-record(columnOrSuperColumn, {column, super_column}).

-record(notFoundException, {}).

-record(invalidRequestException, {why}).

-record(unavailableException, {}).

-record(timedOutException, {}).

-record(authenticationException, {why}).

-record(authorizationException, {why}).

-record(columnParent, {column_family, super_column}).

-record(columnPath, {column_family, super_column, column}).

-record(sliceRange, {start, finish, reversed, count}).

-record(slicePredicate, {column_names, slice_range}).

-record(keyRange, {start_key, end_key, start_token, end_token, count}).

-record(keySlice, {key, columns}).

-record(deletion, {timestamp, super_column, predicate}).

-record(mutation, {column_or_supercolumn, deletion}).

-record(tokenRange, {start_token, end_token, endpoints}).

-record(authenticationRequest, {credentials}).

-endif.
