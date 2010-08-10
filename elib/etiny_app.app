{application, etiny_app,
 [
  % A quick description of the application.
  {description, "tinyurl server in erlang"},

  % The version of the applicaton
  {vsn, "0.1"},

  % All modules used by the application.
  {modules, [
		etiny_app
   ]},

  % All of the registered names the application uses. This can be ignored.
  {registered, []},

  % Applications that are to be started prior to this one. This can be ignored
  % leave it alone unless you understand it well and let the .rel files in
  % your release handle this.
  {applications,
   [
    kernel,
    stdlib,
	sasl
   ]},

  % OTP application loader will load, but not start, included apps. Again
  % this can be ignored as well.  To load but not start an application it
  % is easier to include it in the .rel file followed by the atom 'none'
  {included_applications, []},

  % configuration parameters similar to those in the config file specified
  % on the command line. can be fetched with gas:get_env
  {env, [
	{cassandra, [{host, "127.0.0.1"}, {port, 8080}]}
  ]},

  % The Module and Args used to start this application.
  {mod, {etiny_app, []}}
 ]
}.
