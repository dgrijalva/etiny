require 'rake/clean'

desc "Build the project"
task :build do
  `mkdir -p ebin`
  Dir["elib/**/*.erl"].each{|f| `erlc -o ebin/ #{f}`}
  Dir["elib/**/*.app"].each{|f| `cp #{f} ebin/`}
end

desc "Start the server"
task :start => [:build] do
  exec("erl -config server.config -pa ebin -boot start_sasl -run etiny_app boot")
end

CLEAN << 'ebin'