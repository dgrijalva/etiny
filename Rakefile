require 'rake/clean'

desc "Build the project"
task :build do
  out = nil
  `mkdir -p ebin`
  Dir["elib/**/*.erl"].each do |f| 
    out = `erlc -o ebin/ #{f}`
    if $? != 0
      puts "BUILD FAILED"
      puts out
      exit
    end
  end
  Dir["elib/**/*.app"].each{|f| `cp #{f} ebin/`}
  Dir["include/**/*.*"].each{|f| `cp #{f} ebin/`}
end

desc "Start the server"
task :start => [:build] do
  exec("erl -config config/server.config -pa ebin -I include -boot start_sasl -s etiny_app")
end

CLEAN << 'ebin'