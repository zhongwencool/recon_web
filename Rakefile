
###############
## see rake rule on: http://jasonseifer.com/2010/04/06/rake-tutorial 
###############

desc "display help infomations"
task :default do
  sh "rake -T"
end

desc "start a demon erlang node by heart"
task :start do
  #
  # Include `pwd` in the path to ebin so code:priv_dir works. See:
  #
  # http://erlang.org/pipermail/erlang-questions/2011-October/062024.html
  #
  sh "exec erl -heart -env HEART_COMMAND 'rake restart' -sname zhongwencool -setcookie zhongwencool -pa `pwd`/ebin deps/*/ebin -detached -noinput -noshell -s recon_web"
end

desc "stop erlang node normal"
task :stop do
  sh "erl -noshell -sname temp_control -setcookie zhongwencool -eval \"rpc:call(zhongwencool@localhost, init, stop, [])\" -s init stop"
end

desc "start debug module lager:info"
task :debug do
  sh "rebar co&& exec erl -sname zhongwencool -setcookie zhongwencool -pa `pwd`/ebin deps/*/ebin -boot start_sasl -s recon_web start debug"
end

desc "remsh mode shell"
task :remsh do
  sh "erl -setcookie zhongwencool -remsh zhongwencool@localhost -sname remsh_tmp"
end

desc "restart node"
task :restart =>[:stop,:start] do 
puts "restart ok"
end

desc "rebar eunit skip_deps=true"
task :test do
  sh "rebar eunit skip_deps=true"
end

desc "rebar co"
task :co do
  sh "rebar co"
end

desc "erl -pa ../recon_web/ebin deps/*/ebin"
task :shell do
  sh "erl -pa ../recon_web/ebin deps/*/ebin ./"
end
   
