Pry.config.correct_indent = false if ENV["EMACS"]
Pry.commands.alias_command 'c', 'continue'
Pry.commands.alias_command 's', 'step'
Pry.commands.alias_command 'n', 'next'
#Pry.config.exception_handler = proc do |output, exception, _pry_|
#  output.puts "#{exception}"
#  output.puts "#{exception.backtrace.first(10)}"
#end