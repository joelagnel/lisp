#!/usr/bin/env ruby1.8

# From a posting by Rubikitch at the eev mailing list:
# <http://lists.gnu.org/archive/html/eev/2005-06/msg00002.html>
# <http://article.gmane.org/gmane.emacs.eev.devel/11>

# Note that Rubikitch hasn't signed the FSF papers yet -- this file
# cannot be included in Emacs until he signs the transfer of
# copyright, but it isn't part of the "kernel" of eev that is ready
# for inclusion...

# I created a command index of ratpoison and screen.
# (eev "infoindex.rb -o /tmp/ratpoison.e ratpoison.info")
# (eev "infoindex.rb -o /tmp/screen.e screen.info*")
# (find-fline "/tmp/ratpoison.e")
# (find-fline "/tmp/screen.e")

# The infoindex.rb script is here.
# (find-fline "/usr/share/info/" "screen")
# (find-angg ".zshrc" "zcatinfo")
# cd /usr/share/info/; zcatinfo screen > /tmp/screen.info
# ruby1.8 $EEVE/infoindex.rb -o /tmp/screen.e /tmp/screen.info
# (find-fline "/tmp/screen.e")

require 'optparse'
require 'tempfile'

class << IO
  def redirect(stdout)
    begin
      stdout_sv = STDOUT.dup
      STDOUT.reopen(stdout)
      yield
    ensure
      STDOUT.flush
      STDOUT.reopen(stdout_sv)
    end
  end
end

def system_to_string(*args)
  ret = nil
  Tempfile.open("s2s") do |f|
    IO.redirect(f) {
      system *args
    }
    ret = f.open.read
  end
  ret
end

module EmacsLisp
  def elisp(lisp)
    system_to_string("gnudoit", lisp).chomp
  end

  # imported from el4r
  def dump_string(string)
    dumped = string.dup
    # \ -> \\
    dumped.gsub! %r"\\" do '\\\\' end
    # " -> \"
    dumped.gsub! %r'"' do '\\"' end
    # (zero byte) -> \0
    dumped.gsub! %r'\0' do "\\\0" end
    %Q'"#{dumped}"'
  end

end

class ParseInfo
  def initialize(io, regexp)
    @io = io
    @regexp = regexp
    @results = []
  end

  class Result < Struct.new(:info, :node, :line)
    include EmacsLisp
    def to_s
      arg = dump_string("(#{info})#{node}")
      line = dump_string(self.line)
      %Q[# (find-node #{arg} #{line})]
    end
  end

  def parse
    @io.read.split(/\n/).each do |line|
      case line
      when /^File: (.+)\.info,  Node: (.+?),/
        @info = $1
        @node = $2
      when @regexp
        @results << Result.new(@info, @node, line)
      else
      end
    end
        
  end

  def output(output=$>)
    @results.each do |res|
      output.puts res
    end
  end
end
  

if __FILE__==$0
  outfile = nil
  ARGV.options {|o|
    o.on("-o OUTFILE") {|v| outfile = v}
    o.parse!
  }

  obj = ParseInfo.new(ARGF, /^ - Command: (.+)$/)
  obj.parse

  if outfile
    File.chmod 0644, outfile if File.exist? outfile
    open(outfile, "w"){|io|
      obj.output io
      io.chmod 0444
    }
  else
    obj.output $>
  end
end
