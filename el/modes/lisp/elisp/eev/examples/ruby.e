# Notes about Ruby
# Authors:  Eduardo Ochs <edrx@mat.puc-rio.br>
#           Rubikitch <rubikitch@ruby-lang.org>
# Version: 2005jun20 6:37

# Note: I think that Rubikitch hasn't signed his FSF papers yet - it
# might be dangerous to include this file in GNU Emacs until we
# resolve this.

# (find-es "ruby")


# «.infoindex.rb»	(to "infoindex.rb")
# «.ratpoison.e»	(to "ratpoison.e")
# «.screen.e»		(to "screen.e")
# «.eeruby»		(to "eeruby")





#####
#
# debian packages for ruby (and a few `code-c-d's)
# 2005jun20
#
#####

apt-get install irb ruby ruby-elisp ruby-examples ruby-manual
apt-get install rubybook

# (find-fline "/usr/doc/irb/")
# (find-fline "/usr/doc/ruby-elisp/")
# (find-fline "/usr/doc/ruby-examples/")
# (find-fline "/usr/doc/ruby-examples/examples/")
# (find-fline "/usr/doc/ruby-manual/")
# (find-fline "/usr/doc/ruby/")
# (find-fline "/usr/doc/rubybook/")
# (find-status "irb")
# (find-status "ruby")
# (find-status "ruby-elisp")
# (find-status "ruby-examples")
# (find-status "ruby-manual")
# (find-status "rubybook")
# (find-vldifile "irb.list")
# (find-vldifile "ruby-elisp.list")
# (find-vldifile "ruby-examples.list")
# (find-vldifile "ruby-manual.list")
# (find-vldifile "ruby.list")
# (find-vldifile "rubybook.list")

# (find-man "1 irb")
# (find-man "1 ruby")

(code-c-d "rubybook"   "/usr/share/doc/rubybook/html/")
(code-c-d "rubymanual" "/usr/share/doc/ruby-manual/html/")
(code-c-d "rubyfaq"    "/usr/share/doc/ruby-manual/faq/")

;; (find-rubymanualfile "")
;; (find-rubymanualw3m "syntax.html" "\nEmbedded Documentation\n")
;; (find-rubymanualw3m "index.html")
;; (find-rubybookfile "")
;; (find-rubyfaqfile "")





#####
#
# hyperlinks to the command descriptions in ratpoison.info and screen.info
# (by rubikitch, with minimal changes by edrx)
# 2005jun20
#
#####

# http://lists.gnu.org/archive/html/eev/2005-06/msg00002.html
#
# «infoindex.rb»  (to ".infoindex.rb")
cat > /tmp/infoindex.rb <<'%%%'
#!/usr/bin/env ruby
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
%%%

chmod 755 /tmp/infoindex.rb

#
# «ratpoison.e»  (to ".ratpoison.e")
# «screen.e»  (to ".screen.e")
# (find-fline "/usr/share/info/")

cd /usr/share/info/
for i in ratpoison.info.gz screen*.gz; do
  cp $i /tmp/; gunzip -fv /tmp/$i
done

cd /tmp/
ruby1.8 infoindex.rb -o ratpoison.e ratpoison.info
ruby1.8 infoindex.rb -o screen.e screen.info

#
# (find-fline "/tmp/ratpoison.e")
# (find-fline "/tmp/screen.e")



"

#####
#
# eeruby (by rubikitch, with small changes by edrx)
# 2005jun20
#
#####

# http://lists.gnu.org/archive/html/eev/2005-06/msg00001.html

;; «eeruby»  (to ".eeruby")
;; (eeruby "puts ARGV[0]")

(setq eeruby-file (ee-expand "$EEVTMPDIR/ee.rb"))
;; (setq eeruby-eevscript (format "ruby %s $*" eeruby-file))
(setq eeruby-eevscript (format "ruby1.8 %s $*" eeruby-file))
(defun eeruby (s &optional e noeev)
  (interactive "rP")
  (ee-write s e "" "" eeruby-file)
  (or noeev (eev eeruby-eevscript nil))
  (format "eeruby: wrote %s %s" eeruby-file
	  (if noeev "" (format "and %s" ee-file))))

(eeb-define 'eeruby-bounded 'eeruby 'ee-delimiter-hash nil t t)

(defun find-ruby (rubycode &rest pos-spec-list)
  (interactive "sRuby code: ")
  (eeruby rubycode)
  (apply 'find-sh eeruby-eevscript pos-spec-list))

(defun find-ruby0 (rubycode &rest pos-spec-list)
  (interactive "sRuby code: ")
  (eeruby rubycode)
  (find-sh0 eeruby-eevscript))

;; (find-ruby "puts 1+2")
;; (find-ruby0 "puts 77")






#  Local Variables:
#  coding:               raw-text
#  modes:                (fundamental-mode sh-mode emacs-lisp-mode)
#  ee-delimiter-hash:    "\n#\n"
#  ee-delimiter-percent: "\n%\n"
#  ee-anchor-format:     "«%s»"
#  End:
