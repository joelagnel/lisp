#!/usr/bin/ruby -s
# -*- Ruby -*-

def usage
  name = File::basename $0
  print <<EOU
#{name}: howm メモを, 一メモ一ファイルに分割 (手抜き仕様)
(例)
  #{name} 2004_10_10.howm
  → 2004_10_10.howm.aa, 2004_10_10.howm.ab, … ができる
(オプション例)
  -prefix=hoge.     → hoge.aa, hoge.ab, … ができる
  -help または -h   → このメッセージを表示
EOU
end

#####################################

if ($help || $h || ARGV.length == 0)
  usage
  exit 0
end

$prefix ||= ARGV[0] + '.'
ext = 'aa'

ARGF.readlines.join.split(/^= /).each_with_index{|x, i|
  next if x.empty?
  x = '= ' + x if i > 0
  open($prefix + ext, 'w'){|io| io.print x}
  ext.succ!
}
