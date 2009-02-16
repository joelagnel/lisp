

# «.one-liners»		(to "one-liners")
# «.noncanonical-mode»	(to "noncanonical-mode")




(find-es "anatocc")
(find-eevrcfile ".zshrc" "EETMPC")
(find-eevrcfile ".zshrc" "EEAOUT")



# «one-liners»  (to ".one-liners")
#
eegcc <<<' int main(){ printf("%d = 0x%x\n", 4095, 4095); return 0; } '
eec
#
eegcc <<<' int main(){ printf("%d\n", isatty(0)); return 0; } '
eec              ;# answer: 1 (stdin is a tty)
echo | eec       ;# answer: 0 (stdin is a pipe, not a tty)
#



# (find-node "(libc)Program Arguments" "int main")
#
eegcc -g <<<'
int main (int argc, char *argv[]) {
  int i;
  for (i=0; i<argc; ++i)
    printf("  argv[%d]=\"%s\"\n", i, argv[i]);
  return 0;
}
'
eec foo
eec "foo bar" baz

#
# (eeb-gdb-start nil (ee-expand "$EEAOUT"))
br main
run

#





#####
#
# terminals: receiving characters as they are typed
# 2004oct08
#
#####

# «noncanonical-mode»  (to ".noncanonical-mode")
# (find-node "(libc)Canonical or Not")
# (find-node "(libc)Noncanonical Input")
# (find-node "(libc)Local Modes")
# (find-node "(coreutils)stty invocation")
# (find-node "(coreutils)Input")
# (find-node "(coreutils)Local")
# (find-htetfile "BackspaceDelete.gz" "one-liner")
#
eegcc <<<'
  void main() { int c; while(c = getchar()) printf("%d 0x%02X\n", c, c); }
'
stty -icanon min 1
eec              ;# now type some chars. Leave with C-c
stty sane

#





#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
