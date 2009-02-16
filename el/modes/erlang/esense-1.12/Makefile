release := $(shell echo esense-`sed -n 's/^.*esense-version "\(.*\)"/\1/p' esense.el`)

all:
	erlc esense.erl esense_edoc_layout.erl

dist: ChangeLog  	     \
      Makefile   	     \
      README     	     \
      TODO       	     \
      esense.bat 	     \
      esense.el              \
      esense-trace.el        \
      esense-start.el        \
      esense-distel.el       \
      esense.erl             \
      esense.sh              \
      esense_edoc_layout.erl
	rm -rf $(release)
	mkdir $(release)
	cp $^ $(release)
	tar cvfz $(release).tgz $(release)
	rm -rf $(release)


-include Makefile.internal
