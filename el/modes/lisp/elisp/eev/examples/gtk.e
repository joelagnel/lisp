


# «.getting-started»	(to "getting-started")

(code-c-d "gtktut" "/usr/share/doc/libgtk2.0-doc/gtk-tutorial/")
(code-c-d "gtkref" "/usr/share/doc/libgtk2.0-doc/gtk/")

(defun find-html-file (dir pagestem tag)
  (find-sh0 (format "firefox %s%s%s" dir
		    (if pagestem (concat pagestem ".html") "")
		    (if tag (concat "#" tag) ""))))

(defun find-gtkref (&optional pagestem tag) (interactive)
  (find-html-file "/usr/share/doc/libgtk2.0-doc/gtk/" pagestem tag))

(defun find-gtktut (&optional pagestem tag) (interactive)
  (find-html-file "/usr/share/doc/libgtk2.0-doc/gtk-tutorial/" pagestem tag))





# (find-status   "libgtk2.0-dev")
# (find-vldifile "libgtk2.0-dev.list")
# (find-udfile   "libgtk2.0-dev/")
# (find-status   "libgtk2.0-doc")
# (find-vldifile "libgtk2.0-doc.list")
# (find-udfile   "libgtk2.0-doc/")
# (eev "firefox /usr/share/doc/libgtk2.0-doc/gtk-tutorial/ &")
# (eev "firefox /usr/share/doc/libgtk2.0-doc/gtk/ &")





#####
#
# Getting started
# 2004nov18
#
#####

# «getting-started»  (to ".getting-started")
# Getting started: (find-gtktut "c58")
#
eegcc $(pkg-config --cflags --libs gtk+-2.0) <<'%%%'
#include <gtk/gtk.h>
int main( int argc, char *argv[] ) {
    GtkWidget *window;
    gtk_init (&argc, &argv);
    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_show (window);
    gtk_main ();
    return 0;
}
%%%
eec

#



#####
#
# first widgets (adapted from packbox.c)
# 2004nov18
#
#####

#
# The original packbox.c: (find-gtktut "x405")
# Index:                  (find-gtkref "ix01")
# (find-fline "/tmp/pack.c")

eegcc $(pkg-config --cflags --libs gtk+-2.0) -Wall <<'%%%'
#include <stdio.h>
#include <stdlib.h>
#include "gtk/gtk.h"

int main(int argc, char *argv[]) {
  GtkWidget *window, *box1, *box2, *quitbox;
  GtkWidget *button, *label, *separator;
  
  gtk_init (&argc, &argv);
  
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  box1 = gtk_vbox_new (FALSE, 0);
  box2 = gtk_hbox_new (FALSE, 0);
  quitbox = gtk_hbox_new (FALSE, 0);
  
  g_signal_connect (G_OBJECT (window), "delete_event",
		    G_CALLBACK (gtk_main_quit), NULL);
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
  
  button = gtk_button_new_with_label ("buttton");
  gtk_box_pack_start (GTK_BOX (box2), button, FALSE, FALSE,0);
  gtk_widget_show (button);
  
  label = gtk_label_new ("end");
  gtk_box_pack_end (GTK_BOX (box2), label, FALSE, FALSE, 0);
  gtk_widget_show (label);
  
  gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
  gtk_widget_show (box2);
  
  separator = gtk_hseparator_new ();
  gtk_widget_set_size_request (separator, 400, 5);
  gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 5);
  gtk_widget_show (separator);    
  
  button = gtk_button_new_with_label ("Quit");
  g_signal_connect_swapped (G_OBJECT (button), "clicked",
			    G_CALLBACK (gtk_main_quit),
			    G_OBJECT (window));
  gtk_box_pack_start (GTK_BOX (quitbox), button, TRUE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (box1), quitbox, FALSE, FALSE, 0);
  
  gtk_container_add (GTK_CONTAINER (window), box1);
  
  gtk_widget_show (button);
  gtk_widget_show (quitbox);
  gtk_widget_show (box1);
  gtk_widget_show (window);
  
  gtk_main ();
  
  return 0;
}
%%%

eec

#






# Scribble:    (find-gtktut "c2442")
# DrawingArea: (find-gtktut "x2490")
#     (find-gtkref "GtkDrawingArea")








#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
