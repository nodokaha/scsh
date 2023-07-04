INCLUDES = -I@top_srcdir@/c/xlib -I@top_srcdir@/c/libs -I@x_includes@ \
	-I@top_srcdir@/c -I@scsh_includes@

noinst_LTLIBRARIES = libscxx11.la

libscxx11_la_SOURCES = \
        client.c colormap.c cursor.c display.c error.c event-types.c \
	event.c font.c gcontext.c grab.c graphics.c init.c key.c \
	pixmap.c property.c region.c text.c types.c util.c visual.c window.c \
	wm.c xlib.h

libscxx11_la_LIBADD = $(X_PRE_LIBS) $(X_LIBS) $(X_EXTRA_LIBS) -lX11
libscxx11_la_LDFLAGS= -avoid-version -module
