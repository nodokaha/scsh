INCLUDES = -I@top_srcdir@/c/xlib -I@top_srcdir@/c/libs -I@x_includes@ \
	-I@top_srcdir@/c -I@scsh_includes@

noinst_LTLIBRARIES = libscxxft.la

if WITH_XFT
xft_sources = xft.c xft.h xrender.c xrender.h
xft_libadd = -lXft
endif

libscxxft_la_SOURCES = \
	$(xft_sources)

libscxxft_la_LIBADD = $(X_LIBS) $(xft_libadd) 
libscxxft_la_LDFLAGS= -avoid-version -module
