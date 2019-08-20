lib_LTLIBRARIES = libare.la
libare_la_CFLAGS = -I$(top_builddir)/include -DGENDB=$(abs_top_srcdir)
libare_la_SOURCES = are.c log.c module.c types.c utils.c

if PCRE2
libare_la_SOURCES += are_pcre2.c
endif
