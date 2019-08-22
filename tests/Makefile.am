TESTS = are-test.el
TEST_EXTENSIONS = .el
EL_LOG_COMPILER = ./run-emacs
AM_EL_LOG_FLAGS = $(EMACS)

if ASAN
AM_EL_LOG_FLAGS += "yes"
else
AM_EL_LOG_FLAGS += "no"
endif

if UBSAN
AM_EL_LOG_FLAGS += "yes"
else
AM_EL_LOG_FLAGS += "no"
endif
