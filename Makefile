# module makefile 
# Christian Leth Petersen 02/2009 revised 03/2010

include ../../PLATFORM.mk
include ./TARGET
SRCS=constants.scm  datatypes.scm lookup.scm storage.scm parser.scm ivueparser.scm
include ../s-makefile.stub

