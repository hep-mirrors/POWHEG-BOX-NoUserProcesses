ifdef CDEBUG
DEBUG= -g -O0
endif

LIBDIR        = ../$(OBJ)
LIBRARY	      = $(LIBDIR)/libnlojet.a
CODE          =  $(patsubst %.cc,%.o,$(wildcard *.cc))

INCLUDE1=$(PWD)/bits
INCLUDE2=$(PWD)/include

%.o: %.cc | 
	$(CC) $(DEBUG)  -c  -o  $@ $^ $(CXXFLAGS) -I. -I$(INCLUDE1) -I$(INCLUDE2)


all: $(LIBRARY)

$(LIBRARY): $(CODE)
	 ar cru libnlojet.a $(CODE); ranlib libnlojet.a; mv libnlojet.a $(LIBDIR)

