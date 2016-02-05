OBJDIR=obj
OBJS=$(OBJDIR)/main.o $(OBJDIR)/cnf.o $(OBJDIR)/list.o
HEADERS=bool.h cnf.h list.h assert.h
CFLAGS=-Wall -Wextra
LDFLAGS=
PROG=cdcl.prog

all : $(OBJDIR) $(PROG)

$(PROG) : $(OBJS)
	$(CC) $(CFLAGS)    -o $@ $^ $(LDFLAGS)

$(OBJDIR)/%.o : %.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJDIR) :
	mkdir $(OBJDIR)

clean :
	rm -rf $(OBJDIR)

rec : clean all

.PHONY: all clean rec


