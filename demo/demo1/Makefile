# PREDEFINED VARIABLES
CFLAGS = -arch sm_20
CC =/usr/local/cuda/bin/nvcc

#GLOBAL VARIABLES

PROGRAM = main
EXEC = exec

$(EXEC).out:
	$(CC) $(CFLAGS) $(PROGRAM).cu -o $(EXEC).out

run:
	./$(EXEC).out 
clean:
	rm -rf *~ *.out *.o *# *.log

all: $(EXEC).out run


