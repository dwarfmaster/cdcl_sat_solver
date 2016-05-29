FILES=hipp_to_sat.hs \
	  hipp_solver.hs \
	  hipp.hs        \
	  hipp_main.hs

MAINP=main.prog
TESTP=test.prog
BENP=bench.prog
HIPPP=hipp.prog

all: hipp

hipp : $(HIPPP)

$(HIPPP): $(FILES)
	ghc $^ -o $(HIPPP)

.PHONY:all hipp

