FILES=data.hs    \
	  status.hs  \
	  chooser.hs \
	  loader.hs  \
	  solver.hs
MAIN=main.hs
BENCH=problems.hs  \
	  benchmark.hs
TESTS=problems.hs \
	  test.hs
HIPP=hipp_to_sat.hs \
	 hipp_solver.hs \
	 hipp.hs        \
	 hipp_main.hs

MAINP=main.prog
TESTP=test.prog
BENP=bench.prog
HIPPP=hipp.prog

all: main bench tests hipp

main: $(MAINP)
	
$(MAINP): $(FILES) $(MAIN)
	ghc $^ -o $(MAINP)

bench: $(BENP)
	
$(BENP): $(FILES) $(BENCH)
	ghc $^ -o $(BENP)

tests: $(TESTP)
	
$(TESTP): $(FILES) $(TESTS)
	ghc $^ -o $(TESTP)

hipp : $(HIPPP)

$(HIPPP): $(FILES) $(HIPP)
	ghc $^ -o $(HIPPP)

.PHONY:all main bench tests hipp

