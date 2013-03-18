ERL_SRC = $(wildcard *.erl)
BEAM	= $(ERL_SRC:.erl=.beam)

all: $(BEAM) test

test:
	./run.sh reservation_multiple_actors test

test_single:
	./run.sh reservation_single_actor test

clean:
	rm *.beam

%.beam: %.erl
	erlc +debug_info $*.erl
