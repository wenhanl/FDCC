ERLC=/usr/bin/erlc
ERLCFLAGS=-o
SRCDIR=src
BEAMDIR=./ebin

all: 
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
	@ cp $(SRCDIR)/*erl $(BEAMDIR) ;

clean: 
	@ rm -rf $(BEAMDIR)/*.beam $(BEAMDIR)/*.erl ;
	@ rm -rf erl_crush.dump
