# Unix makefile for tigermain example

#HOME=~

#MOSMLHOME=/usr
MOSMLHOME=/usr/local
#MOSMLHOME=/usr/local/bin
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=${MOSMLHOME}/bin/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/bin/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo tigertemp.uo topsort.uo tigertree.uo \
	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigerutils.uo tigerinterp.uo tigercodegen.uo  \
	tigerassem.uo tigergraph.uo tigerflow.uo tigerliveness.uo tigercolor.uo tigerregalloc.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigercolor.uo: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerassem.uo tigerflow.ui \
    tigercolor.ui tigerframe.uo tigerutils.uo
    
tigercolor.ui: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerassem.uo tigerflow.ui tigerframe.ui
tigerregalloc.uo: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerassem.uo tigerflow.ui \
    tigerregalloc.ui tigercolor.uo
tigerregalloc.ui: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerassem.uo tigerflow.ui tigerframe.ui
tigerassem.uo: tigerassem.ui tigertemp.uo tigerutils.uo
tigerpp.uo: tigerabs.uo tigersres.uo tigertrans.uo
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo tigerutils.uo
tigerescap.ui: tigerabs.uo 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerit.uo \
    tigerassem.uo tigertemp.ui 
tigerinterp.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigerinterp.uo: tigerinterp.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigerit.uo tigertemp.ui 
tigerframe.ui: tigertree.uo tigertemp.ui tigertab.ui
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui tigertab.uo tigerutils.uo
tigergrm.ui: tigerabs.uo 
tigergraph.uo: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerutils.uo
tigerflow.ui: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerassem.uo
tigerflow.uo: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerutils.uo tigerassem.uo tigerflow.ui
tigerliveness.ui: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerassem.uo tigerflow.ui
tigerliveness.uo: tigergraph.ui tigertemp.ui tigertab.ui tigertemp.ui tigerassem.uo tigerflow.ui tigerutils.uo tigerliveness.ui tigertab.uo tigerframe.uo
tigerseman.ui: tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerit.uo: tigertree.uo tigertab.ui 
tigertree.uo: tigertemp.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigerabs.uo tigertrans.ui tigerutils.uo topsort.uo tigerpp.uo
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerpila.uo: tigerpila.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigertab.uo: tigertab.ui 
tigermain.uo: tigerseman.ui tigerescap.ui tigerinterp.ui tigergrm.ui tigerlex.uo \
    tigerpp.uo tigercanon.ui tigercanon.uo tigerflow.uo tigerliveness.uo \
        tigercodegen.uo tigercolor.uo tigerframe.uo tigerregalloc.uo
tigertemp.uo: tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui 
