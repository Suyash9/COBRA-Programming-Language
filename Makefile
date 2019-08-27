# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += COBRATokens.hs COBRAGrammar.hs COBRAEvaluator.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) myinterpreter

# Build an executable for COBRA interpreter
myinterpreter: $(DEPEND) myinterpreter.hs
				ghc myinterpreter.hs

# Generate ML files from a parser definition file
COBRAGrammar.hs : COBRAGrammar.y
	@rm -f COBRAGrammar.hs
	happy COBRAGrammar.y
	@chmod -w COBRAGrammar.hs

# Generate ML files from a lexer definition file
COBRATokens.hs : COBRATokens.x
	@rm -f COBRATokens.hs
	alex COBRATokens.x
	@chmod -w COBRATokens.hs

# Clean up the directory
clean::
	rm -rf COBRATokens.hs COBRAGrammar.hs *.hi *.o *.info


