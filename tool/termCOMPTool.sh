#!/bin/bash

# ./TRSAnalyzer $* | z3 -in | sed -e 's/^sat/YES/' -e 's/^unsat/MAYBE/'
stack run $* | z3 -in | sed -e 's/^sat/YES/' -e 's/^unsat/MAYBE/'