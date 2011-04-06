#
# Makefile --
#
#   Makefile for building delback
#
# Copyright (c) 2011 Daniel Carlsson
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

.SUFFIXES: .erl .beam .yrl


ESRC=.
EBIN=../ebin
ERLC=erlc
ERL=erl -boot start_clean

# ----------
# MODULES
# ----------

MODULES= \
	delback \
	delback_store

TARGET_FILES=$(MODULES:%=$(EBIN)/%.beam)

ERL_COMPILE_FLAGS += -W -DTEST

# ----------
# TARGETS
# ----------

all: compile test

compile: $(TARGET_FILES)

delback: compile
	$(ERL) -pa $(EBIN) -s delback start

test: 	
	$(ERL) -noshell -pa $(EBIN) \
	 -eval 'eunit:test("../ebin", [verbose])' \
	 -s init stop

clean:
	rm -f $(TARGET_FILES)
	rm -f core


$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<


