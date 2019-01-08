REBAR3=rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR3) xref

.PHONY: clean compile
