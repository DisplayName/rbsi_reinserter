all: generate

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile: get-deps
	@./rebar compile

generate: compile
	@rm -rf ./rel/rbsi_reinserter
	@./rebar generate

console:
	@chmod +x ./rel/rbsi_reinserter/bin/rbsi_reinserter
	@./rel/rbsi_reinserter/bin/rbsi_reinserter console