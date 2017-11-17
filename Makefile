all:
	stack build --copy-bins --local-bin-path "$(shell pwd)/bin"
