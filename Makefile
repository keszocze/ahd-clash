
build:
	stack build

clean:
	stack clean

all: build test

test:
	stack test

test1:
	stack test --ta '-p Lab1'
