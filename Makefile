build:
	stack build

clean:
	stack clean

all: build test

test:
	stack test

test1:
	stack test lab-clash:test:test-library --ta '-p Lab1'

test2:
	stack test lab-clash:test:test-library --ta '-p Lab2'

test3:
	stack test lab-clash:test:test-library --ta '-p Lab3'
