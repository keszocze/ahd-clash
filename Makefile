build:
	stack build

clean:
	stack clean

all: build test

testLecture:
	stack test lab-clash:test:test-library --ta '-p Lecture'

test:
	stack test lab-clash:test:test-library

test1:
	stack test lab-clash:test:test-library --ta '-p Lab1'

test2:
	stack test lab-clash:test:test-library --ta '-p Lab2'

test3:
	stack test lab-clash:test:test-library --ta '-p Lab3'

test4:
	stack test lab-clash:test:test-library --ta '-p Lab4'

test4O:
	stack test lab-clash:test:optional-tests --ta '-p Lab4'

test5:
	stack test lab-clash:test:test-library --ta '-p Lab5'

test5O:
	stack test lab-clash:test:optional-tests --ta '-p Lab5'
