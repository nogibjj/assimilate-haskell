install:
	#install things I need like formatting tools
	#cabal install hlint stylish-haskell

run:
	cabal run

update:
	#updates the package index
	cabal update

test:
	#test

format:	
	#format cabal file
	cabal format
	#format source code
	stylish-haskell -i app/*.hs

lint:
	#lint

refactor: format lint

deploy:
	#deploy goes here
		
all: install lint test format deploy
