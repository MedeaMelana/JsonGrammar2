run:
	ghci -isrc:tests -Wall Example

clean:
	cabal clean

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/JsonGrammar/index.html
