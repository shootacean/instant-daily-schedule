live:
	#cd docs && elm-live ../src/Main.elm --start-page=index.html -- --output=elm.js
	elm-live src/Main.elm --start-page=index.html -- --output=elm.js

build:
	elm make src/Main.elm --output=docs/elm.js
