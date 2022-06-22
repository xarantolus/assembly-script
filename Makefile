

build: $(wildcard src/**.rs)
	wasm-pack build --target=web

.PHONY: build
