

build: $(wildcard src/**.rs)
	wasm-pack build --target=web --release

test:
	cargo test

.PHONY: build clean test

clean:
	rm -rf pkg
