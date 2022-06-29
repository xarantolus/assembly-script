

build: $(wildcard src/**.rs)
	wasm-pack build --target=web --release

.PHONY: build clean

clean:
	rm -rf pkg
