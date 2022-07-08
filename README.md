# assembly-script
This is a very basic `x86_64` (very small subset) Assembler based on the [`iced-x86`](https://crates.io/crates/iced-x86) Rust crate. I built it because I couldn't find a version of the GNU Assembler for `x86_64` that worked on the web.

I use it in the [MemeAssembly playground](https://github.com/xarantolus/memeassembly-playground) to translate `x86_64` Assembly code into its binary representation. This binary representation is then run in the browser.

This project will probably not be useful to you if you don't have very similar goals, e.g. like assembling a minimal `x86_64` subset on the web.

### Build instructions
First make sure you have the Rust toolchain installed.

1. [Install `wasm-pack`](https://rustwasm.github.io/wasm-pack/installer/)

2. Clone this repo

       git clone https://github.com/xarantolus/assembly-script.git

3. Go to its directory:

       cd assembly-script

4. Run `make` to generate the WebAssembly binary

       make

5. You can now run a local web server and open [`user.html`](user.html) to see a demo, or see the generated `pkg` directory for info on how to use this in your own projects (or [`user.html`](user.html) as an example)
