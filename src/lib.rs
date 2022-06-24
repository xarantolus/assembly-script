pub mod parser;

use wasm_bindgen::prelude::*;


// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);

    fn write_stdout(s: &str);
    fn write_stderr(s: &str);
}

#[wasm_bindgen]
pub fn interpret_assembly_file(
    _run_id: i32,
    input_file_content: String,
    entrypoint_name: String,
) -> Result<i8, String> {
    alert(input_file_content.as_str());

    let parsed_file =
        parser::input::parse_gnu_as_input(input_file_content).map_err(|e| format!("{}", e))?;

    return Ok(42);
}
