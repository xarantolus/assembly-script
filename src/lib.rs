pub mod encoder;
pub mod parser;

use encoder::encoder::encode_file;
use wasm_bindgen::prelude::*;

use crate::encoder::encoder::EncodeResult;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn assemble(
    input_file_content: String,
    instr_start_address: u64,
    data_start_address: u64,
    entrypoint: String,
) -> Result<EncodeResult, String> {
    let parsed_file =
        parser::input::parse_gnu_as_input(input_file_content).map_err(|e| format!("{}", e))?;

    let result = encode_file(
        parsed_file,
        instr_start_address,
        data_start_address,
        Some(entrypoint),
    )
    .map_err(|e| e.to_string())?;

    return Ok(result);
}
