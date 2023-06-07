pub mod encoder;
pub mod parser;

use encoder::encoder::{encode_file, EncodeResult};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

pub fn assemble_raw(
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

#[wasm_bindgen]
pub fn assemble(
    input_file_content: String,
    instr_start_address: u64,
    data_start_address: u64,
    entrypoint: String,
) -> Result<JsValue, String> {
    let res = assemble_raw(
        input_file_content,
        instr_start_address,
        data_start_address,
        entrypoint,
    )?;

    return serde_wasm_bindgen::to_value(&res).map_err(|e| format!("{}", e).to_string());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assemble() {
        let input_file_content = r#"
        .section .text
        .globl _start
        _start:
            mov rax, [rax]
        "#;

        assert!(matches!(
            assemble_raw(
                input_file_content.to_string(),
                0x1000,
                0x2000,
                "_start".to_string(),
            ),
            Ok(_)
        ));
    }
}
