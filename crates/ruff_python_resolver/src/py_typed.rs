use std::path::{Path, PathBuf};

#[derive(Debug)]
pub(crate) struct PyTypedInfo {
    py_typed_path: PathBuf,
    is_partially_typed: bool,
}

pub(crate) fn get_py_typed_info(dir_path: &Path) -> Option<PyTypedInfo> {
    let py_typed_path = dir_path.join("py.typed");
    if py_typed_path.is_file() {
        // Do a quick sanity check on the size before we attempt to read it. This
        // file should always be really small - typically zero bytes in length.
        let file_len = py_typed_path.metadata().ok()?.len();
        if file_len > 0 && file_len < 64 * 1024 {
            // PEP 561 doesn't specify the format of "py.typed" in any detail other than
            // to say that "If a stub package is partial it MUST include partial\n in a top
            // level py.typed file."
            let contents = std::fs::read_to_string(&py_typed_path).ok()?;
            let is_partially_typed =
                contents.contains("partial\n") || contents.contains("partial\r\n");
            Some(PyTypedInfo {
                py_typed_path,
                is_partially_typed,
            })
        } else {
            None
        }
    } else {
        None
    }
}
