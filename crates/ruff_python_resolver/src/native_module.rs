use std::path::Path;

fn is_native_module_file_extension(file_name: &Path) -> bool {
    file_name
        .extension()
        .map_or(false, |ext| ext == "so" || ext == "pyd" || ext == "dylib")
}

pub(crate) fn is_native_module_file_name(_module_name: &Path, _file_name: &Path) -> bool {
    false
}
