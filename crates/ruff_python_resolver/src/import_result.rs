//! Interface that describes the output of the import resolver.

use std::collections::HashMap;
use std::path::{PathBuf};

use crate::py_typed::PyTypedInfo;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ImportType {
    BuiltIn,
    ThirdParty,
    Local,
}

#[derive(Debug)]
pub(crate) struct ImplicitImport {
    is_stub_file: bool,
    is_native_lib: bool,
    name: String,
    path: String,
    py_typed: Option<PyTypedInfo>,
}

#[derive(Debug)]
pub(crate) struct ImportResult {
    // The formatted import name. Useful for error messages.
    pub(crate) import_name: String,

    // Indicates whether the import name was relative (starts
    // with one or more dots).
    pub(crate) is_relative: bool,

    // True if import was resolved to a module or file.
    pub(crate) is_import_found: bool,

    // The specific submodule was not found but a part of
    // its path was resolved.
    pub(crate) is_partly_resolved: bool,

    // True if the import refers to a namespace package (a
    // folder without an __init__.py(i) file at the last level).
    // To determine if any intermediate level is a namespace
    // package, look at the resolvedPaths array. Namespace package
    // entries will have an empty string for the resolvedPath.
    pub(crate) is_namespace_package: bool,

    // True if there is an __init__.py(i) file in the final
    // directory resolved.
    pub(crate) is_init_file_present: bool,

    // Did it resolve to a stub within a stub package?
    pub(crate) is_stub_package: bool,

    // Type of import (built-in, local, third-party).
    pub(crate) import_type: ImportType,

    // The resolved absolute paths for each of the files in the module name.
    // Parts that have no files (e.g. directories within a namespace
    // package) have empty strings for a resolvedPath.
    pub(crate) resolved_paths: Vec<PathBuf>,

    // For absolute imports, the search path that was used to resolve
    // (or partially resolve) the module.
    pub(crate) search_path: Option<PathBuf>,

    // True if resolved file is a type hint (.pyi) file rather than
    // a python (.py) file.
    pub(crate) is_stub_file: bool,

    // True if resolved file is a native DLL.
    pub(crate) is_native_lib: bool,

    // True if the resolved file is a type hint (.pyi) file that comes
    // from typeshed in the stdlib or third-party stubs.
    pub(crate) is_stdlib_typeshed_file: bool,
    pub(crate) is_third_party_typeshed_file: bool,

    // True if the resolved file is a type hint (.pyi) file that comes
    // from the configured typings directory.
    pub(crate) is_local_typings_file: bool,

    // List of files within the final resolved path that are implicitly
    // imported as part of the package - used for both traditional and
    // namespace packages.
    pub(crate) implicit_imports: HashMap<String, ImplicitImport>,

    // Implicit imports that have been filtered to include only
    // those symbols that are explicitly imported in a "from x import y"
    // statement.
    pub(crate) filtered_implicit_imports: HashMap<String, ImplicitImport>,

    // If resolved from a type hint (.pyi), then store the import result
    // from .py here.
    pub(crate) non_stub_import_result: Option<Box<ImportResult>>,

    // Is there a "py.typed" file (as described in PEP 561) present in
    // the package that was used to resolve the import?
    pub(crate) py_typed_info: Option<PyTypedInfo>,

    // The directory of the package, if found.
    pub(crate) package_directory: Option<PathBuf>,
}

impl ImportResult {
    pub(crate) fn not_found(import_name: String) -> Self {
        Self {
            import_name,
            is_relative: false,
            is_import_found: false,
            is_partly_resolved: false,
            is_namespace_package: false,
            is_init_file_present: false,
            is_stub_package: false,
            import_type: ImportType::Local,
            resolved_paths: vec![],
            search_path: None,
            is_stub_file: false,
            is_native_lib: false,
            is_stdlib_typeshed_file: false,
            is_third_party_typeshed_file: false,
            is_local_typings_file: false,
            implicit_imports: Default::default(),
            filtered_implicit_imports: Default::default(),
            non_stub_import_result: None,
            py_typed_info: None,
            package_directory: None,
        }
    }
}
