use std::collections::HashMap;
use std::path::{Path, PathBuf};

use log::debug;

use crate::import_result::{ImplicitImport, ImportResult, ImportType};

mod import_result;
mod native_module;
mod py_typed;

#[derive(Debug)]
enum PythonPlatform {
    Darwin,
    Linux,
    Windows,
}

#[derive(Debug)]
enum PythonVersion {
    Py37,
    Py38,
    Py39,
    Py310,
    Py311,
}

#[derive(Debug)]
struct ExecutionEnvironment {
    root: PathBuf,
    python_version: PythonVersion,
    python_platform: PythonPlatform,
    extra_paths: Vec<PathBuf>,
}

#[derive(Debug)]
struct ImportModuleDescriptor {
    leading_dots: u32,
    name_parts: Vec<String>,
    has_trailing_dot: bool,
    // Do we need this?
    imported_symbols: Vec<String>,
}

impl ImportModuleDescriptor {
    pub(crate) fn name(&self) -> String {
        format!(
            "{}{}",
            ".".repeat(self.leading_dots as usize),
            &self.name_parts.join(".")
        )
    }
}

#[derive(Debug)]
struct PythonPathResult {
    paths: Vec<PathBuf>,
    prefix: PathBuf,
}

#[derive(Debug)]
struct Host;

impl Host {
    fn python_search_paths(&self, _python_path: Option<&Path>) -> PythonPathResult {
        todo!()
    }
    fn python_version(&self, _python_path: Option<&Path>) -> PythonVersion {
        todo!()
    }
    fn python_platform(&self) -> PythonPlatform {
        todo!()
    }
}

fn _find_implicit_imports(
    _importing_module_name: &str,
    _dir_path: &Path,
    _exclusions: &[&Path],
) -> HashMap<String, ImplicitImport> {
    todo!()
    // let mut implicit_imports: HashMap<String, ImplicitImport> = HashMap::new();

    // Enumerate all files and directories in the path, expanding links.

    // Add implicit file-based modules.

    // Add implicit directory-based modules.
}

fn _resolve_absolute_import(
    root: &Path,
    _execution_environment: &ExecutionEnvironment,
    module_descriptor: &ImportModuleDescriptor,
    import_name: &str,
    allow_partial: bool,
    allow_native_lib: bool,
    use_stub_package: bool,
    allow_pyi: bool,
    look_for_py_typed: bool,
) -> ImportResult {
    if use_stub_package {
        debug!("Attempting to resolve stub package using root path `{root:?}`");
    } else {
        debug!("Attempting to resolve using root path `{root:?}`");
    }

    // Starting at the specified path, walk the file system to find the specified module.
    let mut resolved_paths: Vec<PathBuf> = Vec::new();
    let mut dir_path = root.to_path_buf();
    let mut is_namespace_package = false;
    let mut is_init_file_present = false;
    let mut is_stub_package = false;
    let mut is_stub_file = false;
    let mut is_native_lib = false;
    // let mut implicit_imports = HashMap::new();
    let mut package_directory = None;
    let mut py_typed_info = None;

    // Handle the "from . import XXX" case.
    if module_descriptor.name_parts.is_empty() {
        let py_file_path = dir_path.join("__init__.py");
        let pyi_file_path = dir_path.join("__init__.pyi");

        if allow_pyi && pyi_file_path.is_file() {
            debug!("Resolved import with file: {pyi_file_path:?}");
            resolved_paths.push(pyi_file_path);
        } else if py_file_path.is_file() {
            debug!("Resolved import with file: {py_file_path:?}");
            resolved_paths.push(py_file_path);
        } else {
            debug!("Partially resolved import with directory: {dir_path:?}");
            // TODO(charlie): Why?
            // resolvedPaths.push('');
            is_namespace_package = true;
        }

        // TODO(charlie): Why? Oh, to support namespace packages? We have to look at imports, maybe?
        // implicitImports = this._findImplicitImports(importName, dirPath, [pyFilePath, pyiFilePath]);
    } else {
        for (i, part) in module_descriptor.name_parts.iter().enumerate() {
            let is_first_part = i == 0;
            let is_last_part = i == module_descriptor.name_parts.len() - 1;

            // Extend the path.
            if use_stub_package && is_first_part {
                // Add `-stubs` to the last part of the path.
                dir_path = dir_path.join(format!("{part}-stubs"));
            } else {
                dir_path = dir_path.join(part);
            }

            let found_directory = dir_path.is_dir();
            if found_directory {
                if is_first_part {
                    package_directory = Some(dir_path.clone());
                }

                // See if we can find an __init__.py[i] in this directory.
                let py_file_path = dir_path.join("__init__.py");
                let pyi_file_path = dir_path.join("__init__.pyi");

                if allow_pyi && pyi_file_path.is_file() {
                    debug!("Resolved import with file: {pyi_file_path:?}");
                    resolved_paths.push(pyi_file_path);

                    if is_last_part {
                        is_stub_package = true;
                    }
                    is_init_file_present = true;
                } else if py_file_path.is_file() {
                    debug!("Resolved import with file: {py_file_path:?}");
                    resolved_paths.push(py_file_path);
                    is_init_file_present = true;
                }

                if look_for_py_typed {
                    py_typed_info =
                        py_typed_info.or_else(|| py_typed::get_py_typed_info(&dir_path));
                }

                if !is_last_part {
                    // We are not at the last part, and we found a directory, so continue to look
                    // for the next part.
                    if !is_init_file_present {
                        // TODO(charlie): Why?
                        // resolvedPaths.push('');
                        is_namespace_package = true;
                        py_typed_info = None;
                    }
                    continue;
                }

                if is_init_file_present {
                    // TODO(charlie): Why?
                    // implicitImports = this._findImplicitImports(moduleDescriptor.nameParts.join('.'), dirPath, [
                    //     pyFilePath,
                    //     pyiFilePath,
                    // ]);
                    break;
                }
            }

            // We weren't able to find a directory or we found a directory with no __init__.py[i]
            // file. See if we can find a ".py" or ".pyi" file with this name.
            let py_file_path = dir_path.with_extension("py");
            let pyi_file_path = dir_path.with_extension("pyi");

            if allow_pyi && pyi_file_path.is_file() {
                debug!("Resolved import with file: {pyi_file_path:?}");
                resolved_paths.push(pyi_file_path);
                if is_last_part {
                    is_stub_file = true;
                }
            } else if py_file_path.is_file() {
                debug!("Resolved import with file: {py_file_path:?}");
                resolved_paths.push(py_file_path);
            } else {
                if allow_native_lib && dir_path.is_dir() {
                    // We didn't find a .py or .pyi file, so see if we can find a native library
                    // with this name.
                    if let Some(native_lib_path) = dir_path
                        .read_dir()
                        .unwrap()
                        .flatten()
                        .filter(|entry| entry.file_type().map_or(false, |ft| ft.is_file()))
                        .find(|entry| {
                            native_module::is_native_module_file_name(&dir_path, &entry.path())
                        })
                    {
                        debug!("Resolved import with file: {native_lib_path:?}");
                        is_native_lib = true;
                        resolved_paths.push(native_lib_path.path());
                    }
                }

                if !is_native_lib && found_directory {
                    debug!("Partially resolved import with directory: {dir_path:?}");
                    // TODO(charlie): Why?
                    // resolvedPaths.push('');
                    if is_last_part {
                        // TODO(charlie): Why?
                        // implicitImports = this._findImplicitImports(importName, dirPath, [pyFilePath, pyiFilePath]);
                        is_namespace_package = true;
                    }
                } else if is_native_lib {
                    debug!("Did not find file {py_file_path:?} or {pyi_file_path:?}");
                }
            }
            break;
        }
    }

    let import_found = if allow_partial {
        !resolved_paths.is_empty()
    } else {
        resolved_paths.len() == module_descriptor.name_parts.len()
    };

    ImportResult {
        import_name: import_name.to_string(),
        is_relative: false,
        is_import_found: import_found,
        is_partly_resolved: !resolved_paths.is_empty()
            && resolved_paths.len() < module_descriptor.name_parts.len(),
        is_namespace_package,
        is_init_file_present,
        is_stub_package,
        import_type: ImportType::Local,
        resolved_paths,
        search_path: Some(root.into()),
        is_stub_file,
        is_native_lib,
        is_stdlib_typeshed_file: false,
        is_third_party_typeshed_file: false,
        is_local_typings_file: false,
        implicit_imports: HashMap::default(),
        filtered_implicit_imports: HashMap::default(),
        non_stub_import_result: None,
        py_typed_info,
        package_directory,
    }
}

// Follows import resolution algorithm defined in PEP-420:
// https://www.python.org/dev/peps/pep-0420/
fn resolve_absolute_import(
    root: &Path,
    execution_environment: &ExecutionEnvironment,
    module_descriptor: &ImportModuleDescriptor,
    import_name: &str,
    allow_partial: bool,
    allow_native_lib: bool,
    use_stub_package: bool,
    allow_pyi: bool,
    look_for_py_typed: bool,
) -> ImportResult {
    if allow_pyi && use_stub_package {
        // Look for packaged stubs first. PEP 561 indicates that package authors can ship
        // their stubs separately from their package implementation by appending the string
        // '-stubs' to its top - level directory name. We'll look there first.
        let import_result = _resolve_absolute_import(
            root,
            execution_environment,
            module_descriptor,
            import_name,
            allow_partial,
            false,
            true,
            true,
            true,
        );

        // We found fully typed stub packages.
        if import_result.package_directory.is_some() {
            // If this is a namespace package that wasn't resolved, assume that
            // it's a partial stub package and continue looking for a real package.
            if !import_result.is_namespace_package || import_result.is_import_found {
                return import_result;
            }
        }
    }

    _resolve_absolute_import(
        root,
        execution_environment,
        module_descriptor,
        import_name,
        allow_partial,
        allow_native_lib,
        false,
        allow_pyi,
        look_for_py_typed,
    )
}

struct Config {
    // Absolute directory of project. All relative paths in the config
    // are based on this path.
    // project_root: String,

    // Path to python interpreter.
    // python_path: Option<PathBuf>,

    // Path to use for typeshed definitions.
    typeshed_path: Option<PathBuf>,

    // Path to custom typings (stub) modules.
    stub_path: Option<PathBuf>,
}

fn _resolve_best_absolute_import(
    _source_file: &Path,
    execution_environment: &ExecutionEnvironment,
    module_descriptor: &ImportModuleDescriptor,
    allow_pyi: bool,
    config: &Config,
    host: &Host,
) -> Option<ImportResult> {
    let import_name = module_descriptor.name();

    // Search for local stub files (using `stub_path`).
    if allow_pyi {
        if let Some(stub_path) = config.stub_path.as_ref() {
            debug!("Looking in stub path: {}", stub_path.display());

            let mut typings_import = resolve_absolute_import(
                stub_path,
                execution_environment,
                module_descriptor,
                &import_name,
                // Pyright leaves this `undefined`.
                false,
                false,
                true,
                allow_pyi,
                false,
            );

            if typings_import.is_import_found {
                // We will treat typings file as "local" rather than "third-party".
                typings_import.import_type = ImportType::Local;
                typings_import.is_local_typings_file = true;

                // If it's a namespace package that didn't resolve to a file, make sure that the
                // imported symbols are present in the implicit imports. If not, we'll skip the
                // typings import and continue searching.
                // STOPSHIP(charlie): The second condition here looks wrong. Pyright uses:
                // `!typingsImport.resolvedPaths[typingsImport.resolvedPaths.length - 1]`
                if typings_import.is_namespace_package
                    && typings_import.resolved_paths.last().is_none()
                {
                    if _is_namespace_package_resolved(
                        module_descriptor,
                        &typings_import.implicit_imports,
                    ) {
                        return Some(typings_import);
                    }
                } else {
                    return Some(typings_import);
                }
            }

            return None;
        }
    }

    let mut best_result_so_far: Option<ImportResult> = None;
    let _local_import: Option<ImportResult> = None;

    // Look in the root directory of the execution environment.
    // STOPSHIP(charlie): In Pyright, they check `if (execEnv.root)`?
    debug!(
        "Looking in root directory of execution environment: {}",
        execution_environment.root.display()
    );

    let mut local_import = resolve_absolute_import(
        &execution_environment.root,
        execution_environment,
        module_descriptor,
        &import_name,
        false,
        true,
        true,
        allow_pyi,
        false,
    );
    local_import.import_type = ImportType::Local;

    best_result_so_far = Some(local_import);

    // Look in any extra paths.
    for extra_path in &execution_environment.extra_paths {
        debug!("Looking in extra path: {}", extra_path.display());

        let mut local_import = resolve_absolute_import(
            extra_path,
            execution_environment,
            module_descriptor,
            &import_name,
            false,
            true,
            true,
            allow_pyi,
            false,
        );
        local_import.import_type = ImportType::Local;

        best_result_so_far = _pick_best_import(best_result_so_far, local_import, module_descriptor);
    }

    // Look for third-party imports in Python's `sys` path.
    for search_path in _get_python_search_paths(config, host) {
        debug!("Looking in Python search path: {}", search_path.display());

        let mut third_party_import = resolve_absolute_import(
            &search_path,
            execution_environment,
            module_descriptor,
            &import_name,
            false,
            true,
            true,
            allow_pyi,
            false,
        );
        third_party_import.import_type = ImportType::ThirdParty;

        best_result_so_far =
            _pick_best_import(best_result_so_far, third_party_import, module_descriptor);
    }

    // If a library is fully py.typed, then we have found the best match,
    // unless the execution environment is typeshed itself, in which case
    // we don't want to favor py.typed libraries. Use the typeshed lookup below.
    if _get_typeshed_root(config.typeshed_path.as_ref()) != execution_environment.root {
        if best_result_so_far.as_ref().map_or(false, |result| {
            result.py_typed_info.is_some() && !result.is_partly_resolved
        }) {
            return best_result_so_far;
        }
    }

    if allow_pyi && !module_descriptor.name_parts.is_empty() {
        // Check for a stdlib typeshed file.
        debug!("Looking for typeshed stdlib path: {}", import_name);
        if let Some(mut typeshed_stdilib_import) =
            _find_typeshed_path(execution_environment, module_descriptor, &import_name, true)
        {
            typeshed_stdilib_import.is_stdlib_typeshed_file = true;
            return Some(typeshed_stdilib_import);
        }

        // Check for a third-party typeshed file.
        debug!("Looking for typeshed third-party path: {}", import_name);
        if let Some(mut typeshed_third_party_import) = _find_typeshed_path(
            execution_environment,
            module_descriptor,
            &import_name,
            false,
        ) {
            typeshed_third_party_import.is_third_party_typeshed_file = true;

            best_result_so_far = _pick_best_import(
                best_result_so_far,
                typeshed_third_party_import,
                module_descriptor,
            );
        }
    }

    // We weren't able to find an exact match, so return the best
    // partial match.
    best_result_so_far
}

fn _get_python_search_paths(_config: &Config, _host: &Host) -> Vec<PathBuf> {
    // todo!("_get_python_search_paths")
    vec![]
}

fn _get_typeshed_root(_typeshed_path: Option<&PathBuf>) -> PathBuf {
    // todo!("_get_typeshed_root")
    Path::new("typeshed").to_path_buf()
}

fn _is_namespace_package_resolved(
    _module_descriptor: &ImportModuleDescriptor,
    _implicit_imports: &HashMap<String, ImplicitImport>,
) -> bool {
    todo!("_is_namespace_package_resolved")
}

fn _find_typeshed_path(
    _execution_environment: &ExecutionEnvironment,
    _module_descriptor: &ImportModuleDescriptor,
    _import_name: &str,
    _is_std_lib: bool,
) -> Option<ImportResult> {
    // todo!("_find_typeshed_path")
    None
}

fn _pick_best_import(
    _best_result_so_far: Option<ImportResult>,
    _new_import: ImportResult,
    _module_descriptor: &ImportModuleDescriptor,
) -> Option<ImportResult> {
    todo!("_pick_best_import")
}

fn _resolve_relative_import(
    _import_name: &str,
    _source_file: &Path,
    _execution_environment: &ExecutionEnvironment,
    _module_descriptor: &ImportModuleDescriptor,
) -> Option<ImportResult> {
    todo!("_resolve_relative_import")
}

fn _resolve_import_strict(
    import_name: &str,
    source_file: &Path,
    execution_environment: &ExecutionEnvironment,
    module_descriptor: &ImportModuleDescriptor,
    config: &Config,
    host: &Host,
) -> ImportResult {
    // TODO(charlie): Add caching.
    if module_descriptor.leading_dots > 0 {
        // Is it a relative import?
        let relative_import = _resolve_relative_import(
            import_name,
            source_file,
            execution_environment,
            module_descriptor,
        );

        if let Some(mut relative_import) = relative_import {
            relative_import.is_relative = true;
            return relative_import;
        }
    } else {
        let best_import = _resolve_best_absolute_import(
            source_file,
            execution_environment,
            module_descriptor,
            true,
            config,
            host,
        );

        if let Some(mut best_import) = best_import {
            if best_import.is_stub_file {
                best_import.non_stub_import_result = Some(Box::new(
                    _resolve_best_absolute_import(
                        source_file,
                        execution_environment,
                        module_descriptor,
                        false,
                        config,
                        host,
                    )
                    .unwrap_or_else(|| ImportResult::not_found(import_name.to_string())),
                ));
            }
            return best_import;
        }
    }

    ImportResult::not_found(import_name.to_string())
}

fn resolve_import_internal(
    source_file: &Path,
    execution_environment: &ExecutionEnvironment,
    module_descriptor: &ImportModuleDescriptor,
    config: &Config,
    host: &Host,
) -> ImportResult {
    let import_name = module_descriptor.name();

    let import_result = _resolve_import_strict(
        &import_name,
        source_file,
        execution_environment,
        module_descriptor,
        config,
        host,
    );
    if import_result.is_import_found || module_descriptor.leading_dots > 0 {
        return import_result;
    }

    // If the import is absolute and no other method works, try resolving the absolute in the
    // importing file's directory, then the parent directory, and so on, until the import root is
    // reached.
    todo!()
}

fn resolve_import(
    source_file: &Path,
    execution_environment: &ExecutionEnvironment,
    module_descriptor: &ImportModuleDescriptor,
    config: &Config,
    host: &Host,
) -> ImportResult {
    resolve_import_internal(
        source_file,
        execution_environment,
        module_descriptor,
        config,
        host,
    )
}

fn main() {
    env_logger::init();

    let source_file =
        PathBuf::from("/Users/crmarsh/workspace/staging/crates/ruff_python_resolver/foo/bar.py");

    let execution_environment = ExecutionEnvironment {
        root: PathBuf::from("/Users/crmarsh/workspace/staging/crates/ruff_python_resolver/foo"),
        python_version: PythonVersion::Py37,
        python_platform: PythonPlatform::Darwin,
        extra_paths: vec![],
    };

    let module_descriptor = ImportModuleDescriptor {
        leading_dots: 0,
        name_parts: vec!["baz".to_string()],
        has_trailing_dot: false,
        imported_symbols: vec![],
    };

    let config = Config {
        typeshed_path: None,
        stub_path: None,
    };

    let host = Host;

    println!(
        "{:?}",
        resolve_import(
            &source_file,
            &execution_environment,
            &module_descriptor,
            &config,
            &host
        )
    );
}

#[cfg(test)]
mod tests {
    use std::fs::{create_dir, create_dir_all, File};
    use std::io::{self, Write};
    use std::path::{Path, PathBuf};

    use tempfile::TempDir;

    use crate::import_result::{ImportResult, ImportType};
    use crate::{
        resolve_import, Config, ExecutionEnvironment, Host, ImportModuleDescriptor, PythonPlatform,
        PythonVersion,
    };

    struct PythonFile {
        path: PathBuf,
        content: String,
    }

    impl PythonFile {
        fn new(path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
            Self {
                path: path.into(),
                content: content.into(),
            }
        }

        fn create(&self, dir: &TempDir) -> io::Result<PathBuf> {
            let file_path = dir.path().join(self.path.as_path());
            if let Some(parent) = file_path.parent() {
                create_dir_all(parent)?;
            }
            let mut f = File::create(&file_path)?;
            f.write_all(self.content.as_bytes())?;
            f.sync_all()?;

            Ok(file_path)
        }
    }

    fn resolve(
        source_file: impl AsRef<Path>,
        name: &str,
        root: impl Into<PathBuf>,
    ) -> io::Result<ImportResult> {
        env_logger::init();

        let execution_environment = ExecutionEnvironment {
            root: root.into(),
            python_version: PythonVersion::Py37,
            python_platform: PythonPlatform::Darwin,
            extra_paths: vec![],
        };

        let module_descriptor = ImportModuleDescriptor {
            leading_dots: 0,
            name_parts: name
                .split('.')
                .map(std::string::ToString::to_string)
                .collect(),
            has_trailing_dot: false,
            imported_symbols: vec![],
        };

        let config = Config {
            typeshed_path: None,
            stub_path: None,
        };

        let host = Host;

        Ok(resolve_import(
            source_file.as_ref(),
            &execution_environment,
            &module_descriptor,
            &config,
            &host,
        ))
    }

    #[test]
    fn import_side_by_side_file_root() -> io::Result<()> {
        // Create a temporary directory.
        let temp_dir = TempDir::new().expect("Failed to create temp directory");

        let dir = TempDir::new()?;

        let file1 = PythonFile::new("file1.py", "import file1").create(&dir)?;
        let file2 = PythonFile::new("file2.py", "import file2").create(&dir)?;

        // Attempt to import `file1` from `file2.py`.
        let result = resolve(file2, "file1", dir.path())?;

        assert!(result.is_import_found);
        assert_eq!(result.import_type, ImportType::Local);
        assert_eq!(result.resolved_paths, vec![file1]);

        Ok(())
    }

    #[test]
    fn import_side_by_side_file_sub_folder() -> io::Result<()> {
        let dir = TempDir::new()?;

        let init_py = PythonFile::new("test/__init__.py", "").create(&dir)?;
        let file1 = PythonFile::new("test/file1.py", "import file1").create(&dir)?;
        let file2 = PythonFile::new("test/file2.py", "import file2").create(&dir)?;

        // Attempt to import `file1` from `file2.py`.
        let result = resolve(file2, "test.file1", dir.path())?;

        assert!(result.is_import_found);
        assert_eq!(result.import_type, ImportType::Local);
        assert_eq!(result.resolved_paths, vec![init_py, file1]);

        Ok(())
    }
}
