#!/usr/bin/env python3
"""Run Python code through a compiler-backed import wrapper.

This script emulates the common CPython entrypoints:
- ``runpy.py -m package.module [args...]``
- ``runpy.py path/to/script [args...]``
- ``runpy.py -c "code" [args...]``

For source modules loaded through importlib, it installs a custom meta path
finder/loader that compiles ``.py`` sources to ``.pyc`` and executes those
code objects. Compilation backend is selected with ``--compiler``:
- path to ``pycomparse`` (default)
- ``builtin`` to use CPython's ``py_compile``

Generated ``.pyc`` files are written under ``--pyc-dir``. If omitted, a
temporary cache directory is created and removed after execution.

Use ``--path`` (repeatable) to prepend paths to ``sys.path`` before module
resolution. Use ``-v`` and/or ``--log-file`` to record compiler invocations.

At runtime it also writes a small re-exec wrapper under the selected pyc
directory and sets ``sys.executable`` to that wrapper so subprocesses (for
example ``multiprocessing`` spawn workers) reuse the same compiler settings.

For compatibility with interpreter-style respawns, leading ``-E``, ``-I``,
``-b``, ``-bb``, ``-u``, ``-v``, ``-W`` and ``-X`` options are accepted.
These options are also preserved for re-exec wrapper invocations.
"""

from __future__ import annotations

import argparse
import hashlib
import importlib
import importlib.abc
import importlib.machinery
import importlib.util
import marshal
import os
import py_compile
import shlex
import subprocess
import sys
import tempfile
import types
from pathlib import Path
from types import CodeType
from typing import Optional, TextIO


def is_python38() -> bool:
    return sys.version_info[:2] == (3, 8)


def print_python_hint() -> None:
    version = sys.version.split()[0]
    print(
        f"error: scripts/runpy.py requires Python 3.8 "
        f"(found: {version})",
        file=sys.stderr,
    )
    print(
        "hint: run with 'uv run --no-project python -B "
        "scripts/runpy.py ...'",
        file=sys.stderr,
    )


def load_code_from_pyc(pyc_path: Path) -> CodeType:
    data = pyc_path.read_bytes()
    if len(data) < 16:
        raise RuntimeError(f"invalid pyc header in {pyc_path}")
    try:
        return marshal.loads(data[16:])
    except ValueError as exc:
        raise RuntimeError(
            f"failed to load generated pyc {pyc_path}. "
            "This usually means the wrapper is not running under Python 3.8."
        ) from exc


def compile_source_with_pycomparse(
    pycomparse_bin: Path,
    source_path: Path,
    pyc_path: Path,
    verbose: bool,
    log_file: Optional[TextIO],
) -> None:
    pyc_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = [str(pycomparse_bin), "--out", str(pyc_path), str(source_path)]
    log_command(cmd, verbose, log_file)
    proc = subprocess.run(
        cmd,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode == 0:
        return
    stderr = proc.stderr.decode(errors="replace")
    if "LeakSanitizer has encountered a fatal error" in stderr:
        stderr += (
            "\nhint: LeakSanitizer may fail under ptrace. "
            "Try ASAN_OPTIONS=detect_leaks=0.\n"
        )
    raise RuntimeError(
        f"pycomparse failed for {source_path} (exit {proc.returncode})\n{stderr}"
    )


def compile_source_builtin(
    source_path: Path, pyc_path: Path, verbose: bool, log_file: Optional[TextIO]
) -> None:
    pyc_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = ["python", "-m", "py_compile", str(source_path), str(pyc_path)]
    log_command(cmd, verbose, log_file)
    try:
        py_compile.compile(str(source_path), cfile=str(pyc_path), doraise=True)
    except py_compile.PyCompileError as exc:
        raise RuntimeError(
            f"builtin compile failed for {source_path}\n{exc.msg}"
        ) from exc


def compile_source(
    compiler: str,
    source_path: Path,
    pyc_path: Path,
    verbose: bool,
    log_file: Optional[TextIO],
) -> None:
    pyc_path.parent.mkdir(parents=True, exist_ok=True)
    fd, tmp_name = tempfile.mkstemp(
        prefix=f"{pyc_path.name}.",
        suffix=".pyc.tmp",
        dir=str(pyc_path.parent),
    )
    os.close(fd)
    tmp_pyc_path = Path(tmp_name)
    try:
        if compiler == "builtin":
            compile_source_builtin(source_path, tmp_pyc_path, verbose, log_file)
        else:
            compile_source_with_pycomparse(
                Path(compiler), source_path, tmp_pyc_path, verbose, log_file
            )
        os.replace(str(tmp_pyc_path), str(pyc_path))
    except Exception:
        try:
            tmp_pyc_path.unlink()
        except FileNotFoundError:
            pass
        raise


def log_command(cmd: list[str], verbose: bool, log_file: Optional[TextIO]) -> None:
    line = "+ " + " ".join(shlex.quote(part) for part in cmd)
    if verbose:
        print(line, file=sys.stderr)
    if log_file is not None:
        print(line, file=log_file, flush=True)


class PycStore:
    def __init__(self, base_dir: Path):
        self.base_dir = base_dir.resolve()

    def pyc_path_for(self, source_path: Path) -> Path:
        resolved = source_path.resolve()
        digest = hashlib.sha256(str(resolved).encode("utf-8")).hexdigest()
        return self.base_dir / digest[:2] / f"{resolved.stem}-{digest[2:18]}.pyc"


class CompilerSourceLoader(importlib.machinery.SourceFileLoader):
    def __init__(
        self,
        fullname: str,
        path: str,
        compiler: str,
        pyc_store: PycStore,
        verbose: bool,
        log_file: Optional[TextIO],
    ):
        super().__init__(fullname, path)
        self.compiler = compiler
        self.pyc_store = pyc_store
        self.verbose = verbose
        self.log_file = log_file

    def get_code(self, fullname: str):
        source_path = Path(self.path)
        pyc_path = self.pyc_store.pyc_path_for(source_path)
        compile_source(
            self.compiler, source_path, pyc_path, self.verbose, self.log_file
        )
        return load_code_from_pyc(pyc_path)


class CompilerMetaFinder(importlib.abc.MetaPathFinder):
    def __init__(
        self,
        compiler: str,
        pyc_store: PycStore,
        verbose: bool,
        log_file: Optional[TextIO],
    ):
        self.compiler = compiler
        self.pyc_store = pyc_store
        self.verbose = verbose
        self.log_file = log_file

    def find_spec(self, fullname: str, path=None, target=None):
        spec = importlib.machinery.PathFinder.find_spec(fullname, path, target)
        if spec is None or spec.origin is None:
            return spec
        if (
            isinstance(spec.loader, importlib.machinery.SourceFileLoader)
            and spec.origin.endswith(".py")
        ):
            loader = CompilerSourceLoader(
                fullname,
                spec.origin,
                self.compiler,
                self.pyc_store,
                self.verbose,
                self.log_file,
            )
            spec.loader = loader
            spec.cached = str(self.pyc_store.pyc_path_for(Path(spec.origin)))
        return spec


def install_compiler_importer(
    compiler: str, pyc_store: PycStore, verbose: bool, log_file: Optional[TextIO]
) -> CompilerMetaFinder:
    finder = CompilerMetaFinder(compiler, pyc_store, verbose, log_file)
    sys.meta_path.insert(0, finder)
    return finder


def run_script_file(
    script_path: Path,
    script_args: list[str],
    compiler: str,
    pyc_store: PycStore,
    verbose: bool,
    log_file: Optional[TextIO],
    prepend_paths: list[str],
) -> int:
    main_pyc = pyc_store.pyc_path_for(script_path)
    compile_source(compiler, script_path, main_pyc, verbose, log_file)
    code = load_code_from_pyc(main_pyc)
    execute_code_as_main(
        code=code,
        argv0=str(script_path),
        argv_tail=script_args,
        path0=str(script_path.parent),
        prepend_paths=prepend_paths,
        file_name=str(script_path),
        cached_name=str(main_pyc),
        loader=importlib.machinery.SourceFileLoader("__main__", str(script_path)),
    )
    return 0


def run_command(command: str, command_args: list[str], prepend_paths: list[str]) -> int:
    code = compile(command, "<string>", "exec")
    execute_code_as_main(
        code=code,
        argv0="-c",
        argv_tail=command_args,
        path0="",
        prepend_paths=prepend_paths,
        file_name=None,
        cached_name=None,
        loader=None,
    )
    return 0


def execute_code_as_main(
    code: CodeType,
    argv0: str,
    argv_tail: list[str],
    path0: str,
    prepend_paths: list[str],
    file_name: Optional[str],
    cached_name: Optional[str],
    loader: Optional[object],
) -> None:
    sys.argv = [argv0, *argv_tail]
    if sys.path:
        sys.path[0] = path0
    else:
        sys.path.append(path0)
    for path in reversed(prepend_paths):
        if path in sys.path:
            sys.path.remove(path)
        sys.path.insert(0, path)

    main_module = types.ModuleType("__main__")
    main_globals = main_module.__dict__
    main_globals.update(
        {
            "__name__": "__main__",
            "__package__": None,
            "__spec__": None,
            "__builtins__": __builtins__,
        }
    )
    if file_name is not None:
        main_globals["__file__"] = file_name
    if cached_name is not None:
        main_globals["__cached__"] = cached_name
    if loader is not None:
        main_globals["__loader__"] = loader
    sys.modules["__main__"] = main_module
    exec(code, main_globals)


def run_module(module_name: str, module_args: list[str]) -> int:
    stdlib_runpy = load_stdlib_runpy_module()
    sys.argv = [module_name, *module_args]
    stdlib_runpy.run_module(module_name, run_name="__main__", alter_sys=True)
    return 0


def prepend_sys_path(paths: list[str]) -> list[str]:
    for path in reversed(paths):
        if path in sys.path:
            sys.path.remove(path)
        sys.path.insert(0, path)
    return paths


_STDLIB_RUNPY = None


def load_stdlib_runpy_module():
    global _STDLIB_RUNPY
    if _STDLIB_RUNPY is not None:
        return _STDLIB_RUNPY
    stdlib_runpy_path = Path(os.__file__).resolve().parent / "runpy.py"
    spec = importlib.util.spec_from_file_location(
        "_pycomparse_stdlib_runpy", str(stdlib_runpy_path)
    )
    if spec is None or spec.loader is None:
        raise RuntimeError(f"failed to load stdlib runpy from {stdlib_runpy_path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    _STDLIB_RUNPY = module
    return module


def parse_args(argv: list[str]) -> argparse.Namespace:
    ignore_environment = False
    isolated_mode = False
    runtime_binary_flags: list[str] = []
    runtime_woptions: list[str] = []
    runtime_xoptions: list[str] = []

    def preprocess_compat_flags(raw_argv: list[str]) -> list[str]:
        nonlocal ignore_environment
        nonlocal isolated_mode
        nonlocal runtime_binary_flags
        nonlocal runtime_woptions
        nonlocal runtime_xoptions
        prefix: list[str] = []
        i = 0
        while i < len(raw_argv):
            tok = raw_argv[i]
            if tok in ("-m", "-c", "--") or not tok.startswith("-"):
                break
            if tok == "-E":
                ignore_environment = True
                i += 1
                continue
            if tok == "-I":
                isolated_mode = True
                i += 1
                continue
            if tok in ("-b", "-bb", "-u"):
                runtime_binary_flags.append(tok)
                i += 1
                continue
            if tok == "-W":
                if i + 1 < len(raw_argv):
                    runtime_woptions.append(raw_argv[i + 1])
                    i += 2
                else:
                    i += 1
                continue
            if tok.startswith("-W") and len(tok) > 2:
                runtime_woptions.append(tok[2:])
                i += 1
                continue
            if tok == "-X":
                if i + 1 < len(raw_argv):
                    runtime_xoptions.append(raw_argv[i + 1])
                    i += 2
                else:
                    i += 1
                continue
            if tok.startswith("-X") and len(tok) > 2:
                runtime_xoptions.append(tok[2:])
                i += 1
                continue
            prefix.append(tok)
            if tok in ("--compiler", "--pyc-dir", "--log-file", "--path"):
                if i + 1 < len(raw_argv):
                    prefix.append(raw_argv[i + 1])
                    i += 2
                else:
                    i += 1
                continue
            i += 1
        return prefix + raw_argv[i:]

    argv = preprocess_compat_flags(argv)

    default_compiler = os.environ.get(
        "PYCOMPARSE", os.environ.get("PARSER_TEST", "build/pycomparse")
    )
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--compiler",
        action="append",
        help="Path to pycomparse compiler binary, or 'builtin'.",
    )
    parser.add_argument(
        "--pyc-dir",
        help=(
            "Directory where generated .pyc files are written. "
            "Default: temporary directory removed after execution."
        ),
    )
    parser.add_argument(
        "--pyc",
        dest="pyc_dir",
        help=argparse.SUPPRESS,
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print every compiler invocation.",
    )
    parser.add_argument(
        "--log-file",
        help="Write every compiler invocation to this log file.",
    )
    parser.add_argument(
        "-m",
        dest="module",
        help="Run library module as a script (like python -m).",
    )
    parser.add_argument(
        "--path",
        action="append",
        default=[],
        help="Prepend this path to sys.path (repeatable).",
    )
    args, remaining = parser.parse_known_args(argv)
    args.ignore_environment = ignore_environment
    args.isolated_mode = isolated_mode
    args.runtime_binary_flags = runtime_binary_flags
    args.runtime_woptions = runtime_woptions
    args.runtime_xoptions = runtime_xoptions
    if args.compiler is None:
        args.compiler = default_compiler
    else:
        args.compiler = args.compiler[-1]

    def strip_separator(items: list[str]) -> list[str]:
        if items and items[0] == "--":
            return items[1:]
        return items

    if args.module is not None:
        args.mode = "module"
        args.script = None
        args.target_args = strip_separator(remaining)
        return args

    if remaining and remaining[0] == "-c":
        if len(remaining) < 2:
            parser.error("argument -c: expected one argument")
        args.mode = "command"
        args.command = remaining[1]
        args.target_args = strip_separator(remaining[2:])
        return args

    had_script_separator = False
    if remaining and remaining[0] == "--":
        had_script_separator = True
        remaining = remaining[1:]
    if not remaining:
        parser.error("expected -m <module> or <script-path>")
    if remaining[0].startswith("-") and not had_script_separator:
        parser.error(f"unrecognized argument: {remaining[0]}")
    args.mode = "script"
    args.script = remaining[0]
    args.target_args = strip_separator(remaining[1:])
    return args


def create_reexec_wrapper(
    pyc_dir: Path,
    compiler: str,
    prepend_paths: list[str],
    log_file: Optional[Path],
    verbose: bool,
    runtime_flags: list[str],
) -> Path:
    wrapper_path = pyc_dir / "python-wrapper"
    runpy_path = Path(__file__).resolve()
    python_exe = str(Path(sys.executable).resolve())

    wrapper_args = [str(runpy_path), "--compiler", compiler, "--pyc-dir", str(pyc_dir)]
    for path in prepend_paths:
        wrapper_args.extend(["--path", path])
    if log_file is not None:
        wrapper_args.extend(["--log-file", str(log_file)])
    if verbose:
        wrapper_args.append("-v")

    wrapper_source = (
        "#!/usr/bin/env python3\n"
        "import os, sys\n"
        f"python = {python_exe!r}\n"
        f"base = {wrapper_args!r}\n"
        f"default_runtime = {runtime_flags!r}\n"
        "argv = sys.argv[1:]\n"
        "runtime = []\n"
        "i = 0\n"
        "while i < len(argv):\n"
        "    tok = argv[i]\n"
        "    if tok in ('-m', '-c', '--') or not tok.startswith('-'):\n"
        "        break\n"
        "    if tok == '-E':\n"
        "        runtime.append(tok)\n"
        "        i += 1\n"
        "        continue\n"
        "    if tok == '-I':\n"
        "        runtime.append(tok)\n"
        "        i += 1\n"
        "        continue\n"
        "    if tok in ('-b', '-bb', '-u'):\n"
        "        runtime.append(tok)\n"
        "        i += 1\n"
        "        continue\n"
        "    if tok == '-v' or (tok.startswith('-v') and tok[1:] and set(tok[1:]) == {'v'}):\n"
        "        runtime.append(tok)\n"
        "        i += 1\n"
        "        continue\n"
        "    if tok == '-W':\n"
        "        runtime.append(tok)\n"
        "        if i + 1 < len(argv):\n"
        "            runtime.append(argv[i + 1])\n"
        "            i += 2\n"
        "        else:\n"
        "            i += 1\n"
        "        continue\n"
        "    if tok.startswith('-W') and len(tok) > 2:\n"
        "        runtime.append(tok)\n"
        "        i += 1\n"
        "        continue\n"
        "    if tok == '-X':\n"
        "        runtime.append(tok)\n"
        "        if i + 1 < len(argv):\n"
        "            runtime.append(argv[i + 1])\n"
        "            i += 2\n"
        "        else:\n"
        "            i += 1\n"
        "        continue\n"
        "    if tok.startswith('-X') and len(tok) > 2:\n"
        "        runtime.append(tok)\n"
        "        i += 1\n"
        "        continue\n"
        "    break\n"
        "rest = argv[i:]\n"
        "os.execv(python, [python, *default_runtime, *runtime, '-B', *base, *rest])\n"
    )
    wrapper_path.write_text(wrapper_source, encoding="utf-8")
    wrapper_path.chmod(0o755)
    return wrapper_path


def install_reexec_wrapper(wrapper_path: Path) -> None:
    sys.executable = str(wrapper_path)
    os.environ["PYTHONEXECUTABLE"] = str(wrapper_path)
    try:
        import multiprocessing

        multiprocessing.set_executable(sys.executable)
    except Exception:
        pass


def main(argv: list[str] | None = None) -> int:
    if not is_python38():
        print_python_hint()
        return 2

    args = parse_args(sys.argv[1:] if argv is None else argv)
    # Avoid stdlib importers creating __pycache__ in source trees.
    sys.dont_write_bytecode = True

    compiler = args.compiler
    if compiler != "builtin":
        pycomparse_bin = Path(compiler)
        if not pycomparse_bin.is_file() or not os.access(str(pycomparse_bin), os.X_OK):
            print(
                f"error: pycomparse binary is not executable: {pycomparse_bin}",
                file=sys.stderr,
            )
            return 2
        compiler = str(pycomparse_bin.resolve())

    temp_dir_obj = None
    if args.pyc_dir:
        pyc_dir = Path(args.pyc_dir).resolve()
        pyc_dir.mkdir(parents=True, exist_ok=True)
    else:
        temp_dir_obj = tempfile.TemporaryDirectory(prefix="pycomparse-pyc.")
        pyc_dir = Path(temp_dir_obj.name)

    log_file = None
    log_path = None
    if args.log_file:
        log_path = Path(args.log_file).resolve()
        try:
            log_path.parent.mkdir(parents=True, exist_ok=True)
            log_file = log_path.open("a", encoding="utf-8")
        except OSError as exc:
            print(f"error: failed to open log file {log_path}: {exc}", file=sys.stderr)
            if temp_dir_obj is not None:
                temp_dir_obj.cleanup()
            return 2

    try:
        prepend_paths = [str(Path(path).resolve()) for path in args.path]
        runtime_flags: list[str] = []
        if args.ignore_environment:
            runtime_flags.append("-E")
        if args.isolated_mode:
            runtime_flags.append("-I")
        runtime_flags.extend(args.runtime_binary_flags)
        runtime_flags.extend(f"-W{opt}" for opt in args.runtime_woptions)
        runtime_flags.extend(f"-X{opt}" for opt in args.runtime_xoptions)
        pyc_store = PycStore(pyc_dir)
        install_compiler_importer(compiler, pyc_store, args.verbose, log_file)
        wrapper_path = create_reexec_wrapper(
            pyc_dir=pyc_dir,
            compiler=compiler,
            prepend_paths=prepend_paths,
            log_file=log_path,
            verbose=args.verbose,
            runtime_flags=runtime_flags,
        )
        install_reexec_wrapper(wrapper_path)

        if args.mode == "module":
            cwd = str(Path.cwd())
            prepend_paths = prepend_sys_path(prepend_paths)
            if cwd in sys.path:
                sys.path.remove(cwd)
            sys.path.insert(len(prepend_paths), cwd)
            return run_module(args.module, args.target_args)

        if args.mode == "command":
            prepend_paths = prepend_sys_path(prepend_paths)
            return run_command(args.command, args.target_args, prepend_paths)

        script_path = Path(args.script).resolve()
        if not script_path.is_file():
            print(f"error: script does not exist: {script_path}", file=sys.stderr)
            return 2
        prepend_paths = prepend_sys_path(prepend_paths)
        return run_script_file(
            script_path,
            args.target_args,
            compiler,
            pyc_store,
            args.verbose,
            log_file,
            prepend_paths,
        )
    except RuntimeError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    finally:
        if log_file is not None:
            log_file.close()
        if temp_dir_obj is not None:
            temp_dir_obj.cleanup()


if __name__ == "__main__":
    raise SystemExit(main())
