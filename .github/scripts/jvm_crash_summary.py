#!/usr/bin/env python3
"""Extract allowlisted crash hints; a missing report does not exclude a crash."""

import argparse
import json
import os
from pathlib import Path
import re
import stat


MAX_REPORTS = 8
MAX_REPORT_BYTES = 1024 * 1024
MAX_TOTAL_BYTES = 4 * MAX_REPORT_BYTES
SIGNALS = {"SIGSEGV", "SIGABRT", "SIGBUS", "SIGILL", "SIGFPE"}
REPORT_NAME = re.compile(r"hs_err_pid[0-9]+\.log\Z")
CLASS_DECL = re.compile(r"^\s*(?:(?:final|abstract|case)\s+)*class\s+([A-Za-z][A-Za-z0-9_]*)\b", re.M)


def current_java_operation(text):
    """Read only the first Java frame in an unambiguous current-thread section."""
    currents = list(re.finditer(r"^Current thread \([^\n]*\):[^\n]*$", text, re.M))
    if len(currents) != 1:
        return None
    tail = text[currents[0].end():]
    boundary = re.search(r"^(?:---------------|Other [Tt]hreads?\b|VM Arguments:|Environment Variables:)", tail, re.M)
    if boundary:
        tail = tail[:boundary.start()]
    sections = list(re.finditer(r"^Java frames:[^\n]*\n", tail, re.M))
    if len(sections) != 1:
        return None
    for line in tail[sections[0].end():].splitlines():
        if not re.match(r"^[JjVvC]\s", line):
            break
        if re.match(r"^[Jj]\s", line):
            match = re.match(
                r"^(?:j\s+|J\s+[0-9]{1,10}%?\s+(?:(?:c1|c2)\s+)?)"
                r"org\.fusesource\.leveldbjni\.internal\.NativeDB\$DBJNI\.(delete|Get)(?:\(|\s|$)",
                line,
            )
            return {"delete": "NativeDB.delete", "Get": "NativeDB.Get"}.get(match[1]) if match else None
    return None


def is_link(path):
    return path.is_symlink() or (hasattr(path, "is_junction") and path.is_junction())


def test_classes(root):
    """Only source-declared class names may be emitted as suite hints."""
    source = root / "src" / "test"
    if any(is_link(p) for p in (root / "src", source)):
        return set()
    names = set()
    count = 0
    for directory, dirs, files in os.walk(source, followlinks=False):
        dirs[:] = [d for d in dirs if not is_link(Path(directory) / d)]
        for name in files:
            path = Path(directory) / name
            if not name.endswith(".scala") or is_link(path):
                continue
            count += 1
            if count > 2000:
                return set()
            try:
                if path.stat().st_size <= MAX_REPORT_BYTES:
                    names.update(CLASS_DECL.findall(path.read_text(encoding="utf-8")))
            except (OSError, UnicodeError):
                continue
    return names


def parse_report(text, classes):
    lines = text.splitlines()
    header = "\n".join(lines[:24])
    fatal = re.search(
        r"^#\s+(SIG[A-Z]+)\s+\([^)]+\).*?\bpid=([1-9][0-9]{0,9})\b", header, re.M
    )
    if ("A fatal error has been detected by the Java Runtime Environment" not in header
            or not fatal or fatal[1] not in SIGNALS):
        return {"status": "unrecognized"}
    pid = int(fatal[2])
    result = {
        "status": "parsed",
        "signal": fatal[1],
        "pid": pid if 0 < pid <= 2147483647 else None,
        "java_major": None,
        "native_component": None,
        "native_operation": None,
        "suite_hint": None,
        "group_hint": None,
    }
    version = re.search(r"^# JRE version:.*?\(build (?:1\.)?(\d{1,2})(?:[._+)-])", header, re.M)
    if version and 1 <= int(version[1]) <= 99:
        result["java_major"] = int(version[1])
    frame = re.search(r"^# Problematic frame:\s*\n([^\n]*)", header, re.M)
    if frame and re.search(r"\blibleveldbjni[^]\s]*", frame[1]):
        result["native_component"] = "leveldbjni"
        direct = re.search(r"\bJava_org_fusesource_leveldbjni_internal_NativeDB_00024DBJNI_", frame[1])
        if direct:
            method = re.match(r"(delete|Get)(?:\+|$)", frame[1][direct.end():])
            if method:
                result["native_operation"] = {"delete": "NativeDB.delete", "Get": "NativeDB.Get"}[method[1]]
        else:
            result["native_operation"] = current_java_operation(text)
    current = re.search(r'^Current thread \([^\n]*\):[^\n]*"([^"\n]*)"', text, re.M)
    if current:
        suite = re.search(r"ScalaTest-running-([A-Za-z][A-Za-z0-9_]*)\Z", current[1])
        if suite and suite[1] in classes:
            result["suite_hint"] = suite[1]
    arguments = re.search(r"^VM Arguments:\s*\n((?:[^\n]+\n?)*)", text, re.M)
    if arguments:
        jvm = re.search(r"^jvm_args: ([^\n]*)", arguments[1], re.M)
        if jvm:
            values = re.findall(r'(?:^|\s)-Dergo\.directory=("[^"\n]*"|[^\s]+)', jvm[1])
            if len(values) == 1:
                group = re.search(r"[/\\]\.ergo_test[/\\]group-([0-2])[/\\]data\Z", values[0].strip('"'))
                if group:
                    result["group_hint"] = int(group[1])
    return result


def collect(root):
    root = Path(root)
    result = {
        "schema_version": 1,
        "scan_scope": ["checkout-root", "target-root"],
        "attribution": "hints_only",
        "status": "no_reports_in_scope",
        "reports": [],
        "report_limit_reached": False,
        "unavailable_scopes": [],
    }
    if is_link(root) or not root.is_dir():
        result["unavailable_scopes"] = result["scan_scope"]
        return result
    classes = test_classes(root)
    remaining = MAX_TOTAL_BYTES
    for label, scope in zip(result["scan_scope"], (root, root / "target")):
        if is_link(scope):
            result["unavailable_scopes"].append(label)
            continue
        if not scope.exists():
            continue
        try:
            with os.scandir(scope) as entries:
                for entry in entries:
                    if not REPORT_NAME.fullmatch(entry.name):
                        continue
                    result["status"] = "reports_found"
                    if len(result["reports"]) >= MAX_REPORTS:
                        result["report_limit_reached"] = True
                        return result
                    path = Path(entry.path)
                    if is_link(path):
                        result["reports"].append({"status": "skipped_link"})
                        continue
                    try:
                        before = path.stat(follow_symlinks=False)
                        if not stat.S_ISREG(before.st_mode):
                            report = {"status": "skipped_non_regular"}
                        elif before.st_size > min(MAX_REPORT_BYTES, remaining):
                            report = {"status": "oversized"}
                        else:
                            flags = os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0) | getattr(os, "O_BINARY", 0)
                            with os.fdopen(os.open(path, flags), "rb") as stream:
                                opened = os.fstat(stream.fileno())
                                if (before.st_dev, before.st_ino) != (opened.st_dev, opened.st_ino):
                                    raise OSError("report changed")
                                raw = stream.read(min(MAX_REPORT_BYTES, remaining) + 1)
                            if len(raw) > min(MAX_REPORT_BYTES, remaining):
                                report = {"status": "oversized"}
                            else:
                                remaining -= len(raw)
                                report = parse_report(raw.decode("utf-8", errors="replace"), classes)
                    except OSError:
                        report = {"status": "unreadable"}
                    result["reports"].append(report)
        except OSError:
            result["unavailable_scopes"].append(label)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    output = args.output
    if is_link(output) or any(is_link(p) for p in output.parents):
        parser.exit(1, "Refusing a linked output path.\n")
    try:
        summary = collect(args.root)
        output.parent.mkdir(parents=True, exist_ok=True)
        with output.open("x", encoding="utf-8") as stream:
            json.dump(summary, stream, sort_keys=True, indent=2)
            stream.write("\n")
    except OSError:
        parser.exit(1, "Could not write the crash summary.\n")


if __name__ == "__main__":
    main()
