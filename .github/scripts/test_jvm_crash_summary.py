import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


SCRIPT = Path(__file__).with_name("jvm_crash_summary.py")
SPEC = importlib.util.spec_from_file_location("jvm_crash_summary", SCRIPT)
summary = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(summary)

REPORT = """# A fatal error has been detected by the Java Runtime Environment:
#  SIGSEGV (0xb) at pc=0x0001, pid=2514, tid=0x0002
# JRE version: OpenJDK Runtime Environment (8.0_504-b01) (build 1.8.0_504-b01)
# Problematic frame:
# C  [libleveldbjni-64.so+0x001] Java_org_fusesource_leveldbjni_internal_NativeDB_00024DBJNI_delete+0xb

Current thread (0x01): JavaThread "pool-1-ScalaTest-running-ExampleSpec" [_thread_in_native]

VM Arguments:
jvm_args: -Dergo.directory=/checkout/.ergo_test/group-2/data -Dother=PRIVATE_SENTINEL

Environment Variables:
IGNORED_VALUE=PRIVATE_SENTINEL
"""


class CrashSummaryTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        source = self.root / "src/test/scala/ExampleSpec.scala"
        source.parent.mkdir(parents=True)
        source.write_text("package example\nclass ExampleSpec extends AnyFunSuite\n", encoding="utf-8")

    def report(self, text=REPORT, name="hs_err_pid2514.log"):
        path = self.root / name
        path.write_text(text, encoding="utf-8")
        return path

    def test_known_report_contains_only_allowlisted_hints(self):
        self.report()
        result = summary.collect(self.root)
        self.assertEqual(result["status"], "reports_found")
        self.assertEqual(result["reports"], [{
            "status": "parsed", "signal": "SIGSEGV", "pid": 2514, "java_major": 8,
            "native_component": "leveldbjni", "native_operation": "NativeDB.delete",
            "suite_hint": "ExampleSpec", "group_hint": 2,
        }])
        encoded = json.dumps(result)
        for private in ("PRIVATE_SENTINEL", "/checkout", "hs_err_pid2514", "jvm_args", "IGNORED_VALUE"):
            self.assertNotIn(private, encoded)

    def test_missing_report_is_scoped_absence(self):
        result = summary.collect(self.root)
        self.assertEqual(result["status"], "no_reports_in_scope")
        self.assertEqual(result["reports"], [])
        self.assertEqual(result["scan_scope"], ["checkout-root", "target-root"])
        self.assertEqual(result["attribution"], "hints_only")

    def test_unknown_data_is_not_copied(self):
        self.report(REPORT.replace("ExampleSpec", "PRIVATE_SENTINEL")
                    .replace("SIGSEGV", "SIGUNKNOWN"))
        self.assertEqual(summary.collect(self.root)["reports"], [{"status": "unrecognized"}])
        self.report(REPORT.replace("ExampleSpec", "PRIVATE_SENTINEL")
                    .replace("libleveldbjni", "PRIVATE_SENTINEL")
                    .replace("group-2/data", "PRIVATE_SENTINEL"))
        result = summary.collect(self.root)
        self.assertIsNone(result["reports"][0]["suite_hint"])
        self.assertIsNone(result["reports"][0]["native_component"])
        self.assertIsNone(result["reports"][0]["group_hint"])
        self.assertNotIn("PRIVATE_SENTINEL", json.dumps(result))

    def test_other_thread_and_environment_are_not_used(self):
        text = REPORT.replace('Current thread (0x01): JavaThread "pool-1-ScalaTest-running-ExampleSpec"',
                              'Current thread (0x01): JavaThread "native-worker"')
        text += '\nOther thread: "pool-1-ScalaTest-running-ExampleSpec"\n'
        text = text.replace("-Dergo.directory=/checkout/.ergo_test/group-2/data", "-Dother=value")
        text += "\n-Dergo.directory=/checkout/.ergo_test/group-2/data\n"
        self.report(text)
        result = summary.collect(self.root)["reports"][0]
        self.assertIsNone(result["suite_hint"])
        self.assertIsNone(result["group_hint"])

    def test_leveldb_cpp_frame_uses_first_java_frame_only(self):
        cpp = REPORT.replace("Java_org_fusesource_leveldbjni_internal_NativeDB_00024DBJNI_delete+0xb",
                             "leveldb::PosixEnv::UnlockFile+0xb")
        for prefix, method, expected in (("j  ", "delete", "NativeDB.delete"),
                                          ("J  123 ", "Get", "NativeDB.Get"),
                                          ("J  123% c1 ", "delete", "NativeDB.delete"),
                                          ("J  123 c2 ", "Get", "NativeDB.Get")):
            with self.subTest(prefix=prefix, method=method):
                text = cpp.replace("\nVM Arguments:",
                    "\nNative frames: (C=native code)\nC  [libleveldbjni.so] PRIVATE_SENTINEL\n\n"
                    "Java frames: (J=compiled Java code, j=interpreted)\n"
                    f"{prefix}org.fusesource.leveldbjni.internal.NativeDB$DBJNI.{method}(J)V+0\n"
                    "j  PRIVATE_SENTINEL.method()V+0\n\nVM Arguments:")
                result = summary.parse_report(text, set())
                self.assertEqual(result["native_operation"], expected)
                self.assertNotIn("PRIVATE_SENTINEL", json.dumps(result))

    def test_fallback_rejects_decoys_and_ambiguous_sections(self):
        cpp = REPORT.replace("Java_org_fusesource_leveldbjni_internal_NativeDB_00024DBJNI_delete+0xb",
                             "leveldb::MemTable::KeyComparator+0xb")
        good = "j  org.fusesource.leveldbjni.internal.NativeDB$DBJNI.Get(J)V+0\n"
        cases = [
            "j  example.Decoy.run()V+0\n" + good,
            "j  org.fusesource.leveldbjni.internal.NativeDB$DBJNI.GetMore(J)V+0\n",
            "j  example.org.fusesource.leveldbjni.internal.NativeDB$DBJNI.Get(J)V+0\n",
            "j  org.fusesource.leveldbjni.internal.NativeDB$DBJNI.other(J)V+0\n" + good,
            "J  123 c2 example.Decoy.run()V+0\n" + good,
            "J  123 c3 org.fusesource.leveldbjni.internal.NativeDB$DBJNI.Get(J)V+0\n",
            "J  12345678901 c1 org.fusesource.leveldbjni.internal.NativeDB$DBJNI.Get(J)V+0\n",
            "j  123 c1 org.fusesource.leveldbjni.internal.NativeDB$DBJNI.Get(J)V+0\n",
            "J  org.fusesource.leveldbjni.internal.NativeDB$DBJNI.Get(J)V+0\n",
        ]
        for frames in cases:
            with self.subTest(frames=frames):
                text = cpp.replace("\nVM Arguments:", "\nJava frames:\n" + frames + "\nVM Arguments:")
                self.assertIsNone(summary.parse_report(text, set())["native_operation"])
        other = cpp + "\nOther thread: worker\nJava frames:\n" + good
        self.assertIsNone(summary.parse_report(other, set())["native_operation"])
        duplicate = cpp.replace("\nVM Arguments:", "\nJava frames:\n" + good + "\nJava frames:\n" + good + "\nVM Arguments:")
        self.assertIsNone(summary.parse_report(duplicate, set())["native_operation"])
        valid = cpp.replace("\nVM Arguments:", "\nJava frames:\n" + good + "\nVM Arguments:")
        self.assertIsNone(summary.parse_report(valid.replace("libleveldbjni", "libother"), set())["native_operation"])
        two_threads = valid.replace("\nVM Arguments:", "\nCurrent thread (0x02): JavaThread \"other\"\n\nVM Arguments:")
        self.assertIsNone(summary.parse_report(two_threads, set())["native_operation"])

    def test_direct_symbol_precedes_java_fallback(self):
        text = REPORT.replace("\nVM Arguments:", "\nJava frames:\n"
                              "j  org.fusesource.leveldbjni.internal.NativeDB$DBJNI.Get(J)V+0\n\nVM Arguments:")
        self.assertEqual(summary.parse_report(text, set())["native_operation"], "NativeDB.delete")
        direct_get = text.replace("DBJNI_delete+0xb", "DBJNI_Get+0xb").replace(
            "$DBJNI.Get(J)V+0", "$DBJNI.delete(J)V+0")
        self.assertEqual(summary.parse_report(direct_get, set())["native_operation"], "NativeDB.Get")
        unknown = direct_get.replace("DBJNI_Get+0xb", "DBJNI_PRIVATE_SENTINEL+0xb")
        result = summary.parse_report(unknown, set())
        self.assertIsNone(result["native_operation"])
        self.assertNotIn("PRIVATE_SENTINEL", json.dumps(result))

    def test_file_and_total_byte_limits(self):
        self.report()
        with patch.object(summary, "MAX_REPORT_BYTES", 10):
            self.assertEqual(summary.collect(self.root)["reports"], [{"status": "oversized"}])
        written = self.report(name="hs_err_pid2515.log")
        with patch.object(summary, "MAX_TOTAL_BYTES", written.stat().st_size):
            results = summary.collect(self.root)["reports"]
            self.assertEqual(sum(r["status"] == "parsed" for r in results), 1)
            self.assertEqual(sum(r["status"] == "oversized" for r in results), 1)

    def test_report_count_limit(self):
        for i in range(3):
            self.report(name=f"hs_err_pid{i}.log")
        with patch.object(summary, "MAX_REPORTS", 2):
            result = summary.collect(self.root)
            self.assertEqual(len(result["reports"]), 2)
            self.assertTrue(result["report_limit_reached"])

    def test_target_is_scanned_but_not_descendants(self):
        target = self.root / "target"
        target.mkdir()
        (target / "hs_err_pid1.log").write_text(REPORT, encoding="utf-8")
        nested = target / "nested"
        nested.mkdir()
        (nested / "hs_err_pid2.log").write_text(REPORT, encoding="utf-8")
        self.assertEqual(len(summary.collect(self.root)["reports"]), 1)

    def make_link(self, link, target, directory=False):
        try:
            link.symlink_to(target, target_is_directory=directory)
        except (OSError, NotImplementedError):
            self.skipTest("Symbolic links unavailable on this host")

    def test_report_link_is_rejected(self):
        outside = self.root / "outside.log"
        outside.write_text(REPORT, encoding="utf-8")
        self.make_link(self.root / "hs_err_pid1.log", outside)
        self.assertEqual(summary.collect(self.root)["reports"], [{"status": "skipped_link"}])

    def test_target_link_is_not_traversed(self):
        outside = self.root / "elsewhere"
        outside.mkdir()
        (outside / "hs_err_pid1.log").write_text(REPORT, encoding="utf-8")
        self.make_link(self.root / "target", outside, directory=True)
        result = summary.collect(self.root)
        self.assertEqual(result["reports"], [])
        self.assertEqual(result["unavailable_scopes"], ["target-root"])

    def test_unreadable_report_status_has_no_exception_text(self):
        self.report()
        with patch("os.open", side_effect=PermissionError("PRIVATE_SENTINEL")):
            result = summary.collect(self.root)
        self.assertEqual(result["reports"], [{"status": "unreadable"}])
        self.assertNotIn("PRIVATE_SENTINEL", json.dumps(result))

    def test_cli_writes_json_and_rejects_existing_output(self):
        self.report()
        output = self.root / "target/summary.json"
        command = [sys.executable, "-B", str(SCRIPT), "--root", str(self.root), "--output", str(output)]
        first = subprocess.run(command, capture_output=True, text=True)
        self.assertEqual(first.returncode, 0, first.stderr)
        self.assertEqual(json.loads(output.read_text())["reports"][0]["suite_hint"], "ExampleSpec")
        original = output.read_bytes()
        second = subprocess.run(command, capture_output=True, text=True)
        self.assertNotEqual(second.returncode, 0)
        self.assertEqual(output.read_bytes(), original)

    def test_cli_does_not_follow_output_link(self):
        outside = self.root / "outside.json"
        outside.write_text("sentinel", encoding="utf-8")
        output = self.root / "summary.json"
        self.make_link(output, outside)
        result = subprocess.run(
            [sys.executable, "-B", str(SCRIPT), "--root", str(self.root), "--output", str(output)],
            capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(outside.read_text(), "sentinel")


if __name__ == "__main__":
    unittest.main()
