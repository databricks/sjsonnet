#!/usr/bin/env python3

import os
import sys
import subprocess
import glob
import unittest
from pathlib import Path
from typing import List, Tuple, Dict, Optional

def strip_trailing_empty_lines(text: str) -> str:
  """Remove trailing empty lines from text."""
  lines = text.splitlines()
  while lines and not lines[-1].strip():
    lines.pop()
  return '\n'.join(lines)

root_dir = os.getcwd()

def run_binary(binary_path: str, jsonnet_file: str) -> Tuple[str, int]:
  """Run the binary on a jsonnet file and return output and exit code."""
  cmd = [
    binary_path,
    '--ext-str', 'var1=test',
    '--ext-code', 'var2=local f(a, b) = {[a]: b, \"y\": 2}; f(\"x\", 1)',
    '--ext-str', 'stringVar=2 + 2',
    '--ext-code', 'codeVar=3 + 3',
    '--ext-code', 'errorVar=error \'xxx\'',
    '--ext-code', 'staticErrorVar=)',
    '--ext-code', 'UndeclaredX=x',
    '--ext-code', 'selfRecursiveVar=[42, std.extVar(\"selfRecursiveVar\")[0] + 1]',
    '--ext-code', 'mutuallyRecursiveVar1=[42, std.extVar(\"mutuallyRecursiveVar2\")[0] + 1]',
    '--ext-code', 'mutuallyRecursiveVar2=[42, std.extVar(\"mutuallyRecursiveVar1\")[0] + 1]',
    '--tla-str', 'var1=test',
    '--tla-code', 'var2={\"x\": 1, \"y\": 2}',
    jsonnet_file]
  try:
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
    return result.stdout + result.stderr, result.returncode
  except subprocess.TimeoutExpired:
    return "TIMEOUT: Test execution exceeded 10 seconds", 1
  except Exception as e:
    return f"ERROR: Failed to execute binary: {e}", 1

class BaseGraalVMTestSuite(unittest.TestCase):
  """Base class for GraalVM test suites."""
  
  @classmethod
  def setUpClass(cls):
    """Set up the test class by building the GraalVM native binary."""
    if not hasattr(cls, '_binary_built'):      
      subprocess.run(["./mill", "sjsonnet.graal.nativeImage"], check=True, cwd=".")
      BaseGraalVMTestSuite._binary_built = True
    cls.binary_path = os.path.join(root_dir, "out/sjsonnet/graal/nativeImage.dest/native-executable")
  
  @classmethod
  def tearDownClass(cls):
    """Clean up the test class by removing the GraalVM native binary."""
    os.chdir(root_dir)

  def run_individual_test(self, jsonnet_file: str, base_name: str):
    """Run a single jsonnet test and assert it passes."""
    # Run the binary on the jsonnet file and capture output
    output, exit_code = run_binary(self.binary_path, jsonnet_file)
    with open(f"{base_name}.jsonnet.golden", 'r', encoding='utf-8') as f:
      golden_content = f.read()
    
    normalized_output = output.strip()
    normalized_golden_content = golden_content.strip()
    if jsonnet_file.endswith("trace.jsonnet"):
      normalized_output = normalized_output.replace("true", "").strip()
      normalized_golden_content = normalized_golden_content.replace("true", "").strip()
    
    # Compare with golden file, ignoring trailing empty lines
    if normalized_golden_content.startswith("""Exception in thread "main" java.lang.StackOverflowError""") and normalized_output.startswith("""Exception in thread "main" java.lang.StackOverflowError"""):
      return
    elif len(normalized_golden_content) > 10000 and normalized_golden_content != normalized_output:
      print(f"normalized_golden_content: {normalized_golden_content[:100]}")
      print(f"normalized_output: {normalized_output[:100]}")
      self.fail("Mismatch - but content very very large")
    self.assertEqual(normalized_golden_content, normalized_output)


class MainTestSuite(BaseGraalVMTestSuite):
  """Test suite for the main jsonnet test files."""

  @classmethod
  def setUpClass(cls):
    super(MainTestSuite, cls).setUpClass()
    os.chdir(os.path.join(root_dir, "sjsonnet/test/resources/test_suite"))
  
  def test_all_files(self):
    """Test all files in the main test suite using subTest for each file."""
    # Skip list for main test suite
    skip_list = []
    
    # Find all .jsonnet files in the test directory
    jsonnet_files = glob.glob("*.jsonnet")
    self.assertTrue(jsonnet_files, f"No .jsonnet files found in {os.getcwd()}")
            
    for jsonnet_file in sorted(jsonnet_files):
      base_name = Path(jsonnet_file).stem
      
      # Check if this test should be skipped
      if base_name in skip_list:
        continue
      
      with self.subTest(file=base_name):
        self.run_individual_test(jsonnet_file, base_name)

class GoTestSuite(BaseGraalVMTestSuite):
  """Test suite for the Go jsonnet test files."""

  @classmethod
  def setUpClass(cls):
    super(GoTestSuite, cls).setUpClass()
    os.chdir(os.path.join(root_dir, "sjsonnet/test/resources/go_test_suite"))
  
  def test_all_files(self):
    """Test all files in the go test suite using subTest for each file."""
    skip_list = [
      # GraalVM has slight difference with how it manages high precision floats
      "builtin_cos",
      "builtin_exp4",
      "builtin_log3",
      "div3",
      "std.mantissa3",
      "stdlib_smoke_test",

      # These tests rely on custom native functions that are not implemented in the binary we use
      "native1",
      "native2",
      "native3",
      "native4",
      "native5",
      "native6",
      "native7",
      "native_error",
      "native_panic",
    ]
    
    # Find all .jsonnet files in the test directory
    jsonnet_files = glob.glob("*.jsonnet")
    self.assertTrue(jsonnet_files, f"No .jsonnet files found in {os.getcwd()}")
            
    for jsonnet_file in sorted(jsonnet_files):
      base_name = Path(jsonnet_file).stem
      
      # Check if this test should be skipped
      if base_name in skip_list:
        continue

      with self.subTest(file=base_name):
        self.run_individual_test(jsonnet_file, base_name)

class NewTestSuite(BaseGraalVMTestSuite):
  """Test suite for the new jsonnet test files."""

  @classmethod
  def setUpClass(cls):
    super(NewTestSuite, cls).setUpClass()
    os.chdir(os.path.join(root_dir, "sjsonnet/test/resources/new_test_suite"))

  def test_all_files(self):
    """Test all files in the main test suite using subTest for each file."""
    # Skip list for main test suite
    skip_list = []

    # Find all .jsonnet files in the test directory
    jsonnet_files = glob.glob("*jvm-native.jsonnet")
    self.assertTrue(jsonnet_files, f"No .jsonnet files found in {os.getcwd()}")

    for jsonnet_file in sorted(jsonnet_files):
      base_name = Path(jsonnet_file).stem

      # Check if this test should be skipped
      if base_name in skip_list:
        continue

      with self.subTest(file=base_name):
        self.run_individual_test(jsonnet_file, base_name)

if __name__ == "__main__":
  unittest.main(verbosity=2)
