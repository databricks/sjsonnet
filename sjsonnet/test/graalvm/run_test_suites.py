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


def run_binary(binary_path: str, test_dir: str, jsonnet_file: str) -> Tuple[str, int]:
  """Run the binary on a jsonnet file and return output and exit code."""
  cmd = [binary_path, '-Xss100m', '-J', test_dir, jsonnet_file]
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
    cls.binary_path = "out/sjsonnet/graal/nativeImage.dest/native-executable"
  
  def run_individual_test(self, test_dir: str, suite_name: str, jsonnet_file: str, base_name: str):
    """Run a single jsonnet test and assert it passes."""
    # Run the binary on the jsonnet file and capture output
    output, exit_code = run_binary(self.binary_path, test_dir, jsonnet_file)
    with open(os.path.join(test_dir, f"{base_name}.jsonnet.golden"), 'r', encoding='utf-8') as f:
      golden_content = f.read()
    
    # Strip test directory path from output only for go_test_suite
    if suite_name == "go_test_suite":
      normalized_output = strip_trailing_empty_lines(output.replace(f"{test_dir}/", ""))
      normalized_golden_content = strip_trailing_empty_lines(golden_content.replace(f"{test_dir}/", ""))
    else:
      normalized_output = strip_trailing_empty_lines(output)
      normalized_golden_content = strip_trailing_empty_lines(golden_content)
    
    # Compare with golden file, ignoring trailing empty lines
    if len(normalized_golden_content) > 10000 and normalized_golden_content != normalized_output:
      print(f"normalized_golden_content: {normalized_golden_content[:100]}")
      print(f"normalized_output: {normalized_output[:100]}")
      self.fail("Mismatch - but content very very large")
    self.assertEqual(normalized_golden_content, normalized_output)


class MainTestSuite(BaseGraalVMTestSuite):
  """Test suite for the main jsonnet test files."""
  
  def test_all_files(self):
    """Test all files in the main test suite using subTest for each file."""
    test_dir = "sjsonnet/test/resources/test_suite"
    suite_name = "test_suite"
    
    if not os.path.exists(test_dir):
      self.fail(f"Test directory {test_dir} not found")
    
    # Skip list for main test suite
    skip_list = [
      "error.obj_recursive_manifest",
      "error.recursive_object_non_term",
      "error.recursive_import",
      "error.recursive_function_nonterm",
      "error.function_infinite_default",
      "error.obj_recursive",
      "error.array_recursive_manifest",
      "error.function_no_default_arg",
      "error.invariant.option",
      "error.negative_shfit",
      "error.overflow",
      "error.overflow2",
      "error.overflow3",
      "error.parse_json",
      "error.parse.string.invalid_escape",
      "error.top_level_func",
      "stdlib",
      "tla.simple",
      "trace"
    ]
    
    # Find all .jsonnet files in the test directory
    jsonnet_pattern = os.path.join(test_dir, "*.jsonnet")
    jsonnet_files = glob.glob(jsonnet_pattern)
    
    self.assertTrue(jsonnet_files, f"No .jsonnet files found in {test_dir}")
            
    for jsonnet_file in sorted(jsonnet_files):
      base_name = Path(jsonnet_file).stem
      
      # Check if this test should be skipped
      if base_name in skip_list:
        continue
      
      with self.subTest(file=base_name):
        self.run_individual_test(test_dir, suite_name, jsonnet_file, base_name)

class GoTestSuite(BaseGraalVMTestSuite):
  """Test suite for the Go jsonnet test files."""
  
  def test_all_files(self):
    """Test all files in the go test suite using subTest for each file."""
    test_dir = "sjsonnet/test/resources/go_test_suite"
    suite_name = "go_test_suite"
    
    if not os.path.exists(test_dir):
      self.fail(f"Test directory {test_dir} not found")
    
    # Skip list for go_test_suite
    skip_list = [
      "builtin_cos",
      "builtin_exp4", 
      "builtin_log3",
      "div3",
      "extvar_code",
      "extvar_error",
      "extvar_hermetic",
      "extvar_mutually_recursive",
      "extvar_self_recursive",
      "extvar_static_error",
      "extvar_string",
      "function_too_many_params",
      "native1",
      "native2",
      "native3",
      "native4",
      "native5",
      "native6",
      "native7",
      "native_error",
      "native_panic",
      "std.mantissa3",
      "stdlib_smoke_test"
    ]
    
    # Find all .jsonnet files in the test directory
    jsonnet_pattern = os.path.join(test_dir, "*.jsonnet")
    jsonnet_files = glob.glob(jsonnet_pattern)
    
    self.assertTrue(jsonnet_files, f"No .jsonnet files found in {test_dir}")
            
    for jsonnet_file in sorted(jsonnet_files):
      base_name = Path(jsonnet_file).stem
      
      # Check if this test should be skipped
      if base_name in skip_list:
        continue

      with self.subTest(file=base_name):
        self.run_individual_test(test_dir, suite_name, jsonnet_file, base_name)


if __name__ == "__main__":
  unittest.main(verbosity=2)
