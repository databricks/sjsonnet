## 0.5.5.1 (Latest)
* Re-enable handling of unicode strings in std.base64

## 0.5.5
* Fix multiple assertion related bugs (inheritance, binding of super in inherited assertions, and timing of evaluation) by @JoshRosen in https://github.com/databricks/sjsonnet/pull/468
* Fix detection of duplicate fields with dynamic field name expressions by @JoshRosen in https://github.com/databricks/sjsonnet/pull/470
* chore: Make std.setInter accepts str. by @He-Pin in https://github.com/databricks/sjsonnet/pull/472
* chore: Make std.setDiff accepts str. by @He-Pin in https://github.com/databricks/sjsonnet/pull/478
* chore: Make std.setUnion accept str. by @He-Pin in https://github.com/databricks/sjsonnet/pull/480
* Add graalvm build and test suite - experimental by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/482

## 0.5.4
* Upgrade to mill 1.0.2
* Fix parsing error with numbers causing whitespaces to be consumed. by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/457
* Fix parseInt to properly understand longs by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/458

## 0.5.3
* chore: Extract Util#hashMapCapacity method. by @He-Pin in https://github.com/databricks/sjsonnet/pull/412
* chore: Fix some IDE lint problems by @He-Pin in https://github.com/databricks/sjsonnet/pull/413
* Add go-jsonnet test_suite to sjsonnet by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/417
* Enable additional math tests cases by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/418
* Add more test coverage for std.ExtVar. by @Danstahrm in https://github.com/databricks/sjsonnet/pull/402
* Remove strictImport and duplicateKeysInComprehension flags. by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/419
* Remove --no-static-error, --strict-inherited-assertions & --strict-set-operations by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/420
* Remove SBT and consolidate build to mill by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/421
* chore: Add missing version for maven in readme. by @He-Pin in https://github.com/databricks/sjsonnet/pull/423
* chore: bump scala-native to 0.5.8 by @He-Pin in https://github.com/databricks/sjsonnet/pull/422
* Implement md5/sha (except sha3) for the native build by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/424
* Properly implement file hashing in scala native - using the built-in murmurhash by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/425
* Add support for parseYaml in native and js by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/427
* Additional fixes to the parser based on the go test suite by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/428
* chore: bump scala fmt version to 3.9.7 by @He-Pin in https://github.com/databricks/sjsonnet/pull/429
* Update ScalaJS entrypoint and documentation by @LaurelineSwissBorg in https://github.com/databricks/sjsonnet/pull/426
* fix: Imports inside --tla-code-file,--ext-code-file. by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/432
* Add a flag to reverse the order of jpath entries. by @Danstahrm in https://github.com/databricks/sjsonnet/pull/400
* fix: Support std.sort() on arrays of arrays. by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/434
* Fix ||| block parsing by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/433
* chore: tweak tripleBar parsing. by @He-Pin in https://github.com/databricks/sjsonnet/pull/435
* Fix additional discrepancies found when importing go-jsonnet tests. by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/436
* chore: Add test for std.removeAt which should not modify the parameter. by @He-Pin in https://github.com/databricks/sjsonnet/pull/437
* Remove std.strReplaceAll, which is not standard by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/440
* add yaml tests added in go-jsonnet by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/441
* Refactor std.native functions out of Std by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/442
* fix: Handling of special characters in std.parseYaml(). by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/438
* Remove last differences between jvm and native in regard to the go testsuite by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/443
* Fix some additional discrepancies between the implementations by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/444
* fix: Indentation of empty lines in YamlRenderer. by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/445
* Fix benchmark suite by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/446
* fix(js,native): std.parseYaml() handling of "---". by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/447
* fix(jvm): Let the importer read any path that exists and is not a directory. by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/451
* fix: Implement --string when --multi is not set. by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/452
* feat(jvm,native): Special-case "/dev/stdin" and "-" in TLAs, extVars, top-level file. by @rudo-thomas in https://github.com/databricks/sjsonnet/pull/449

## 0.5.2
* Fix readme to publish and allow for server/client to be pushed to maven by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/389
* chore: Update readme for std library by @He-Pin in https://github.com/databricks/sjsonnet/pull/390
* Add client/server to the build workflow by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/393
* chore: Turn off the multi threading support in scala native by @He-Pin in https://github.com/databricks/sjsonnet/pull/396
* chore: bump mill 0.12.14 by @He-Pin in https://github.com/databricks/sjsonnet/pull/397
* Allow empty labels for fmtstrings. by @Danstahrm in https://github.com/databricks/sjsonnet/pull/401
* chore: Change Scala native's release mode to ReleaseFull by @He-Pin in https://github.com/databricks/sjsonnet/pull/395
* Allow `std.map` to operate on string args as well. by @Danstahrm in https://github.com/databricks/sjsonnet/pull/404
* fix(publish): fix publishing for js and native projects by @ngbinh in https://github.com/databricks/sjsonnet/pull/403
* Fix formatting for g/G and f when rounding the value by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/407
* Implement * in FormatSpec to dynamically insert width/precision by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/408
* chore: Uncomment missing `g` formatting test. by @He-Pin in https://github.com/databricks/sjsonnet/pull/409
* Fix various small issues found when I imported all recent error tests by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/410

## 0.5.1
* Fix multiple object comprehension bugs by @JoshRosen in https://github.com/databricks/sjsonnet/pull/358
* Update Scala 3 to 3.3.6 by @He-Pin in https://github.com/databricks/sjsonnet/pull/365
* feat: Allow negative start/end on std.slice by @He-Pin in https://github.com/databricks/sjsonnet/pull/367
* Add std.object*Ex methods by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/368
* fix: Fix foldr for str by @He-Pin in https://github.com/databricks/sjsonnet/pull/373
* chore: Remove unused `new` when create Val.Arr by @He-Pin in https://github.com/databricks/sjsonnet/pull/375
* chore: bump dependencies versions by @He-Pin in https://github.com/databricks/sjsonnet/pull/376
* Fix tailstrict implementation and add more tests by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/379
* correct safe integer range for bitwise ops by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/381
* Add --debug-importer flag to print out what the default importer is doing by @stephenamar-db in https://github.com/databricks/sjsonnet/pull/385

## 0.5.0
* Add support for Scala3
* Fix std.min/maxArray
* Add array compare
* Fix formatter for Array and Obj and add ErrorTests to JS
* Fix overly-eager evaluation of unmerged or removed target fields in std.mergePatch

### 0.4.15.1
* Various small bug fixes.

### 0.4.15
* Add support for `tailstrict` [#189](https://github.com/databricks/sjsonnet/issues/189).
* Bind the standard library to a `$std` variable and use it in desugared expressions [#249](https://github.com/databricks/sjsonnet/issues/249).
* Update re2j dependency.
* Add new Math functions to keep up with go-jsonnet.
* Implement |||- syntax.

### 0.4.14
* Fix a bug in new strict mode for set in `std.setUnion` [#242](https://github.com/databricks/sjsonnet/issues/242).
* Add support for Java 21 and dropped support for Java 11.
* Add `std.native` support, and move `std.xz/std.gzip` to native. Enable support for `std.gzip` in scala native.
* Optimization: `std.sort` should only evaluate keyF once per array element.
* Fix corner case for YAML parsing with primitive types [google/jsonnet/issues/1109](https://github.com/google/jsonnet/issues/1109).
* Add optional regex methods in `std.native` based on [google/jsonnet/pull/1039](https://github.com/google/jsonnet/pull/1039) and jrsonnet.
* Multiple bug fixes regarding handling of hidden fields in `std.mergePatch`.
* Add `importbin` support.
* Updated mill to 0.11.13 to fix publishing to Sonatype.
* Several performance optimizations, primarily aimed at reducing garbage object creation in common cases.


### 0.4.13
* Implemented every missing methods in `std`.
* Improved readability of stack traces when `std` methods are involved.
* Cleaned up default Main class to allow for repeated flags.
* Updated mill to 0.11.9 and added JDK17 build.
* Fixed sjsonnet handling of 64bits integers [#191](https://github.com/databricks/sjsonnet/issues/191)
* Stopped manifesting functions with no arguments in objects by default [#168](https://github.com/databricks/sjsonnet/issues/168)
* Fixed "Duplicate Local Variables in Object Scope" [#178](https://github.com/databricks/sjsonnet/issues/178)

### 0.4.12.1
* Fix a bug leading to the truncation of the data returned by `std.gzip` and `std.xz` [#221](https://github.com/databricks/sjsonnet/pull/221).

### 0.4.12
* Fix a bug introduced with 0.4.11 with synthetic paths [#215](https://github.com/databricks/sjsonnet/pull/215)
* Fix thread-safety bug in Obj.getAllKeys [#217](https://github.com/databricks/sjsonnet/pull/217)

### 0.4.11
* Implement `std.isEmpty`, `std.xor`, `std.xnor`, `std.trim`,
  `std.equalsIgnoreCase`, `std.sha1`, `std.sha256`, `std.sha512`, `std.sha3` [#204](https://github.com/databricks/sjsonnet/pull/210)
* fix: std.manifestJsonMinified and empty arrays/objects [#207](https://github.com/databricks/sjsonnet/pull/207)
* fix: Use different chars for synthetic paths. [#208](https://github.com/databricks/sjsonnet/pull/208)
* Fix sorting algorithm to work for all array types [#211](https://github.com/databricks/sjsonnet/pull/211)
* Add better error handling for format [#212](https://github.com/databricks/sjsonnet/pull/212)

### 0.4.10

* Implement `std.get` [#202](https://github.com/databricks/sjsonnet/pull/202),
  `std.all` and `std.any` [#203](https://github.com/databricks/sjsonnet/pull/203)

### 0.4.9

* Switch from CRC32 to XXHash64 for import cache keys [#198](https://github.com/databricks/sjsonnet/pull/198)

### 0.4.8

* Significant reduction in memory usage from importing and parsing large files [#194](https://github.com/databricks/sjsonnet/pull/194) [#197](https://github.com/databricks/sjsonnet/pull/197)

### 0.4.7

* Ensure almost-octal-number-like strings are quoted when `--yaml-out` is passed,
  to avoid issues with non-compliant YAML parsers [#183](https://github.com/databricks/sjsonnet/pull/183)

### 0.4.6

* Add `std.parseYaml`

### 0.4.5

* Make Jsonnet standard library configurable and non-global
  [#166](https://github.com/databricks/sjsonnet/pull/166), fixing a race condition
  on library initialization in multithreaded environments and allowing custom `std.*`
  functions to be passed in by the user

* Added a flag `--no-duplicate-keys-in-comprehension` to follow upstream google/jsonnet
  behavior of failing if a dictionary comprehension
  [#156](https://github.com/databricks/sjsonnet/pull/156)
  [ea8720f](https://github.com/databricks/sjsonnet/commit/ea8720f218a4765b64e6313ac9f4a84d99c6e315).
  This is optional for migration purposes but will likely become the default in future

* Disallow Jsonnet identifiers that start with numbers [#161](https://github.com/databricks/sjsonnet/pull/161)

* Fix parsing of `+:` in dictionary comprehensions [#155](https://github.com/databricks/sjsonnet/pull/155)

* Fix parse failure when passing `id == ...` to a function arguments
  [#151](https://github.com/databricks/sjsonnet/pull/151)

* Add a flag `--strict-import-syntax` to disallow the syntax `import "foo".bar`,
  when it should be `(import "foo").bar`, following upstream google/jsonnet.
  This is optional for migration purposes but will likely become the default in future
  [#153](https://github.com/databricks/sjsonnet/pull/153)
  [ccbe6e](https://github.com/databricks/sjsonnet/commit/ccbe6e3a3f8cfb661ab6fa733aff690a61e47022)

* Allow assertions to take non-string error messages [#170](https://github.com/databricks/sjsonnet/pull/170)

* Fix parsing of object keys starting with the prefix `assert`
  [#169](https://github.com/databricks/sjsonnet/pull/169)

* Properly propagate assertions during inheritance [#172](https://github.com/databricks/sjsonnet/pull/172),
  and add the flag `--strict-inherited-assertions` to fix a bug where assertions
  inherited from a shared object were only triggering once, rather than
  once-per-inheritor [95342](https://github.com/databricks/sjsonnet/commit/9534260fff4a50d29db379e307bce9a484790fa7).
  This is optional for migration purposes but will likely become the default in future

* Add `std.slice`, `std.manifestJsonMinified`, fix handling of numbers in
  `manifestXmlJsonml`, handling of code in `extCode`
  [#171](https://github.com/databricks/sjsonnet/pull/171)

* Add the ability to include code in `--tla-code` and `--tla-code-file`
  [#175](https://github.com/databricks/sjsonnet/pull/175)

* Add `std.reverse` [a425342](https://github.com/databricks/sjsonnet/commit/a42534244c6966d049f38167473339be899d11af)

* Fixes to main method handling of various combinations of `--exec`, `--yaml-out`,
  `-yaml-stream`, `--multi`, and `--output-file` [#174](https://github.com/databricks/sjsonnet/pull/174)

### 0.4.4

* Update Mill to 0.10.12
* Fix parsing of k/v cli arguments with an "=" in the value

### 0.4.2

* Make lazy initialization of static Val.Obj thread-safe [#136](https://github.com/databricks/sjsonnet/pull/136)
* Deduplicate strings in the parser [#137](https://github.com/databricks/sjsonnet/pull/137)
* Update the JS example [#141](https://github.com/databricks/sjsonnet/pull/141)

### 0.4.1

* Additional significant performance improvements [#119](https://github.com/databricks/sjsonnet/pull/119)
* Error handling fixes and improvements [#125](https://github.com/databricks/sjsonnet/pull/125)

### 0.4.0

* Performance improvements with lots of internal changes [#117](https://github.com/databricks/sjsonnet/pull/117)

### 0.3.3

* Bump uJson version to 1.3.7

### 0.3.2

* Bump uJson version to 1.3.0

### 0.3.1

* Avoid catching fatal exceptions during evaluation

### 0.3.0

* Add `--yaml-debug` flag to add source-line comments showing where each line of YAML came from [#105]()https://github.com/databricks/sjsonnet/pull/105
* Add `objectValues` and `objectVlauesAll` to stdlib [#104](https://github.com/databricks/sjsonnet/pull/104)

### 0.2.8

* Allow direct YAML output generation via `--yaml-out`
* Do not allow duplicate field in object when evaluating list list comprehension [#100](https://github.com/databricks/sjsonnet/pull/100)
* Fix compiler crash when '+' signal is true in a field declaration inside a list comprehension [#98](https://github.com/databricks/sjsonnet/pull/98)
* Fix error message for too many arguments with at least one named arg [#97](https://github.com/databricks/sjsonnet/pull/97)

### 0.2.7

* Streaming JSON output to disk for lower memory usage [#85](https://github.com/databricks/sjsonnet/pull/85)
* Static detection of duplicate fields [#86](https://github.com/databricks/sjsonnet/pull/86)
* Strict mode to disallow error-prone adjacent object literals [#88](https://github.com/databricks/sjsonnet/pull/88)

### 0.2.6

* Add `std.flatMap`, `std.repeat`, `std.clamp`, `std.member`, `std.stripChars`, `std.rstripChars`, `std.lstripChars`

### 0.2.4

* Add support for syntactical key ordering [#53](https://github.com/databricks/sjsonnet/pull/53)
* Bump dependency versions

### 0.2.2

* Bump verion of Scalatags, uPickle

### 0.1.9

* Bump version of FastParse

### 0.1.8

* Bump versions of OS-Lib, uJson, Scalatags

### 0.1.7

* Support std lib methods that take a key lambda [#40](https://github.com/databricks/sjsonnet/pull/40)
* Handle hex in unicode escaoes [#41](https://github.com/databricks/sjsonnet/pull/41)
* Add encodeUTF8, decodeUTF8 std lib methdos [#42](https://github.com/databricks/sjsonnet/pull/42)
* Properly fail on non-boolean conditionals [#44](https://github.com/databricks/sjsonnet/pull/44)
* Support YAML-steam output [#45](https://github.com/databricks/sjsonnet/pull/45)

### 0.1.6

* ~2x performance increase

### 0.1.5

* Javascript support, allowing Sjsonnet to be used in the browser or on
  Node.js
* Performance improvements

### 0.1.4

* Scala 2.13 support
* Performance improvements

### 0.1.3

* Add `std.mod`, `std.min` and `std.max`
* Performance improvements

### 0.1.2

* Improvements to error reporting when types do not match

### 0.1.1

* Performance improvements to the parser via upgrading to Fastparse 2.x

### 0.1.0

* First release
