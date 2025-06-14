/*
Copyright 2015 Google Inc. All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

// This file tests functions from the standard library (std.jsonnet and builtins).

// Can capture std from another file.
std.assertEqual((import 'lib/capture_std_func.libsonnet')().sqrt(4), 2) &&

// Each import has its own std.
std.assertEqual(
  local std = { sqrt: function(x) x };
  local lib = import 'lib/capture_std.libsonnet';
  lib.sqrt(4),
  2
) &&

// Now, test each std library function in turn.

std.assertEqual(std.makeArray(10, function(i) i + 1), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) &&
std.assertEqual(std.makeArray(0, function(i) null), []) &&

local assertClose(a, b) =
  local err =
    if b == 0 then
      a - b
    else
      if a / b - 1 > 0 then a / b - 1 else 1 - a / b;
  if err > 0.000005 then
    error 'Assertion failed (error ' + err + '). ' + a + ' !~ ' + b
  else
    true;

std.assertEqual(std.pow(3, 2), 9) &&
std.assertEqual(std.floor(10), 10) &&
std.assertEqual(std.floor(10.99999), 10) &&
std.assertEqual(std.ceil(10), 10) &&
std.assertEqual(std.ceil(10.99999), 11) &&
std.assertEqual(std.sqrt(0), 0) &&
std.assertEqual(std.sqrt(1), 1) &&
std.assertEqual(std.sqrt(9), 3) &&
std.assertEqual(std.sqrt(16), 4) &&
std.assertEqual(std.abs(33), 33) &&
std.assertEqual(std.abs(-33), 33) &&
std.assertEqual(std.abs(0), 0) &&

assertClose(std.sin(0.0 * std.pi), 0) &&
assertClose(std.sin(0.5 * std.pi), 1) &&
assertClose(std.sin(1.0 * std.pi), 0) &&
assertClose(std.sin(1.5 * std.pi), -1) &&
assertClose(std.sin(2.0 * std.pi), 0) &&
assertClose(std.cos(0.0 * std.pi), 1) &&
assertClose(std.cos(0.5 * std.pi), 0) &&
assertClose(std.cos(1.0 * std.pi), -1) &&
assertClose(std.cos(1.5 * std.pi), 0) &&
assertClose(std.cos(2.0 * std.pi), 1) &&
assertClose(std.tan(0), 0) &&
assertClose(std.tan(0.25 * std.pi), 1) &&
assertClose(std.asin(0), 0) &&
assertClose(std.acos(1), 0) &&
assertClose(std.asin(1), 0.5 * std.pi) &&
assertClose(std.acos(0), 0.5 * std.pi) &&
assertClose(std.atan(0), 0) &&
assertClose(std.atan2(1, 1), std.pi / 4) &&
assertClose(std.atan2(-1, 1), -std.pi / 4) &&
assertClose(std.atan2(1.2, -3.8), 2.835713782184941) &&  // arbitrary, done on a calculator
assertClose(std.deg2rad(0), 0) &&
assertClose(std.deg2rad(45), std.pi / 4) &&
assertClose(std.deg2rad(90), std.pi / 2) &&
assertClose(std.deg2rad(172), 3.0019663134302466) &&  // arbitrary, done on a calculator
assertClose(std.rad2deg(std.pi / 4), 45) &&
assertClose(std.rad2deg(std.pi / 2), 90) &&
assertClose(std.rad2deg(3.0019663134302466), 172) &&  // arbitrary, done on a calculator
assertClose(std.hypot(3, 4), 5) &&
assertClose(std.hypot(5, 12), 13) &&
assertClose(std.hypot(1, 1), std.sqrt(2)) &&
assertClose(std.log(std.exp(5)), 5) &&
assertClose(std.mantissa(1), 0.5) &&
assertClose(std.exponent(1), 1) &&
assertClose(std.mantissa(128), 0.5) &&
assertClose(std.exponent(128), 8) &&
assertClose(std.log2(std.pow(2, -5)), -5) &&
assertClose(std.log2(std.pow(2, 0)), 0) &&
assertClose(std.log2(std.pow(2, std.pi)), std.pi) &&
assertClose(std.log10(std.pow(10, -5)), -5) &&
assertClose(std.log10(std.pow(10, 0)), 0) &&
assertClose(std.log10(std.pow(10, std.pi)), std.pi) &&

std.assertEqual(std.clamp(-3, 0, 5), 0) &&
std.assertEqual(std.clamp(4, 0, 5), 4) &&
std.assertEqual(std.clamp(7, 0, 5), 5) &&

std.assertEqual(std.type(null), 'null') &&
std.assertEqual(std.type(true), 'boolean') &&
std.assertEqual(std.type(false), 'boolean') &&
std.assertEqual(std.type(0), 'number') &&
std.assertEqual(std.type(-1e10), 'number') &&
std.assertEqual(std.type([1, 2, 3]), 'array') &&
std.assertEqual(std.type([]), 'array') &&
std.assertEqual(std.type(function(x) x), 'function') &&
std.assertEqual(std.type({ x: 1, y: 2 }), 'object') &&
std.assertEqual(std.type({}), 'object') &&
std.assertEqual(std.type('fail'), 'string') &&
std.assertEqual(std.type('' + {}), 'string') &&

std.assertEqual(std.isString(''), true) &&
std.assertEqual(std.isBoolean(true), true) &&
std.assertEqual(std.isNumber(0), true) &&
std.assertEqual(std.isObject({}), true) &&
std.assertEqual(std.isArray([]), true) &&
std.assertEqual(std.isFunction(function() 0), true) &&

std.assertEqual(std.isString(null), false) &&
std.assertEqual(std.isBoolean(null), false) &&
std.assertEqual(std.isNumber(null), false) &&
std.assertEqual(std.isObject(null), false) &&
std.assertEqual(std.isArray(null), false) &&
std.assertEqual(std.isFunction(null), false) &&

std.assertEqual(std.member('foo', 'o'), true) &&
std.assertEqual(std.member('foo', 'f'), true) &&
std.assertEqual(std.member('foo', 'x'), false) &&
std.assertEqual(std.member([], 'o'), false) &&
std.assertEqual(std.member(['f'], 'o'), false) &&
std.assertEqual(std.member(['f', 'o', 'o'], 'o'), true) &&
std.assertEqual(std.member(['f', 'o', 'o'], 'f'), true) &&
std.assertEqual(std.member(['f', 'o', 'o'], 'g'), false) &&

std.assertEqual(std.count([true, false, false, true, true, true, false], true), 4) &&
std.assertEqual(std.count([true, false, false, true, true, true, false], false), 3) &&

std.assertEqual(std.filter(function(x) x % 2 == 0, [1, 2, 3, 4]), [2, 4]) &&
std.assertEqual(std.filter(function(x) false, [1, 2, 3, 4]), []) &&
std.assertEqual(std.filter(function(x) x, []), []) &&

std.assertEqual(std.objectHas({ x: 1, y: 2 }, 'x'), true) &&
std.assertEqual(std.objectHas({ x: 1, y: 2 }, 'z'), false) &&
std.assertEqual(std.objectHas({}, 'z'), false) &&

std.assertEqual(std.length('asdfasdf'), 8) &&
std.assertEqual(std.length([1, 4, 9, error 'foo']), 4) &&
std.assertEqual(std.length(function(x, y, z) error 'foo'), 3) &&
std.assertEqual(std.length({ x: 1, y: 2 }), 2) &&
std.assertEqual(std.length({ a: 1, b: 2, c: 0 } + { c: 3, d: error 'foo' }), 4) &&
std.assertEqual(std.length(''), 0) &&
std.assertEqual(std.length([]), 0) &&
std.assertEqual(std.length(function() error 'foo'), 0) &&
std.assertEqual(std.length({}), 0) &&

std.assertEqual(std.objectFields({}), []) &&
std.assertEqual(std.objectFields({ x: 1, y: 2 }), ['x', 'y']) &&
std.assertEqual(std.objectFields({ a: 1, b: 2, c: null, d: error 'foo' }), ['a', 'b', 'c', 'd']) &&
std.assertEqual(std.objectFields({ x: 1 } { x: 1 }), ['x']) &&
std.assertEqual(std.objectFields({ x: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectFields({ x: 1 } { x::: 1 }), ['x']) &&
std.assertEqual(std.objectFields({ x:: 1 } { x: 1 }), []) &&
std.assertEqual(std.objectFields({ x:: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectFields({ x:: 1 } { x::: 1 }), ['x']) &&
std.assertEqual(std.objectFields({ x::: 1 } { x: 1 }), ['x']) &&
std.assertEqual(std.objectFields({ x::: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectFields({ x::: 1 } { x::: 1 }), ['x']) &&

std.assertEqual(std.objectValues({}), []) &&
std.assertEqual(std.objectValues({ x: 1, y: 2 }), [1, 2]) &&
std.assertEqual(std.objectValues({ x: 1 } { x: 1 }), [1]) &&
std.assertEqual(std.objectValues({ x: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectValues({ x: 1 } { x::: 1 }), [1]) &&
std.assertEqual(std.objectValues({ x:: 1 } { x: 1 }), []) &&
std.assertEqual(std.objectValues({ x:: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectValues({ x:: 1 } { x::: 1 }), [1]) &&
std.assertEqual(std.objectValues({ x::: 1 } { x: 1 }), [1]) &&
std.assertEqual(std.objectValues({ x::: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectValues({ x::: 1 } { x::: 1 }), [1]) &&

std.assertEqual(std.objectKeysValues({}), []) &&
std.assertEqual(std.objectKeysValues({ x: 1, y: 2 }), [{ key: 'x', value: 1 }, { key: 'y', value: 2 }]) &&
std.assertEqual(std.objectKeysValues({ x: 1 } { x: 1 }), [{ key: 'x', value: 1 }]) &&
std.assertEqual(std.objectKeysValues({ x: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectKeysValues({ x: 1 } { x::: 1 }), [{ key: 'x', value: 1 }]) &&
std.assertEqual(std.objectKeysValues({ x:: 1 } { x: 1 }), []) &&
std.assertEqual(std.objectKeysValues({ x:: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectKeysValues({ x:: 1 } { x::: 1 }), [{ key: 'x', value: 1 }]) &&
std.assertEqual(std.objectKeysValues({ x::: 1 } { x: 1 }), [{ key: 'x', value: 1 }]) &&
std.assertEqual(std.objectKeysValues({ x::: 1 } { x:: 1 }), []) &&
std.assertEqual(std.objectKeysValues({ x::: 1 } { x::: 1 }), [{ key: 'x', value: 1 }]) &&

std.assertEqual(std.get({ x: 1, y: 2 }, 'x', 5), 1) &&
std.assertEqual(std.get({ x: 1, y: 2 }, 'z', 5), 5) &&
std.assertEqual(std.get({ x: 1, y: 2 }, 'z'), null) &&
std.assertEqual(std.get({ x::: 1, y::: 2 }, 'x', 5), 1) &&
std.assertEqual(std.get({ x::: 1, y::: 2 }, 'z', 5), 5) &&
std.assertEqual(std.get({ x::: 1, y::: 2 }, 'z'), null) &&
std.assertEqual(std.get({ x:: 1, y:: 2 }, 'x', 5), 1) &&
std.assertEqual(std.get({ x:: 1, y:: 2 }, 'x'), 1) &&
std.assertEqual(std.get({ x:: 1, y:: 2 }, 'z', 5), 5) &&
std.assertEqual(std.get({ x:: 1, y:: 2 }, 'z'), null) &&
std.assertEqual(std.get({}, 'z', 5), 5) &&
std.assertEqual(std.get({}, 'z'), null) &&
std.assertEqual(std.get({ x: 1, y: 2 }, 'x', 5, false), 1) &&
std.assertEqual(std.get({ x: 1, y: 2 }, 'z', 5, false), 5) &&
std.assertEqual(std.get({ x::: 1, y::: 2 }, 'x', 5, false), 1) &&
std.assertEqual(std.get({ x::: 1, y::: 2 }, 'z', 5, false), 5) &&
std.assertEqual(std.get({ x:: 1, y:: 2 }, 'x', 5, false), 5) &&
std.assertEqual(std.get({ x:: 1, y:: 2 }, 'z', 5, false), 5) &&
std.assertEqual(std.get({}, 'z', 5, false), 5) &&

std.assertEqual(std.toString({ a: 1, b: 2 }), '{"a": 1, "b": 2}') &&
std.assertEqual(std.toString({}), '{ }') &&
std.assertEqual(std.toString([1, 2]), '[1, 2]') &&
std.assertEqual(std.toString([]), '[ ]') &&
std.assertEqual(std.toString(null), 'null') &&
std.assertEqual(std.toString(true), 'true') &&
std.assertEqual(std.toString(false), 'false') &&
std.assertEqual(std.toString('str'), 'str') &&
std.assertEqual(std.toString(''), '') &&
std.assertEqual(std.toString([1, 2, 'foo']), '[1, 2, "foo"]') &&

std.assertEqual(std.substr('cookie', 1, 3), 'ook') &&
std.assertEqual(std.substr('cookie', 1, 0), '') &&
std.assertEqual(std.substr('cookie', 1, 15), 'ookie') &&
std.assertEqual(std.substr('cookie', 0, 5), 'cooki') &&
std.assertEqual(std.substr('cookie', 0, 6), 'cookie') &&
std.assertEqual(std.substr('cookie', 0, 15), 'cookie') &&
std.assertEqual(std.substr('cookie', 3, 1), 'k') &&
std.assertEqual(std.substr('cookie', 20, 1), '') &&
std.assertEqual(std.substr('cookie', 6, 1), '') &&


std.assertEqual(std.substr('ąę', 1, 1), 'ę') &&

std.assertEqual(std.startsWith('food', 'foo'), true) &&
std.assertEqual(std.startsWith('food', 'food'), true) &&
std.assertEqual(std.startsWith('food', 'foody'), false) &&
std.assertEqual(std.startsWith('food', 'wat'), false) &&

std.assertEqual(std.endsWith('food', 'ood'), true) &&
std.assertEqual(std.endsWith('food', 'food'), true) &&
std.assertEqual(std.endsWith('food', 'omgfood'), false) &&
std.assertEqual(std.endsWith('food', 'wat'), false) &&

std.assertEqual(std.stripChars(' test test test     ', ' '), 'test test test') &&
std.assertEqual(std.stripChars('aaabbbbcccc', 'ac'), 'bbbb') &&
std.assertEqual(std.stripChars('cacabbbbaacc', 'ac'), 'bbbb') &&
std.assertEqual(std.stripChars('', 'ac'), '') &&

std.assertEqual(std.lstripChars(' test test test     ', ' '), 'test test test     ') &&
std.assertEqual(std.lstripChars('aaabbbbcccc', 'ac'), 'bbbbcccc') &&
std.assertEqual(std.lstripChars('cacabbbbaacc', 'ac'), 'bbbbaacc') &&
std.assertEqual(std.lstripChars('', 'ac'), '') &&

std.assertEqual(std.rstripChars(' test test test     ', ' '), ' test test test') &&
std.assertEqual(std.rstripChars('aaabbbbcccc', 'ac'), 'aaabbbb') &&
std.assertEqual(std.rstripChars('cacabbbbaacc', 'ac'), 'cacabbbb') &&
std.assertEqual(std.rstripChars('', 'ac'), '') &&

std.assertEqual(std.codepoint('a'), 97) &&
std.assertEqual(std.char(97), 'a') &&
std.assertEqual(std.codepoint('\u0000'), 0) &&
std.assertEqual(std.char(0), '\u0000') &&

std.assertEqual(std.strReplace('A cat walked by', 'cat', 'dog'), 'A dog walked by') &&
std.assertEqual(std.strReplace('cat', 'cat', 'dog'), 'dog') &&
std.assertEqual(std.strReplace('', 'cat', ''), '') &&
std.assertEqual(std.strReplace('xoxoxoxox', 'xoxox3xox', 'A'), 'xoxoxoxox') &&
std.assertEqual(std.strReplace('xoxoxox3xox', 'xoxox3xox', 'A'), 'xoA') &&
std.assertEqual(std.strReplace('A cat is a cat', 'cat', 'dog'), 'A dog is a dog') &&
std.assertEqual(std.strReplace('wishyfishyisishy', 'ish', 'and'), 'wandyfandyisandy') &&

std.assertEqual(std.map(function(x) x * x, []), []) &&
std.assertEqual(std.map(function(x) x * x, [1, 2, 3, 4]), [1, 4, 9, 16]) &&
std.assertEqual(std.map(function(x) x * x, std.filter(function(x) x > 5, std.range(1, 10))), [36, 49, 64, 81, 100]) &&

std.assertEqual(std.mapWithIndex(function(i, x) x * i, []), []) &&
std.assertEqual(std.mapWithIndex(function(i, x) x * i, [1, 2, 3, 4]), [0, 2, 6, 12]) &&
std.assertEqual(std.mapWithIndex(function(i, x) x * i, std.filter(function(x) x > 5, std.range(1, 10))), [0, 7, 16, 27, 40]) &&

std.assertEqual(std.mapWithKey(function(k, o) k + o, {}), {}) &&
std.assertEqual(std.mapWithKey(function(k, o) k + o, { a: 1, b: 2 }), { a: 'a1', b: 'b2' }) &&

std.assertEqual(std.flatMap(function(x) [x, x], [1, 2, 3]), [1, 1, 2, 2, 3, 3]) &&
std.assertEqual(std.flatMap(function(x) if x == 2 then [] else [x], [1, 2, 3]), [1, 3]) &&
std.assertEqual(std.flatMap(function(x) if x == 2 then [] else [x * 3, x * 2], [1, 2, 3]), [3, 2, 9, 6]) &&

std.assertEqual(std.filterMap(function(x) x >= 0, function(x) x * x, [-3, -2, -1, 0, 1, 2, 3]), [0, 1, 4, 9]) &&

std.assertEqual(std.foldl(function(x, y) [x, y], [], 'foo'), 'foo') &&
std.assertEqual(std.foldl(function(x, y) [x, y], [1, 2, 3, 4], []), [[[[[], 1], 2], 3], 4]) &&

std.assertEqual(std.foldr(function(x, y) [x, y], [], 'bar'), 'bar') &&
std.assertEqual(std.foldr(function(x, y) [x, y], [1, 2, 3, 4], []), [1, [2, [3, [4, []]]]]) &&

std.assertEqual(std.range(2, 6), [2, 3, 4, 5, 6]) &&
std.assertEqual(std.range(2, 2), [2]) &&
std.assertEqual(std.range(2, 1), []) &&

std.assertEqual(std.repeat([], 0), []) &&
std.assertEqual(std.repeat([1], 1), [1]) &&
std.assertEqual(std.repeat([1, 2], 1), [1, 2]) &&
std.assertEqual(std.repeat([1], 2), [1, 1]) &&
std.assertEqual(std.repeat([1, 2], 2), [1, 2, 1, 2]) &&
std.assertEqual(std.repeat('a', 1), 'a') &&
std.assertEqual(std.repeat('a', 4), 'aaaa') &&
std.assertEqual(std.repeat('ab', 4), 'abababab') &&
std.assertEqual(std.repeat('a', 0), '') &&

std.assertEqual(std.join([], [[1, 2], [3, 4, 5], [6]]), [1, 2, 3, 4, 5, 6]) &&
std.assertEqual(std.join(['a', 'b'], [[]]), []) &&
std.assertEqual(std.join(['a', 'b'], []), []) &&
std.assertEqual(std.join(['a', 'b'], [null, [1, 2], null, [3, 4, 5], [6], null]), [1, 2, 'a', 'b', 3, 4, 5, 'a', 'b', 6]) &&
std.assertEqual(std.join(['a', 'b'], [[], [1, 2]]), ['a', 'b', 1, 2]) &&
std.assertEqual(std.join('', [null, '12', null, '345', '6', null]), '123456') &&
std.assertEqual(std.join('ab', ['']), '') &&
std.assertEqual(std.join('ab', []), '') &&
std.assertEqual(std.join('ab', [null, '12', null, '345', '6', null]), '12ab345ab6') &&
std.assertEqual(std.join('ab', ['', '12']), 'ab12') &&
std.assertEqual(std.lines(['a', null, 'b']), 'a\nb\n') &&

std.assertEqual(std.flattenArrays([[1, 2, 3], [4, 5, 6], []]), [1, 2, 3, 4, 5, 6]) &&

std.assertEqual(std.flattenDeepArray([]), []) &&
std.assertEqual(std.flattenDeepArray([1, 2, 3]), [1, 2, 3]) &&
std.assertEqual(std.flattenDeepArray([1, [2, 3]]), [1, 2, 3]) &&
std.assertEqual(std.flattenDeepArray([[1], [2, 3], [[null]]]), [1, 2, 3, null]) &&

std.assertEqual(
  std.manifestIni({
    main: { a: '1', b: '2' },
    sections: {
      s1: { x: '11', y: '22', z: '33' },
      s2: { p: 'yes', q: '' },
      empty: {},
    },
  }),
  'a = 1\nb = 2\n[empty]\n[s1]\nx = 11\ny = 22\nz = 33\n[s2]\np = yes\nq = \n'
) &&

std.assertEqual(
  std.manifestIni({
    sections: {
      s1: { x: '11', y: '22', z: '33' },
      s2: { p: 'yes', q: '' },
      empty: {},
    },
  }),
  '[empty]\n[s1]\nx = 11\ny = 22\nz = 33\n[s2]\np = yes\nq = \n'
) &&

std.assertEqual(
  std.manifestIni({
    main: { a: ['1', '2'] },
    sections: {
      s2: { p: ['yes', ''] },
    },
  }), 'a = 1\na = 2\n[s2]\np = yes\np = \n'
) &&


std.assertEqual(std.escapeStringJson('hello'), '"hello"') &&
std.assertEqual(std.escapeStringJson('he"llo'), '"he\\"llo"') &&
std.assertEqual(std.escapeStringJson('he"llo'), '"he\\"llo"') &&
std.assertEqual(std.escapeStringBash("he\"l'lo"), "'he\"l'\"'\"'lo'") &&
std.assertEqual(std.escapeStringDollars('The path is ${PATH}.'), 'The path is $${PATH}.') &&
std.assertEqual(std.escapeStringXML('2 < 3'), '2 &lt; 3') &&
std.assertEqual(std.escapeStringXML('3 > 2'), '3 &gt; 2') &&
std.assertEqual(std.escapeStringXML('"foo"'), '&quot;foo&quot;') &&
std.assertEqual(std.escapeStringXML("don't believe the hype"), 'don&apos;t believe the hype') &&
std.assertEqual(std.escapeStringXML('PB&J'), 'PB&amp;J') &&
std.assertEqual(std.escapeStringJson('!~'), '"!~"') &&

std.assertEqual(std.manifestPython({
  x: 'test',
  y: [],
  z: ['foo', 'bar'],
  n: 1,
  a: true,
  b: false,
  c: null,
  o: { f1: 'foo', f2: 'bar' },
}), '{%s}' % std.join(', ', [
  '"a": True',
  '"b": False',
  '"c": None',
  '"n": 1',
  '"o": {"f1": "foo", "f2": "bar"}',
  '"x": "test"',
  '"y": []',
  '"z": ["foo", "bar"]',
])) &&

std.assertEqual(std.manifestPythonVars({
  x: 'test',
  y: [],
  z: ['foo', 'bar'],
  n: 1,
  a: true,
  b: false,
  c: null,
  o: { f1: 'foo', f2: 'bar' },
}), std.join('\n', [
  'a = True',
  'b = False',
  'c = None',
  'n = 1',
  'o = {"f1": "foo", "f2": "bar"}',
  'x = "test"',
  'y = []',
  'z = ["foo", "bar"]',
  '',
])) &&

std.assertEqual(
  std.manifestXmlJsonml(
    ['f', {}, ' ', ['g', 'inside'], 'nope', ['h', { attr: 'yolo' }, ['x', { attr: 'otter' }]]]
  ),
  '<f> <g>inside</g>nope<h attr="yolo"><x attr="otter"></x></h></f>'
) &&


std.assertEqual(std.base64('Hello World!'), 'SGVsbG8gV29ybGQh') &&
std.assertEqual(std.base64('Hello World'), 'SGVsbG8gV29ybGQ=') &&
std.assertEqual(std.base64('Hello Worl'), 'SGVsbG8gV29ybA==') &&
std.assertEqual(std.base64(''), '') &&

std.assertEqual(std.base64Decode('SGVsbG8gV29ybGQh'), 'Hello World!') &&
std.assertEqual(std.base64Decode('SGVsbG8gV29ybGQ='), 'Hello World') &&
std.assertEqual(std.base64Decode('SGVsbG8gV29ybA=='), 'Hello Worl') &&
std.assertEqual(std.base64Decode(''), '') &&

std.assertEqual(std.reverse([]), []) &&
std.assertEqual(std.reverse([1]), [1]) &&
std.assertEqual(std.reverse([1, 2]), [2, 1]) &&
std.assertEqual(std.reverse([1, 2, 3]), [3, 2, 1]) &&
std.assertEqual(std.reverse([[1, 2, 3]]), [[1, 2, 3]]) &&

std.assertEqual(std.sort([]), []) &&
std.assertEqual(std.sort([1]), [1]) &&
std.assertEqual(std.sort([1, 2]), [1, 2]) &&
std.assertEqual(std.sort([2, 1]), [1, 2]) &&
std.assertEqual(std.sort(['1', '2']), ['1', '2']) &&
std.assertEqual(std.sort(['2', '1']), ['1', '2']) &&
std.assertEqual(
  std.sort(['The', 'rain', 'in', 'spain', 'falls', 'mainly', 'on', 'the', 'plain.']),
  ['The', 'falls', 'in', 'mainly', 'on', 'plain.', 'rain', 'spain', 'the']
) &&

std.assertEqual(std.uniq([]), []) &&
std.assertEqual(std.uniq([1]), [1]) &&
std.assertEqual(std.uniq([1, 2]), [1, 2]) &&
std.assertEqual(std.uniq(['1', '2']), ['1', '2']) &&
std.assertEqual(
  std.uniq(['The', 'falls', 'in', 'mainly', 'on', 'plain.', 'rain', 'spain', 'the']),
  ['The', 'falls', 'in', 'mainly', 'on', 'plain.', 'rain', 'spain', 'the']
) &&

local animal_set = ['ant', 'bat', 'cat', 'dog', 'elephant', 'fish', 'giraffe'];

std.assertEqual(
  std.uniq(['ant', 'bat', 'cat', 'dog', 'dog', 'elephant', 'fish', 'fish', 'giraffe']),
  animal_set
) &&

std.assertEqual(
  std.set(['dog', 'ant', 'bat', 'cat', 'dog', 'elephant', 'fish', 'giraffe', 'fish']),
  animal_set
) &&

std.assertEqual(std.setUnion(animal_set, animal_set), animal_set) &&
std.assertEqual(std.setUnion(animal_set, []), animal_set) &&
std.assertEqual(std.setUnion([], animal_set), animal_set) &&
std.assertEqual(std.setUnion([], []), []) &&
std.assertEqual(std.setUnion(['a', 'b'], ['b', 'c']), ['a', 'b', 'c']) &&

std.assertEqual(std.setInter(animal_set, animal_set), animal_set) &&
std.assertEqual(std.setInter(animal_set, []), []) &&
std.assertEqual(std.setInter([], animal_set), []) &&
std.assertEqual(std.setInter([], []), []) &&
std.assertEqual(std.setInter(['a', 'b'], ['b', 'c']), ['b']) &&

std.assertEqual(std.setDiff(animal_set, animal_set), []) &&
std.assertEqual(std.setDiff(animal_set, []), animal_set) &&
std.assertEqual(std.setDiff([], animal_set), []) &&
std.assertEqual(std.setDiff([], []), []) &&
std.assertEqual(std.setDiff(['a', 'b'], ['b', 'c']), ['a']) &&

std.assertEqual(std.setMember('a', ['a', 'b', 'c']), true) &&
std.assertEqual(std.setMember('a', []), false) &&
std.assertEqual(std.setMember('a', ['b', 'c']), false) &&

(
  if std.thisFile == '<stdin>' || std.thisFile == "(memory)" then
    // This happens when testing the unparser or scala.js
    true
  else
    std.assertEqual(std.thisFile, 'stdlib_js.jsonnet')
) &&

std.assertEqual(import 'this_file/a.libsonnet', 'this_file/a.libsonnet') &&
std.assertEqual(import 'this_file/b.libsonnet', 'this_file/a.libsonnet') &&


std.assertEqual(std.extVar('var1'), 'test') &&

std.assertEqual(std.toString(std.extVar('var2')), '{"x": 1, "y": 2}') &&
std.assertEqual(std.extVar('var2'), { x: 1, y: 2 }) &&
std.assertEqual(std.extVar('var2') { x+: 2 }.x, 3) &&

std.assertEqual(std.split('foo/bar', '/'), ['foo', 'bar']) &&
std.assertEqual(std.split('/foo/', '/'), ['', 'foo', '']) &&
std.assertEqual(std.split('foo/_bar', '/_'), ['foo', 'bar']) &&
std.assertEqual(std.split('/_foo/_', '/_'), ['', 'foo', '']) &&

std.assertEqual(std.splitLimit('foo/bar', '/', 1), ['foo', 'bar']) &&
std.assertEqual(std.splitLimit('/foo/', '/', 1), ['', 'foo/']) &&
std.assertEqual(std.splitLimit('foo/_bar', '/_', 1), ['foo', 'bar']) &&
std.assertEqual(std.splitLimit('/_foo/_', '/_', 1), ['', 'foo/_']) &&

std.assertEqual(std.splitLimitR('foo/bar', '/', 1), ['foo', 'bar']) &&
std.assertEqual(std.splitLimitR('/foo/', '/', 1), ['/foo', '']) &&
std.assertEqual(std.splitLimitR('/foo/', '/', -1), ['', 'foo', '']) &&
std.assertEqual(std.splitLimitR('foo/_bar', '/_', 1), ['foo', 'bar']) &&
std.assertEqual(std.splitLimitR('/_foo/_', '/_', 1), ['/_foo', '']) &&
std.assertEqual(std.splitLimitR('/_foo/_', '/_', -1), ['', 'foo', '']) &&

local some_toml = {
  key: 'value',
  simple: { t: 5 },
  section: {
    a: 1,
    nested: { b: 2 },
    'e$caped': { q: 't' },
    array: [
      { c: 3 },
      { d: 4 },
    ],
    nestedArray: [{
      k: 'v',
      nested: { e: 5 },
    }],
  },
  arraySection: [
    { q: 1 },
    { w: 2 },
  ],
  'escaped"Section': { z: 'q' },
  emptySection: {},
  emptyArraySection: [{}],
  bool: true,
  notBool: false,
  number: 7,
  array: ['s', 1, [2, 3], { r: 6, a: ['0', 'z'] }],
  emptyArray: [],
  '"': 4,
};

std.assertEqual(
  std.manifestTomlEx(some_toml, '  ') + '\n',
  |||
    "\"" = 4
    array = [
      "s",
      1,
      [ 2, 3 ],
      { a = [ "0", "z" ], r = 6 }
    ]
    bool = true
    emptyArray = []
    key = "value"
    notBool = false
    number = 7

    [[arraySection]]
      q = 1

    [[arraySection]]
      w = 2

    [[emptyArraySection]]

    [emptySection]

    ["escaped\"Section"]
      z = "q"

    [section]
      a = 1

      [[section.array]]
        c = 3

      [[section.array]]
        d = 4

      [section."e$caped"]
        q = "t"

      [section.nested]
        b = 2

      [[section.nestedArray]]
        k = "v"

        [section.nestedArray.nested]
          e = 5

    [simple]
      t = 5
  |||
) &&

local some_json = {
  x: [1, 2, 3, true, false, null, 'string\nstring\n'],
  arr: [[[]]],
  y: { a: 1, b: 2, c: [1, 2] },
  emptyArray: [],
  emptyObject: {},
  objectInArray: [{ f: 3 }],
  '"': null,
};

local bare_yaml_quoted = {
  '685230': 'canonical',
  '+685_230': 'decimal',
  '02472256': 'octal',
  '-1_0': 'negative integer',
  '-0.1_0_0': 'negative float',
  '0x_0A_74_AE': 'hexadecimal',
  '-0x_0A_74_AE': 'negative hexadecimal',
  '0b1010_0111_0100_1010_1110': 'binary',
  '-0b1010_0111_0100_1010_1110': 'binary',
  '190:20:30': 'sexagesimal',
  '-190:20:30': 'negative sexagesimal',
  '6.8523015e+5': 'canonical',
  '6.8523015e-5': 'canonical',
  '-6.8523015e+5': 'negative canonical',
  '685.230_15e+03': 'exponential',
  '-685.230_15e+03': 'negative exponential',
  '-685.230_15e-03': 'negative w/ negative exponential',
  '-685.230_15E-03': 'negative w/ negative exponential',
  '685_230.15': 'fixed',
  '-685_230.15': 'negative fixed',
  '190:20:30.15': 'sexagesimal',
  '-190:20:30.15': 'negative sexagesimal',
  '-.inf': 'negative infinity',
  '.inf': 'positive infinity',
  '+.inf': 'positive infinity',
  '.NaN': 'not a number',
  y: 'boolean true',
  yes: 'boolean true',
  Yes: 'boolean true',
  True: 'boolean true',
  'true': 'boolean true',
  on: 'boolean true',
  On: 'boolean true',
  NO: 'boolean false',
  n: 'boolean false',
  N: 'boolean false',
  off: 'boolean false',
  OFF: 'boolean false',
  'null': 'null word',
  NULL: 'null word capital',
  Null: 'null word',
  '~': 'null key',
  '': 'empty key',
  '-': 'invalid bare key',
  '---': 'triple dash key',
  '2001-12-15T02:59:43.1Z': 'canonical',
  '2001-12-14t21:59:43.10-05:00': 'valid iso8601',
  '2001-12-14 21:59:43.10 -5': 'space separated',
  '2001-12-15 2:59:43.10': 'no time zone (Z)',
  '2002-12-14': 'date',
};
local bare_yaml_unquoted = {
  '0X_0a_74_ae': 'BARE_KEY',
  '__-0X_0a_74_ae': 'BARE_KEY',
  '-0B1010_0111_0100_1010_1110': 'BARE_KEY',
  '__-0B1010_0111_0100_1010_1110': 'BARE_KEY',
  x: 'BARE_KEY',
  b: 'BARE_KEY',
  just_letters_underscores: 'BARE_KEY',
  'just-letters-dashes': 'BARE_KEY',
  'jsonnet.org/k8s-label-like': 'BARE_KEY',
  '192.168.0.1': 'BARE_KEY',
  '1-234-567-8901': 'BARE_KEY',
};
local bare_yaml_test = bare_yaml_quoted + bare_yaml_unquoted;

std.assertEqual(
  std.manifestJsonEx(some_json, '    ') + '\n',
  |||
    {
        "\"": null,
        "arr": [
            [
                [

                ]
            ]
        ],
        "emptyArray": [

        ],
        "emptyObject": {

        },
        "objectInArray": [
            {
                "f": 3
            }
        ],
        "x": [
            1,
            2,
            3,
            true,
            false,
            null,
            "string\nstring\n"
        ],
        "y": {
            "a": 1,
            "b": 2,
            "c": [
                1,
                2
            ]
        }
    }
  |||
) &&

std.assertEqual(
  std.manifestJsonEx(some_json, '', ' ', ' : '),
  '{ "\\"" : null, "arr" : [ [ [  ] ] ], "emptyArray" : [  ], '
  + '"emptyObject" : {  }, "objectInArray" : [ { "f" : 3 } ], '
  + '"x" : [ 1, 2, 3, true, false, null, "string\\nstring\\n" ], '
  + '"y" : { "a" : 1, "b" : 2, "c" : [ 1, 2 ] } }'
) &&

std.assertEqual(
  std.manifestJsonMinified(some_json),
  '{"\\"":null,"arr":[[[]]],"emptyArray":[],"emptyObject":{},"objectInArray":[{"f":3}],'
  + '"x":[1,2,3,true,false,null,"string\\nstring\\n"],"y":{"a":1,"b":2,"c":[1,2]}}'
) &&

std.assertEqual(
  std.manifestYamlDoc([{ x: [1, 2, 3] }], quote_keys=false) + '\n',
  |||
    - x:
      - 1
      - 2
      - 3
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc([{ x: [1, 2, 3] }]) + '\n',
  |||
    - "x":
      - 1
      - 2
      - 3
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ x: [1, 2, 3] }) + '\n',
  |||
    "x":
    - 1
    - 2
    - 3
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc([[[1, 2], [3, 4]]]) + '\n',
  |||
    -
      -
        - 1
        - 2
      -
        - 3
        - 4
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ x: [[[1, [1], 1]]] }) + '\n',
  |||
    "x":
    -
      -
        - 1
        -
          - 1
        - 1
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ x: [[[1, { f: 3, g: [1, 2] }, 1]]] }) + '\n',
  |||
    "x":
    -
      -
        - 1
        - "f": 3
          "g":
          - 1
          - 2
        - 1
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ x: [[[1, { f: 3, g: [1, 2] }, 1]]] }, quote_keys=false) + '\n',
  |||
    x:
    -
      -
        - 1
        - f: 3
          g:
          - 1
          - 2
        - 1
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc('hello\nworld\n') + '\n',
  |||
    |
      hello
      world
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc(['hello\nworld\n']) + '\n',
  |||
    - |
      hello
      world
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ f: 'hello\nworld\n' }) + '\n',
  |||
    "f": |
      hello
      world
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ f: 'hello\nworld\n' }, quote_keys=false) + '\n',
  |||
    f: |
      hello
      world
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc(some_json) + '\n',
  |||
    "\"": null
    "arr":
    -
      - []
    "emptyArray": []
    "emptyObject": {}
    "objectInArray":
    - "f": 3
    "x":
    - 1
    - 2
    - 3
    - true
    - false
    - null
    - |
      string
      string
    "y":
      "a": 1
      "b": 2
      "c":
      - 1
      - 2
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc(some_json, quote_keys=false) + '\n',
  |||
    "\"": null
    arr:
    -
      - []
    emptyArray: []
    emptyObject: {}
    objectInArray:
    - f: 3
    x:
    - 1
    - 2
    - 3
    - true
    - false
    - null
    - |
      string
      string
    "y":
      a: 1
      b: 2
      c:
      - 1
      - 2
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc([{ x: [1, 2, 3] }], indent_array_in_object=true) + '\n',
  |||
    - "x":
        - 1
        - 2
        - 3
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc([{ x: [1, 2, 3] }], indent_array_in_object=true, quote_keys=false) + '\n',
  |||
    - x:
        - 1
        - 2
        - 3
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ x: [1, 2, 3] }, indent_array_in_object=true) + '\n',
  |||
    "x":
      - 1
      - 2
      - 3
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc([[[1, 2], [3, 4]]], indent_array_in_object=true) + '\n',
  |||
    -
      -
        - 1
        - 2
      -
        - 3
        - 4
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ x: [[[1, [1], 1]]] }, indent_array_in_object=true) + '\n',
  |||
    "x":
      -
        -
          - 1
          -
            - 1
          - 1
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ x: [[[1, { f: 3, g: [1, 2] }, 1]]] }, indent_array_in_object=true) + '\n',
  |||
    "x":
      -
        -
          - 1
          - "f": 3
            "g":
              - 1
              - 2
          - 1
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc('hello\nworld\n', indent_array_in_object=true) + '\n',
  |||
    |
      hello
      world
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc(['hello\nworld\n'], indent_array_in_object=true) + '\n',
  |||
    - |
      hello
      world
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc({ f: 'hello\nworld\n' }, indent_array_in_object=true) + '\n',
  |||
    "f": |
      hello
      world
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc(some_json, indent_array_in_object=true) + '\n',
  |||
    "\"": null
    "arr":
      -
        - []
    "emptyArray": []
    "emptyObject": {}
    "objectInArray":
      - "f": 3
    "x":
      - 1
      - 2
      - 3
      - true
      - false
      - null
      - |
        string
        string
    "y":
      "a": 1
      "b": 2
      "c":
        - 1
        - 2
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc(some_json, indent_array_in_object=true, quote_keys=false) + '\n',
  |||
    "\"": null
    arr:
      -
        - []
    emptyArray: []
    emptyObject: {}
    objectInArray:
      - f: 3
    x:
      - 1
      - 2
      - 3
      - true
      - false
      - null
      - |
        string
        string
    "y":
      a: 1
      b: 2
      c:
        - 1
        - 2
  |||
) &&

std.assertEqual(
  std.manifestYamlDoc(bare_yaml_test, quote_keys=false) + '\n',
  |||
    "": "empty key"
    "+.inf": "positive infinity"
    "+685_230": "decimal"
    "-": "invalid bare key"
    "---": "triple dash key"
    "-.inf": "negative infinity"
    "-0.1_0_0": "negative float"
    -0B1010_0111_0100_1010_1110: "BARE_KEY"
    "-0b1010_0111_0100_1010_1110": "binary"
    "-0x_0A_74_AE": "negative hexadecimal"
    "-190:20:30": "negative sexagesimal"
    "-190:20:30.15": "negative sexagesimal"
    "-1_0": "negative integer"
    "-6.8523015e+5": "negative canonical"
    "-685.230_15E-03": "negative w/ negative exponential"
    "-685.230_15e+03": "negative exponential"
    "-685.230_15e-03": "negative w/ negative exponential"
    "-685_230.15": "negative fixed"
    ".NaN": "not a number"
    ".inf": "positive infinity"
    "02472256": "octal"
    0X_0a_74_ae: "BARE_KEY"
    "0b1010_0111_0100_1010_1110": "binary"
    "0x_0A_74_AE": "hexadecimal"
    1-234-567-8901: "BARE_KEY"
    "190:20:30": "sexagesimal"
    "190:20:30.15": "sexagesimal"
    192.168.0.1: "BARE_KEY"
    "2001-12-14 21:59:43.10 -5": "space separated"
    "2001-12-14t21:59:43.10-05:00": "valid iso8601"
    "2001-12-15 2:59:43.10": "no time zone (Z)"
    "2001-12-15T02:59:43.1Z": "canonical"
    "2002-12-14": "date"
    "6.8523015e+5": "canonical"
    "6.8523015e-5": "canonical"
    "685.230_15e+03": "exponential"
    "685230": "canonical"
    "685_230.15": "fixed"
    "N": "boolean false"
    "NO": "boolean false"
    "NULL": "null word capital"
    "Null": "null word"
    "OFF": "boolean false"
    "On": "boolean true"
    "True": "boolean true"
    "Yes": "boolean true"
    __-0B1010_0111_0100_1010_1110: "BARE_KEY"
    __-0X_0a_74_ae: "BARE_KEY"
    b: "BARE_KEY"
    jsonnet.org/k8s-label-like: "BARE_KEY"
    just-letters-dashes: "BARE_KEY"
    just_letters_underscores: "BARE_KEY"
    "n": "boolean false"
    "null": "null word"
    "off": "boolean false"
    "on": "boolean true"
    "true": "boolean true"
    x: "BARE_KEY"
    "y": "boolean true"
    "yes": "boolean true"
    "~": "null key"
  |||
) &&

std.assertEqual(
  std.manifestYamlStream([bare_yaml_quoted, bare_yaml_unquoted], quote_keys=false),
  |||
    ---
    "": "empty key"
    "+.inf": "positive infinity"
    "+685_230": "decimal"
    "-": "invalid bare key"
    "---": "triple dash key"
    "-.inf": "negative infinity"
    "-0.1_0_0": "negative float"
    "-0b1010_0111_0100_1010_1110": "binary"
    "-0x_0A_74_AE": "negative hexadecimal"
    "-190:20:30": "negative sexagesimal"
    "-190:20:30.15": "negative sexagesimal"
    "-1_0": "negative integer"
    "-6.8523015e+5": "negative canonical"
    "-685.230_15E-03": "negative w/ negative exponential"
    "-685.230_15e+03": "negative exponential"
    "-685.230_15e-03": "negative w/ negative exponential"
    "-685_230.15": "negative fixed"
    ".NaN": "not a number"
    ".inf": "positive infinity"
    "02472256": "octal"
    "0b1010_0111_0100_1010_1110": "binary"
    "0x_0A_74_AE": "hexadecimal"
    "190:20:30": "sexagesimal"
    "190:20:30.15": "sexagesimal"
    "2001-12-14 21:59:43.10 -5": "space separated"
    "2001-12-14t21:59:43.10-05:00": "valid iso8601"
    "2001-12-15 2:59:43.10": "no time zone (Z)"
    "2001-12-15T02:59:43.1Z": "canonical"
    "2002-12-14": "date"
    "6.8523015e+5": "canonical"
    "6.8523015e-5": "canonical"
    "685.230_15e+03": "exponential"
    "685230": "canonical"
    "685_230.15": "fixed"
    "N": "boolean false"
    "NO": "boolean false"
    "NULL": "null word capital"
    "Null": "null word"
    "OFF": "boolean false"
    "On": "boolean true"
    "True": "boolean true"
    "Yes": "boolean true"
    "n": "boolean false"
    "null": "null word"
    "off": "boolean false"
    "on": "boolean true"
    "true": "boolean true"
    "y": "boolean true"
    "yes": "boolean true"
    "~": "null key"
    ---
    -0B1010_0111_0100_1010_1110: "BARE_KEY"
    0X_0a_74_ae: "BARE_KEY"
    1-234-567-8901: "BARE_KEY"
    192.168.0.1: "BARE_KEY"
    __-0B1010_0111_0100_1010_1110: "BARE_KEY"
    __-0X_0a_74_ae: "BARE_KEY"
    b: "BARE_KEY"
    jsonnet.org/k8s-label-like: "BARE_KEY"
    just-letters-dashes: "BARE_KEY"
    just_letters_underscores: "BARE_KEY"
    x: "BARE_KEY"
    ...
  |||
) &&

std.assertEqual(
  std.manifestYamlStream([some_json, some_json, {}, [], 3, '"']),
  |||
    ---
    "\"": null
    "arr":
    -
      - []
    "emptyArray": []
    "emptyObject": {}
    "objectInArray":
    - "f": 3
    "x":
    - 1
    - 2
    - 3
    - true
    - false
    - null
    - |
      string
      string
    "y":
      "a": 1
      "b": 2
      "c":
      - 1
      - 2
    ---
    "\"": null
    "arr":
    -
      - []
    "emptyArray": []
    "emptyObject": {}
    "objectInArray":
    - "f": 3
    "x":
    - 1
    - 2
    - 3
    - true
    - false
    - null
    - |
      string
      string
    "y":
      "a": 1
      "b": 2
      "c":
      - 1
      - 2
    ---
    {}
    ---
    []
    ---
    3
    ---
    "\""
    ...
  |||
) &&

std.assertEqual(
  std.manifestYamlStream([some_json, some_json, {}, [], 3, '"'], quote_keys=false),
  |||
    ---
    "\"": null
    arr:
    -
      - []
    emptyArray: []
    emptyObject: {}
    objectInArray:
    - f: 3
    x:
    - 1
    - 2
    - 3
    - true
    - false
    - null
    - |
      string
      string
    "y":
      a: 1
      b: 2
      c:
      - 1
      - 2
    ---
    "\"": null
    arr:
    -
      - []
    emptyArray: []
    emptyObject: {}
    objectInArray:
    - f: 3
    x:
    - 1
    - 2
    - 3
    - true
    - false
    - null
    - |
      string
      string
    "y":
      a: 1
      b: 2
      c:
      - 1
      - 2
    ---
    {}
    ---
    []
    ---
    3
    ---
    "\""
    ...
  |||
) &&

std.assertEqual(
  std.manifestYamlStream([some_json, some_json, {}, [], 3, '"'], indent_array_in_object=true),
  |||
    ---
    "\"": null
    "arr":
      -
        - []
    "emptyArray": []
    "emptyObject": {}
    "objectInArray":
      - "f": 3
    "x":
      - 1
      - 2
      - 3
      - true
      - false
      - null
      - |
        string
        string
    "y":
      "a": 1
      "b": 2
      "c":
        - 1
        - 2
    ---
    "\"": null
    "arr":
      -
        - []
    "emptyArray": []
    "emptyObject": {}
    "objectInArray":
      - "f": 3
    "x":
      - 1
      - 2
      - 3
      - true
      - false
      - null
      - |
        string
        string
    "y":
      "a": 1
      "b": 2
      "c":
        - 1
        - 2
    ---
    {}
    ---
    []
    ---
    3
    ---
    "\""
    ...
  |||
) &&

std.assertEqual(
  std.manifestYamlStream([{}, [], 3, '"'], c_document_end=false),
  |||
    ---
    {}
    ---
    []
    ---
    3
    ---
    "\""
  |||
) &&

std.assertEqual(std.parseInt('01234567890'), 1234567890) &&
std.assertEqual(std.parseInt('-01234567890'), -1234567890) &&
std.assertEqual(std.parseOctal('755'), 493) &&
std.assertEqual(std.parseOctal('0755'), 493) &&
std.assertEqual(std.parseHex('ff'), 255) &&
std.assertEqual(std.parseHex('FF'), 255) &&
std.assertEqual(std.parseHex('0ff'), 255) &&
std.assertEqual(std.parseHex('0FF'), 255) &&
std.assertEqual(std.parseHex('a'), 10) &&
std.assertEqual(std.parseHex('A'), 10) &&
std.assertEqual(std.parseHex('4a'), 74) &&

// verified by running md5 -s value
//std.assertEqual(std.md5(''), 'd41d8cd98f00b204e9800998ecf8427e') &&
//std.assertEqual(std.md5('grape'), 'b781cbb29054db12f88f08c6e161c199') &&
//std.assertEqual(std.md5("{}[]01234567890\"'+=-_/<>?,.!@#$%^&*|\\:;`~"), 'a680db28332f0c9647376e5b2aeb4b3d') &&
//std.assertEqual(std.md5('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur sodales ligula in libero. Sed dignissim lacinia nunc. Curabitur tortor. Pellentesque nibh. Aenean quam. In scelerisque sem at dolor. Maecenas mattis. Sed convallis tristique sem. Proin ut ligula vel nunc egestas porttitor. Morbi lectus risus, iaculis vel, suscipit quis, luctus non, massa. Fusce ac turpis quis ligula lacinia aliquet. Mauris ipsum. Nulla metus metus, ullamcorper vel, tincidunt sed, euismod in, nibh. Quisque volutpat condimentum velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nam nec ante. Sed lacinia, urna non tincidunt mattis, tortor neque adipiscing diam, a cursus ipsum ante quis turpis. Nulla facilisi. Ut fringilla. Suspendisse potenti. Nunc feugiat mi a tellus consequat imperdiet. Vestibulum sapien. Proin quam. Etiam ultrices. Suspendisse in justo eu magna luctus suscipit. Sed lectus. Integer euismod lacus luctus magna. Quisque cursus, metus vitae pharetra auctor, sem massa mattis sem, at interdum magna augue eget diam. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Morbi lacinia molestie dui. Praesent blandit dolor. Sed non quam. In vel mi sit amet augue congue elementum. Morbi in ipsum sit amet pede facilisis laoreet. Donec lacus nunc, viverra nec, blandit vel, egestas et, augue. Vestibulum tincidunt malesuada tellus. Ut ultrices ultrices enim. Curabitur sit amet mauris. Morbi in dui quis est pulvinar ullamcorper. Nulla facilisi. Integer lacinia sollicitudin massa. Cras metus. Sed aliquet risus a tortor. Integer id quam. Morbi mi. Quisque nisl felis, venenatis tristique, dignissim in, ultrices sit amet, augue. Proin sodales libero eget ante. Nulla quam. Aenean laoreet. Vestibulum nisi lectus, commodo ac, facilisis ac, ultricies eu, pede. Ut orci risus, accumsan porttitor, cursus quis, aliquet eget, justo. Sed pretium blandit orci. Ut eu diam at pede suscipit sodales. Aenean lectus elit, fermentum non, convallis id, sagittis at, neque. Nullam mauris orci, aliquet et, iaculis et, viverra vitae, ligula. Nulla ut felis in purus aliquam imperdiet. Maecenas aliquet mollis lectus. Vivamus consectetuer risus et tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur sodales ligula in libero. Sed dignissim lacinia nunc. Curabitur tortor. Pellentesque nibh. Aenean quam. In scelerisque sem at dolor. Maecenas mattis. Sed convallis tristique sem. Proin ut ligula vel nunc egestas porttitor. Morbi lectus risus, iaculis vel, suscipit quis, luctus non, massa. Fusce ac turpis quis ligula lacinia aliquet. Mauris ipsum. Nulla metus metus, ullamcorper vel, tincidunt sed, euismod in, nibh. Quisque volutpat condimentum velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nam nec ante. Sed lacinia, urna non tincidunt mattis, tortor neque adipiscing diam, a cursus ipsum ante quis turpis. Nulla facilisi. Ut fringilla. Suspendisse potenti. Nunc feugiat mi a tellus consequat imperdiet. Vestibulum sapien. Proin quam. Etiam ultrices. Suspendisse in justo eu magna luctus suscipit. Sed lectus. Integer euismod lacus luctus magna. Quisque cursus, metus vitae pharetra auctor, sem massa mattis sem, at interdum magna augue eget diam. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Morbi lacinia molestie dui. Praesent blandit dolor. Sed non quam. In vel mi sit amet augue congue elementum. Morbi in ipsum sit amet pede facilisis laoreet. Donec lacus nunc, viverra nec, blandit vel, egestas et, augue. Vestibulum tincidunt malesuada tellus. Ut ultrices ultrices enim. Curabitur sit amet mauris. Morbi in dui quis est pulvinar ullamcorper. Nulla facilisi. Integer lacinia sollicitudin massa. Cras metus. Sed aliquet risus a tortor. Integer id quam. Morbi mi. Quisque nisl felis, venenatis tristique, dignissim in, ultrices sit amet, augue. Proin sodales libero eget ante. Nulla quam. Aenean laoreet. Vestibulum nisi lectus, commodo ac, facilisis ac, ultricies eu, pede. Ut orci risus, accumsan porttitor, cursus quis, aliquet eget, justo. Sed pretium blandit orci. Ut eu diam at pede suscipit sodales. Aenean lectus elit, fermentum non, convallis id, sagittis at, neque. Nullam mauris orci, aliquet et, iaculis et, viverra vitae, ligula. Nulla ut felis in purus aliquam imperdiet. Maecenas aliquet mollis lectus. Vivamus consectetuer risus et tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur sodales ligula in libero. Sed dignissim lacinia nunc. Curabitur tortor. Pellentesque nibh. Aenean quam. In scelerisque sem at dolor. Maecenas mattis. Sed convallis tristique sem. Proin ut ligula vel nunc egestas porttitor. Morbi lectus risus, iaculis vel, suscipit quis, luctus non, massa. Fusce ac turpis quis ligula lacinia aliquet. Mauris ipsum. Nulla metus metus, ullamcorper vel, tincidunt sed, euismod in, nibh. Quisque volutpat condimentum velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nam nec ante. Sed lacinia, urna non tincidunt mattis, tortor neque adipiscing diam, a cursus ipsum ante quis turpis. Nulla facilisi. Ut fringilla. Suspendisse potenti. Nunc feugiat mi a tellus consequat imperdiet. Vestibulum sapien. Proin quam. Etiam ultrices. Suspendisse in justo eu magna luctus suscipit. Sed lectus. Integer euismod lacus luctus magna. Quisque cursus, metus vitae pharetra auctor, sem massa mattis sem, at interdum magna augue eget diam. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Morbi lacinia molestie dui. Praesent blandit dolor. Sed non quam. In vel mi sit amet augue congue elementum. Morbi in ipsum si.'), '3496bb633e830e7679ce53700d42de1e') &&
std.assertEqual(std.parseInt('-01234567890'), -1234567890) &&

std.assertEqual(std.prune({}), {}) &&
std.assertEqual(std.prune([]), []) &&
std.assertEqual(std.prune(null), null) &&
std.assertEqual(std.prune({ a: [], b: {}, c: null }), {}) &&
std.assertEqual(std.prune([[], {}, null]), []) &&
std.assertEqual(std.prune({ a: [[], {}, null], b: { a: [], b: {}, c: null } }), {}) &&
std.assertEqual(std.prune([[[], {}, null], { a: [], b: {}, c: null }]), []) &&
std.assertEqual(std.prune({ a: [{ b: true }] }), { a: [{ b: true }] }) &&

std.assertEqual(std.parseJson('"foo"'), 'foo') &&
std.assertEqual(std.parseJson('{}'), {}) &&
std.assertEqual(std.parseJson('[]'), []) &&
std.assertEqual(std.parseJson('null'), null) &&
std.assertEqual(std.parseJson('12'), 12) &&
std.assertEqual(std.parseJson('12.123'), 12.123) &&
std.assertEqual(std.parseJson('{"a": {"b": ["c", 42]}}'), { a: { b: ['c', 42] } }) &&

std.assertEqual(std.parseYaml('{}'), {}) &&
std.assertEqual(std.parseYaml('[]'), []) &&
std.assertEqual(
  std.parseYaml(
    |||
      foo:
        bar:
        - true
        - 42
        - 1.0
    |||
  ), { foo: { bar: [true, 42, 1] } }
) &&
std.assertEqual(
  std.parseYaml(
    |||
      ---
      foo:
        bar:
        - true
        - 42
        - 1.0
      ---
      wibble:
        wobble:
        - true
        - 42
        - 1.0
    |||
  ), [{ foo: { bar: [true, 42, 1] } }, { wibble: { wobble: [true, 42, 1] } }]
) &&
std.assertEqual(
  std.parseYaml(
    |||
      - 1
      - 2
      - 3
    |||
  ), [1, 2, 3]
) &&
std.assertEqual(
  std.parseYaml(
    |||
      f1: |
        a
        b
      f2: "a\nb\n"
    |||
  ), { f1: 'a\nb\n', f2: 'a\nb\n' }
) &&
// Issue https://github.com/google/jsonnet/issues/1014
std.assertEqual(std.parseYaml('version: 1.2.3'), { version: '1.2.3' }) &&
// Issue https://github.com/google/jsonnet/issues/1050
std.assertEqual(std.type(std.parseYaml('id: "12345"').id), 'string') &&

std.assertEqual(std.asciiUpper('!@#$%&*()asdfghFGHJKL09876 '), '!@#$%&*()ASDFGHFGHJKL09876 ') &&
std.assertEqual(std.asciiLower('!@#$%&*()asdfghFGHJKL09876 '), '!@#$%&*()asdfghfghjkl09876 ') &&

std.assertEqual(std.deepJoin(['a', ['b', 'c', [[], 'd', ['e'], 'f', 'g'], [], []], 'h']),
                'abcdefgh') &&

std.assertEqual(std.findSubstr('', 'a'), []) &&
std.assertEqual(std.findSubstr('aa', ''), []) &&
std.assertEqual(std.findSubstr('aa', 'bb'), []) &&
std.assertEqual(std.findSubstr('aa', 'a'), []) &&
std.assertEqual(std.findSubstr('aa', 'aa'), [0]) &&
std.assertEqual(std.findSubstr('aa', 'bbaabaaa'), [2, 5, 6]) &&

std.assertEqual(std.find(null, [null]), [0]) &&
std.assertEqual(std.find([], [[]]), [0]) &&
std.assertEqual(std.find({}, [{}]), [0]) &&
std.assertEqual(std.find('a', []), []) &&
std.assertEqual(std.find('a', ['b']), []) &&
std.assertEqual(std.find('a', ['a']), [0]) &&
std.assertEqual(std.find('a', ['a', ['a'], 'b', 'a']), [0, 3]) &&
std.assertEqual(std.find(['a'], [['a']]), [0]) &&

std.assertEqual(std.encodeUTF8(''), []) &&
std.assertEqual(std.encodeUTF8('A'), [65]) &&
std.assertEqual(std.encodeUTF8('AAA'), [65, 65, 65]) &&
std.assertEqual(std.encodeUTF8('§'), [194, 167]) &&
std.assertEqual(std.encodeUTF8('Zażółć gęślą jaźń'), [90, 97, 197, 188, 195, 179, 197, 130, 196, 135, 32, 103, 196, 153, 197, 155, 108, 196, 133, 32, 106, 97, 197, 186, 197, 132]) &&
std.assertEqual(std.encodeUTF8('😃'), [240, 159, 152, 131]) &&

std.assertEqual(std.decodeUTF8([]), '') &&
std.assertEqual(std.decodeUTF8([65]), 'A') &&
std.assertEqual(std.decodeUTF8([65, 65, 65]), 'AAA') &&
std.assertEqual(std.decodeUTF8([(function(x) 65)(42)]), 'A') &&
std.assertEqual(std.decodeUTF8([65 + 1 - 1]), 'A') &&
std.assertEqual(std.decodeUTF8([90, 97, 197, 188, 195, 179, 197, 130, 196, 135, 32, 103, 196, 153, 197, 155, 108, 196, 133, 32, 106, 97, 197, 186, 197, 132]), 'Zażółć gęślą jaźń') &&
std.assertEqual(std.decodeUTF8([240, 159, 152, 131]), '😃') &&


std.assertEqual(std.any([true, false]), true) &&
std.assertEqual(std.any([false, false]), false) &&
std.assertEqual(std.any([]), false) &&

std.assertEqual(std.all([true, false]), false) &&
std.assertEqual(std.all([true, true]), true) &&
std.assertEqual(std.all([]), true) &&

std.assertEqual(std.sum([1, 2, 3]), 6) &&

std.assertEqual(std.avg([1, 2, 3]), 2) &&
std.assertEqual(std.avg([0, 0, 0]), 0) &&
std.assertEqual(std.avg([1, 1, 2.5]), 1.5) &&

std.assertEqual(std.minArray([3, 1, 2]), 1) &&
std.assertEqual(std.minArray(['3', '1', '2']), '1') &&
std.assertEqual(std.minArray(['a2', 'b1'], keyF=function(x) x[0]), 'a2') &&
std.assertEqual(std.minArray(['a2', 'b1'], keyF=function(x) x[1]), 'b1') &&
std.assertEqual(std.minArray([], onEmpty='default'), 'default') &&

std.assertEqual(std.maxArray([1, 2, 3]), 3) &&
std.assertEqual(std.maxArray(['1', '2', '3']), '3') &&
std.assertEqual(std.maxArray(['a', 'x', 'z']), 'z') &&
std.assertEqual(std.maxArray(['a2', 'b1'], keyF=function(x) x[0]), 'b1') &&
std.assertEqual(std.maxArray(['a2', 'b1'], keyF=function(x) x[1]), 'a2') &&
std.assertEqual(std.maxArray([], onEmpty='default'), 'default') &&


std.assertEqual(std.xor(true, false), true) &&
std.assertEqual(std.xor(true, true), false) &&

std.assertEqual(std.xnor(true, false), false) &&
std.assertEqual(std.xnor(true, true), true) &&

std.assertEqual(std.round(1.2), 1) &&
std.assertEqual(std.round(1.5), 2) &&

std.assertEqual(std.isEmpty(''), true) &&
std.assertEqual(std.isEmpty('non-empty string'), false) &&

std.assertEqual(std.contains([1, 2, 3], 2), true) &&
std.assertEqual(std.contains([1, 2, 3], 'foo'), false) &&

std.assertEqual(std.equalsIgnoreCase('foo', 'FOO'), true) &&
std.assertEqual(std.equalsIgnoreCase('foo', 'bar'), false) &&

std.assertEqual(std.isEven(10), true) &&
std.assertEqual(std.isEven(5), false) &&
std.assertEqual(std.isOdd(5), true) &&
std.assertEqual(std.isOdd(10), false) &&
std.assertEqual(std.isInteger(1), true) &&
std.assertEqual(std.isInteger(1.1), false) &&
std.assertEqual(std.isDecimal(1.1), true) &&
std.assertEqual(std.isDecimal(1), false) &&

std.assertEqual(std.remove([1, 2, 3], 2), [1, 3]) &&
std.assertEqual(std.removeAt([1, 2, 3], 1), [1, 3]) &&

std.assertEqual(std.objectRemoveKey({ foo: 1, bar: 2, baz: 3 }, 'foo'), { bar: 2, baz: 3 }) &&

std.assertEqual(std.trim('already trimmed string'), 'already trimmed string') &&
std.assertEqual(std.trim('    string with spaces on both ends     '), 'string with spaces on both ends') &&
std.assertEqual(std.trim('string with newline character at end\n'), 'string with newline character at end') &&
std.assertEqual(std.trim('string with tabs at end\t\t'), 'string with tabs at end') &&
// The last character here was previously expressed as \u00A0, but this is rewritten by jsonnetfmt.
// To avoid ambiguity but allow the code to pass unchanged through jsonnetfmt, this now uses a std.char() call.
std.assertEqual(std.trim('string with other special whitespaces at end\f\r\u0085' + std.char(160)), 'string with other special whitespaces at end') &&

true
