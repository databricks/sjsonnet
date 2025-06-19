std.assertEqual(std.native('regexFullMatch')(@'e', 'hello'), null) &&

std.assertEqual(
  std.native('regexFullMatch')(@'h.*o', 'hello'),
  {
    string: 'hello',
    captures: [],
    namedCaptures: {},
  }
) &&

std.assertEqual(
  std.native('regexFullMatch')(@'h(.*)o', 'hello'),
  {
    string: 'hello',
    captures: ['ell'],
    namedCaptures: {},
  }
) &&

std.assertEqual(
  std.native('regexFullMatch')(@'h(?P<mid>.*)o', 'hello'),
  {
    string: 'hello',
    captures: ['ell'],
    namedCaptures: {
      mid: 'ell',
    },
  }
) &&

std.assertEqual(std.native('regexPartialMatch')(@'world', 'hello'), null) &&

std.assertEqual(
  std.native('regexPartialMatch')(@'e', 'hello'),
  {
    string: 'hello',
    captures: [],
    namedCaptures: {},
  }
) &&

std.assertEqual(
  std.native('regexPartialMatch')(@'e(.*)o', 'hello'),
  {
    string: 'hello',
    captures: ['ll'],
    namedCaptures: {},
  }
) &&

std.assertEqual(
  std.native('regexPartialMatch')(@'e(?P<mid>.*)o', 'hello'),
  {
    string: 'hello',
    captures: ['ll'],
    namedCaptures: {
      mid: 'll',
    },
  }
) &&

std.assertEqual(std.native('regexQuoteMeta')(@'1.5-2.0?'), '\\Q1.5-2.0?\\E') &&


std.assertEqual(std.native('regexReplace')('wishyfishyisishy', @'ish', 'and'), 'wandyfishyisishy') &&
std.assertEqual(std.native('regexReplace')('yabba dabba doo', @'b+', 'd'), 'yada dabba doo') &&

std.assertEqual(std.native('regexGlobalReplace')('wishyfishyisishy', @'ish', 'and'), 'wandyfandyisandy') &&
std.assertEqual(std.native('regexGlobalReplace')('yabba dabba doo', @'b+', 'd'), 'yada dada doo') &&

true