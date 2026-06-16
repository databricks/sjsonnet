// %c format conversion must reject codepoints above 0x10FFFF, matching go-jsonnet behavior.
// go-jsonnet: RUNTIME ERROR: Invalid unicode codepoint, got 1.114112e+06
// jrsonnet:   RUNTIME ERROR: Invalid unicode codepoint, got: 1114112
"%c" % 1114112
