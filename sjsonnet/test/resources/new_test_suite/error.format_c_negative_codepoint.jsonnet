// %c format conversion must reject negative codepoints, matching go-jsonnet behavior.
// go-jsonnet: RUNTIME ERROR: Codepoints must be >= 0, got -1
// jrsonnet:   RUNTIME ERROR: Codepoints must be >=0, got: -1
"%c" % -1
