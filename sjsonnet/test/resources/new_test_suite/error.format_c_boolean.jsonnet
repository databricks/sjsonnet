// %c format conversion must reject boolean values, matching go-jsonnet behavior.
// go-jsonnet: RUNTIME ERROR: %c expected number / string, got: boolean
// jrsonnet:   RUNTIME ERROR: Format required a number, got boolean
"%c" % true
