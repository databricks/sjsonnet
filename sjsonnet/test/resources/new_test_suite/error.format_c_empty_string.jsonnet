// %c format conversion must reject empty strings, matching C++/go-jsonnet behavior.
// C++ jsonnet:  RUNTIME ERROR: %c expected 1-sized string got: 0
// go-jsonnet:   RUNTIME ERROR: %c expected 1-sized string got: 0
// jrsonnet:     runtime error: %c expected 1 char string, got 0
"%c" % ""
