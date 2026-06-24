// %c format conversion must reject multi-character strings, matching C++/go-jsonnet behavior.
// C++ jsonnet:  RUNTIME ERROR: %c expected 1-sized string got: 2
// go-jsonnet:   RUNTIME ERROR: %c expected 1-sized string got: 2
// jrsonnet:     runtime error: %c expected 1 char string, got 2
"%c" % "AB"
