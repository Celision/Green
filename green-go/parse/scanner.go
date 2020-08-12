package parse

//As mentioned in the scanner.go file for Golang, this file, source.go,
//tokens.go, and token_string.go are self-contained and thus should be
//made into their own package. Such will happen in a date soon enough.
type scanner struct {
	source
}
