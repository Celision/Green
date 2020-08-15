package main

import (
	"os"

	"github.com/PraxisProject/Green/green-go/parse"
)

func main() {
	var FILE string = "test.cx"
	ast, err := parse.ParseFile(FILE, func(err error) {
		panic(err.Error())
	}, 0)
	if err != nil {
		panic(err.Error())
	}
	parse.Fdump(os.Stdout, ast)
	//os.Stdout
}
