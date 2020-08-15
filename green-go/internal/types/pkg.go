package types

import (
	"fmt"

	"github.com/PraxisProject/Green/green-go/internal/utils"
)

var pkgMap = make(map[string]*Pkg) //maps package path to a package

const MaxPkgHeight = 1e9

type Pkg struct {
	Path   string //string lit used in import statement
	Name   string //name of package
	Prefix string //escaped path for use in symbol table
	Syms   map[string]*Sym
	//Pathsym *obj.LSym <- might not be needed for now.

	//package's height in import graph. Leaves are 0, 1 + max height of imports for rest.
	Height int

	Imported bool //export data of this package was parsed
	Direct   bool //imported directly
}

func NewPkg(path, name string) *Pkg {
	if p := pkgMap[path]; p != nil {
		if name != "" && p.Name != name {
			panic(fmt.Sprintf("conflicting package names %s and %s for path %q", p.Name, name, path))
		}
		return p
	}

	p := new(Pkg)
	p.Path = path
	p.Name = name
	p.Prefix = utils.PathToPrefix(path)
	p.Syms = make(map[string]*Sym)
	pkgMap[path] = p

	return p
}
