package parse

/* nodes in the AST */

type Node interface {
	/* Pos() will return filepath, line, and column of the syntax object. */
	Pos() Pos
	aNode()
}

type Pos struct {
	line, col uint32
	filepath  string
}

type node struct {
	pos Pos
}

func (n *node) Pos() Pos { return n.pos }
func (n *node) aNode()   {}
