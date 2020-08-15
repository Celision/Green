package parse

import "fmt"

/* nodes in the AST */

type Node interface {
	/* Pos() will return filepath, line, and column of the syntax object. */
	Pos() Pos
	aNode()
}

const PosMax = 1 << 30

type Pos struct {
	base      *PosBase
	line, col uint32
}

//represents base for relative position. filename:line:col
type PosBase struct {
	pos       Pos
	filename  string
	line, col uint32
}

func MakePos(base *PosBase, line, col uint) Pos { return Pos{base, sat32(line), sat32(col)} }

func (pos Pos) IsKnown() bool  { return pos.line > 0 }
func (pos Pos) Base() *PosBase { return pos.base }
func (pos Pos) Line() uint     { return uint(pos.line) }
func (pos Pos) Col() uint      { return uint(pos.col) }

func (pos Pos) RelFilename() string { return pos.base.Filename() }

func (pos Pos) RelLine() uint {
	b := pos.base
	if b.Line() == 0 {
		// base line is unknown => relative line is unknown
		return 0
	}
	return b.Line() + (pos.Line() - b.Pos().Line())
}

func (pos Pos) RelCol() uint {
	b := pos.base
	if b.Col() == 0 {
		// base column is unknown => relative column is unknown
		return 0
	}
	if pos.Line() == b.Pos().Line() {
		// pos on same line as pos base => column is relative to pos base
		return b.Col() + (pos.Col() - b.Pos().Col())
	}
	return pos.Col()
}

func (pos Pos) String() string {
	relf, rell, relc := pos.RelFilename(), pos.RelLine(), pos.RelCol()
	absf, absl, absc := pos.Base().Pos().RelFilename(), pos.Line(), pos.Col()
	s := makePositionalString(relf, rell, relc)
	if (relf != absf) || (rell != absl) || (relc != absc) {
		s += "[" + makePositionalString(absf, absl, absc) + "]"
	}
	return s
}

func makePositionalString(fn string, l, c uint) string {
	if l == 0 {
		if fn == "" {
			return "<unknown position>"
		}
		return fn
	}
	if c == 0 {
		return fmt.Sprintf("%s:%d", fn, l)
	}
	return fmt.Sprintf("%s:%d:%d", fn, l, c)
}

func NewFileBase(filename string) *PosBase {
	base := &PosBase{MakePos(nil, linebase, colbase), filename, linebase, colbase}
	base.pos.base = base
	return base
}

func NewLineBase(pos Pos, filename string, line, col uint) *PosBase {
	return &PosBase{pos, filename, sat32(line), sat32(col)}
}

func (base *PosBase) IsFileBase() bool {
	if base == nil {
		return false
	}
	return base.pos.base == base
}

func (base *PosBase) Pos() (_ Pos) {
	if base == nil {
		return
	}
	return base.pos
}

func (base *PosBase) Filename() string {
	if base == nil {
		return ""
	}
	return base.filename
}

func (base *PosBase) Line() uint {
	if base == nil {
		return 0
	}
	return uint(base.line)
}

func (base *PosBase) Col() uint {
	if base == nil {
		return 0
	}
	return uint(base.col)
}

func sat32(x uint) uint32 {
	if x > PosMax {
		return PosMax
	}
	return uint32(x)
}

type node struct {
	pos Pos
}

func (n *node) Pos() Pos { return n.pos }
func (n *node) aNode()   {}

type GFile struct {
	PkgName  *Name
	DeclList []Decl
	Lines    uint
	node
}

type (
	Decl interface {
		Node
		aDecl()
	}

	ImportDecl struct {
		Group        *Group //nil means not pqart of a group
		LocalPkgName *Name  //including "."; nil means no rename present
		Path         *BasicLit
		decl
	}

	ConstDecl struct {
		Group    *Group
		NameList []*Name
		Type     Expr //nil means no type
		Values   Expr //nil means no values
		decl
	}

	TypeDecl struct {
		Group *Group
		Name  *Name
		Alias bool
		Type  Expr
		decl
	}

	VarDecl struct {
		Group    *Group
		NameList []*Name
		Type     Expr
		Values   Expr
		decl
	}

	FuncDecl struct {
		Recv *Field //method/caller object. Nil means regular function.
		Name *Name
		Type *FuncType
		Body *BlockStmt //nil means no body (fwd declaration)
		decl
	}

	PolyStructDecl struct {
		Name *Name
		Type *PolyType
		decl
	}

	PolyEvalDecl struct {
		Name     *Name
		RecvName *Name          //name of PolyStruct type that can be the receiver
		Body     *BlockPolyStmt //nil means no body
		Type     *FuncType
		decl
	}
)

type decl struct{ node }

func (*decl) aDecl() {}

//All declarations belonging to the same group will point to the same Group node.
type Group struct {
	dummy int //not empty so we can have guaranteed different instances.
}

type (
	Expr interface {
		Node
		aExpr()
	}

	//placeholder for an expression failing to parse correctly and where no better one can be found.
	BadExpr struct {
		expr
	}

	Name struct {
		Value string
		expr
	}

	BasicLit struct {
		Value string
		Kind  LitKind
		Bad   bool //true means literal value has syntactic errors.
		expr
	}

	CompositeLit struct {
		Type     Expr //nil means no literal type
		ElemList []Expr
		NKeys    int
		Rbrace   Pos
		expr
	}

	KeyValueExpr struct {
		Key, Value Expr
		expr
	}

	FuncLit struct {
		Type *FuncType
		Body *BlockStmt
		expr
	}

	ParenExpr struct {
		X Expr
		expr
	}

	SelectorExpr struct {
		X   Expr
		Sel *Name
		expr
	}

	IndexExpr struct {
		X     Expr
		Index Expr
		expr
	}

	SliceExpr struct {
		X     Expr
		Index [3]Expr
		Full  bool
		expr
	}

	Operation struct {
		Op   Operator
		X, Y Expr //Y == nil means unary expression
		expr
	}

	CallExpr struct {
		Func    Expr
		ArgList []Expr //nil means no arguments
		HasDots bool   //last argument is followed by ellipses
		expr
	}

	ListExpr struct {
		ElemList []Expr
		expr
	}

	ArrayType struct {
		Len  Expr //nil means len is ellipses
		Elem Expr
		expr
	}

	SliceType struct {
		Elem Expr
		expr
	}

	DotsType struct {
		Elem Expr
		expr
	}

	StructType struct {
		FieldList []*Field
		//TagList   []*BasicLit //if i >= len(TagList) || TagList[i] == nil, then no tag for field i
		expr
	}

	Field struct {
		Name *Name //nil means anonymous field/parameter or embedded interface
		Type Expr  //field names declared in a list share the same type (identical pointers)
		node
	}

	PolyType struct {
		FieldList []*PolyField
		expr
	}

	PolyField struct {
		Name      *Name
		TupleType []Expr
		node
	}

	InterfaceType struct {
		MethodList []*Field
		expr
	}

	FuncType struct {
		ParamList  []*Field
		ResultList []*Field
		expr
	}

	MapType struct {
		Key, Value Expr
		expr
	}

	ChanType struct {
		Dir  ChanDir //0 means no direction
		Elem Expr
		expr
	}
)

type expr struct{ node }

func (*expr) aExpr() {}

type ChanDir uint

const (
	_ ChanDir = iota
	SendOnly
	RecvOnly
)

type (
	Stmt interface {
		Node
		aStmt()
	}

	SimpleStmt interface {
		Stmt
		aSimpleStmt()
	}

	EmptyStmt struct {
		simpleStmt
	}

	LabeledStmt struct {
		Label *Name
		Stmt  Stmt
		stmt
	}

	BlockStmt struct {
		List   []Stmt
		Rbrace Pos
		stmt
	}

	BlockPolyStmt struct {
		List   []PolyStmt
		Rbrace Pos
		stmt
	}

	PolyStmt struct {
		PType *Name
		Args  []*Name
		Block *BlockStmt
		stmt
	}

	ExprStmt struct {
		X Expr
		simpleStmt
	}

	SendStmt struct {
		Chan, Value Expr
		simpleStmt
	}

	DeclStmt struct {
		DeclList []Decl
		stmt
	}

	AssignStmt struct {
		Op       Operator //0 means no operation
		Lhs, Rhs Expr     //Rhs == ImplicitOne means Lhs++ (Op == Add) or Lhs-- (Op == Sub)
		simpleStmt
	}

	BranchStmt struct {
		Tok   token //Break, Continue, Fallthrough, or Goto
		Label *Name
		//Target is the continuation of the control folow after the branch
		//has been executed, and is computed by the parser if CheckBranches
		//is set (which is pretty much always). Target is *LabeledStmt for gotos,
		//and *SwitchStmt, *SelectStmt, or *ForStmt for breaks and continues
		//(depending on branch context). Target is unset for fallthroughs.
		Target Stmt
		stmt
	}

	CallStmt struct {
		Tok  token //CX/Green or Defer
		Call *CallExpr
		stmt
	}

	ReturnStmt struct {
		Results Expr //nil means no explicit return values.
		stmt
	}

	IfStmt struct {
		Init SimpleStmt
		Cond Expr
		Then *BlockStmt
		Else Stmt //either nil, *IfStmt, or *BlockStmt
		stmt
	}

	ForStmt struct {
		Init SimpleStmt //includes *RangeClause
		Cond Expr
		Post SimpleStmt
		Body *BlockStmt
		stmt
	}

	SwitchStmt struct {
		Init   SimpleStmt
		Tag    Expr
		Body   []*CaseClause
		Rbrace Pos
		stmt
	}

	SelectStmt struct {
		Body   []*CommClause
		Rbrace Pos
		stmt
	}
)

type (
	RangeClause struct {
		Lhs Expr //nil means no Lhs = or Lhs :=
		Def bool //is this :=?
		X   Expr //range X
		simpleStmt
	}

	CaseClause struct {
		Cases Expr //nil means default clause
		Body  []Stmt
		Colon Pos
		node
	}

	CommClause struct {
		Comm  SimpleStmt //send or receive stmt, nil means default clause
		Body  []Stmt
		Colon Pos
		node
	}
)

type stmt struct{ node }

func (stmt) aStmt() {}

type simpleStmt struct{ stmt }

func (simpleStmt) aSimpleStmt() {}
