package parse

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

/**
 * Code written and developed by Jonas Callister and Geoffrey Kublin.
 * Praxis Project, 2020. (c) All Rights Reserved.
 * Implementation based on Golang syntactic parser:
 * https://github.com/golang/go/tree/master/src/cmd/compile/internal/syntax
 *
 * parse.go - custom syntactic parser for the Green programming language.
 *
 * Why use a custom parser?
 * (1) ensure grammar is implemented correctly.
 * (2) Green grammar can be complex.
 * (3) Necessary for metalanguage capabilities.
 * (4) Mahjicka.
 */

type Mode uint

const (
	CheckBranches Mode = 1 << iota
)

//Error describes syntax error, implements error interface.
type Error struct {
	Pos Pos
	Msg string
}

func (err Error) Error() string {
	return fmt.Sprintf("%s: %s", err.Pos, err.Msg)
}

var _ error = Error{} //verifies Error implements error.

//Called for each error encountered in reading source file.
type ErrorHandler func(err error)

func Parse(base *PosBase, src io.Reader, errh ErrorHandler, mode Mode) (_ *GFile, first error) {
	defer func() {
		if p := recover(); p != nil {
			if err, ok := p.(Error); ok {
				first = err
				return
			}
			panic(p)
		}
	}()

	var p parser
	p.init(base, src, errh, mode)
	p.next()
	return p.fileOrNil(), p.first
}

// ParseFile behaves like Parse but it reads the source from the named file.
func ParseFile(filename string, errh ErrorHandler, mode Mode) (*GFile, error) {
	f, err := os.Open(filename)
	if err != nil {
		if errh != nil {
			errh(err)
		}
		return nil, err
	}
	defer f.Close()
	return Parse(NewFileBase(filename), f, errh, mode)
}

const debug = false
const trace = false

type parser struct {
	file *PosBase
	errh ErrorHandler
	mode Mode
	scanner

	base   *PosBase //current position base
	first  error    //first error encountered
	errcnt int      //number of errors encountered

	fnest  int    //function nesting level (for error handling)
	xnest  int    //expression nesting level (for composite literal ambiguity resolution)
	indent []byte //tracing support
}

func (p *parser) init(file *PosBase, r io.Reader, errh ErrorHandler, mode Mode) {
	p.file = file
	p.errh = errh
	p.mode = mode
	p.scanner.init(
		r,
		//since CX/Green supports no directive handling,
		//the error handler is very simplistic for the scanner.
		func(line, col uint, msg string) {
			p.errorAt(p.posAt(line, col), msg)
			return
		},
		StandardScanMode,
	)

	p.base = file
	p.first = nil
	p.errcnt = 0

	p.fnest = 0
	p.xnest = 0
	p.indent = nil
}

//updateBase sets current posbase to a new line base at pos.
func (p *parser) updateBase(pos Pos, tline, tcol uint, text string) {
	i, n, ok := trailingDigits(text)
	if i == 0 {
		return
	}

	if !ok {
		p.errorAt(p.posAt(tline, tcol+i), "invalid line number: "+text[i:])
		return
	}

	var line, col uint
	i2, n2, ok2 := trailingDigits(text[:i-1])
	if ok2 {
		i, i2 = i2, i
		line, col = n2, n
		if col == 0 || col > PosMax {
			p.errorAt(p.posAt(tline, tcol+i2), "invalid column number: "+text[i2:])
			return
		}
		text = text[:i2-1]
	} else {
		line = n
	}

	if line == 0 || line > PosMax {
		p.errorAt(p.posAt(tline, tcol+i), "invalid line number: "+text[i:])
		return
	}

	filename := text[:i-1]
	if filename == "" && ok2 {
		filename = p.base.Filename()
	}

	p.base = NewLineBase(pos, filename, line, col)
}

func commentText(s string) string {
	if s[:2] == "/*" {
		return s[2 : len(s)-2]
	}

	i := len(s)
	if s[i-1] == '\r' {
		i--
	}
	return s[2:i]
}

func trailingDigits(text string) (uint, uint, bool) {
	i := strings.LastIndex(text, ":")
	if i < 0 {
		return 0, 0, false
	}

	n, err := strconv.ParseUint(text[i+1:], 10, 0)
	return uint(i + 1), uint(n), err == nil
}

func (p *parser) got(tok token) bool {
	if p.tok == tok {
		p.next()
		return true
	}
	return false
}

func (p *parser) want(tok token) {
	if !p.got(tok) {
		p.syntaxError("expecting " + tokstring(tok))
		p.advance()
	}
}

//like got(_Assign) but also accepts _Define and reports error.
func (p *parser) gotAssign() bool {
	switch p.tok {
	case _Define:
		p.syntaxError("expecting =")
		fallthrough
	case _Assign:
		p.next()
		return true
	}
	return false
}

func (p *parser) posAt(line, col uint) Pos {
	return MakePos(p.base, line, col)
}

func (p *parser) errorAt(pos Pos, msg string) {
	err := Error{pos, msg}
	if p.first == nil {
		p.first = err
	}
	p.errcnt++
	if p.errh == nil {
		panic(p.first)
	}
	p.errh(err)
}

func (p *parser) syntaxErrorAt(pos Pos, msg string) {
	if trace {
		p.print("syntax error: " + msg)
	}

	if p.tok == _EOF && p.first != nil {
		return //avoid follow-up errors
	}

	switch {
	case msg == "":
		//nothing to do
	case strings.HasPrefix(msg, "in "), strings.HasPrefix(msg, "at "), strings.HasPrefix(msg, "after "):
		msg = " " + msg
	case strings.HasPrefix(msg, "expecting "):
		msg = ", " + msg
	default:
		//plain error - current token not needed
		p.errorAt(pos, "syntax error: "+msg)
		return
	}

	//get token string
	var tok string
	switch p.tok {
	case _Name, _Semi:
		tok = p.lit
	case _Literal:
		tok = "literal " + p.lit
	case _Operator:
		tok = p.op.String()
	case _AssignOp:
		tok = p.op.String() + "="
	case _IncOp:
		tok = p.op.String()
		tok += tok
	default:
		tok = tokstring(p.tok)
	}

	p.errorAt(pos, "syntax error: unexpected "+tok+msg)
}

//tokstring returns English-friendly version of token string.
func tokstring(tok token) string {
	switch tok {
	case _Comma:
		return "comma"
	case _Semi:
		return "semicolon or newline"
	}

	return tok.String()
}

func (p *parser) pos() Pos               { return p.posAt(p.line, p.col) }
func (p *parser) syntaxError(msg string) { p.syntaxErrorAt(p.pos(), msg) }

//Stopset contains keywords that start a statement.
//good places to sync things up especially for errors.
const stopset uint64 = 1<<_Break |
	1<<_Const |
	1<<_Continue |
	1<<_Defer |
	1<<_Fallthrough |
	1<<_For |
	1<<_Green |
	1<<_Goto |
	1<<_If |
	1<<_Return |
	1<<_Select |
	1<<_Switch |
	1<<_Type |
	1<<_Var

//advance consumes tokens until a token of the
//stopset or followlist is found.
//the stopset is only considered if we are inside a
//function, method, or poly definition. (p.fnest > 0).
//by the way...
/**
 * Poly, or polymorphic variables and evaluators, essentially
 * act as specialized unions, similar to how custom typing works
 * in functional languages that are ML-based.
 * Consider the following definition:
 *
 * poly Expr {
 *   Int(uint32)
 *   Ident(string)
 *   Primary($)
 *   Unary(string, $)
 *   Binary(string, $, $)
 * }
 *
 * The following is a polymorphic structure. Each field
 * is a possible named sub-type which any Expression instance
 * can be. For example,
 *   typeof(Expr{Int(1)}) == typeof(Expr{Ident("foo")})
 * Unfortunately, this means for polymorphic variables to be
 * useful, we need to define evaluators to construct them,
 * mutate them, and get their values from them.
 * So, we introduce polymorphic evaluators: specialized methods
 * which perform pattern matching on polymorphic variables.
 *
 * poly (Expr) mutate(y string) string {
 *   //instant pattern matching mode
 *   Int(i) -> { return sprintf("%s: %d", y, i) }
 *   Ident(s) -> { return sprintf("%s: %s", y, s) }
 *   Primary(x) -> { return x.mutate(y) }
 *   Unary(s, x) -> { return sprintf("%s%s", s, x.mutate(y)) }
 *   Binary(s, x1, x2) -> {
 *     y1 := x1.mutate(y)
 *     y2 := x2.mutate(y)
 *     return sprintf("%s %s %s", y1, s, y2)
 *   }
 * }
 *
 * Unlike methods, polymorphic evaluators cannot mutate the
 * variable they are operating on.
 *
 * For the sake of clarifying terminology:
 *  - polymorphic structures are the structural definition
 *    which a polymorphic variable may instantiate.
 *  - polymorphic variables are the instances of polymorphic
 *    structures. Polymorphic variables are immutable.
 *  - polymorphic evaluators act on polymorphic variables
 *    to perform useful action on them.
 *
 * For polymorphic structures ($ means its own type):
 * PolyStruct = "poly" identifier "{" { PolyField ";" } "}" .
 * PolyField  = identifier "(" PFTList ")" .
 * PFTList    = ("$" | Type ) { "," ( "$" | Type ) } .
 *
 * For polymorphic evaluators:
 * PolyEval     = "poly" "(" identifier ")" identifier Signature PolyBlock .
 * PolyBlock    = "{" PolyEvalList "}" .
 * PolyEvalList = { identifier "(" IdentifierList ")" "->" Block ";" }
 */
func (p *parser) advance(followlist ...token) {
	if trace {
		p.print(fmt.Sprintf("advance %s", followlist))
	}

	//compute followset.
	var followset uint64 = 1 << _EOF
	if len(followlist) > 0 {
		if p.fnest > 0 {
			followset |= stopset
		}
	}

	for !contains(followset, p.tok) {
		if trace {
			p.print("skip " + p.tok.String())
		}
		p.next()
		if len(followlist) == 0 {
			break
		}
	}

	if trace {
		p.print("next " + p.tok.String())
	}
}

//prints parser trace
func (p *parser) trace(msg string) func() {
	p.print(msg + " (")
	const tab = ". "
	p.indent = append(p.indent, tab...)
	return func() {
		p.indent = p.indent[:len(p.indent)-len(tab)]
		if x := recover(); x != nil {
			panic(x)
		}
		p.print(")")
	}
}

func (p *parser) print(msg string) {
	fmt.Printf("%5d: %s%s\n", p.line, p.indent, msg)
}

//-----------------------------------------------------------
//Declarations.

//Package shit.
//All parse methods are annotated by matching productions.
//Except for slice returns, methods named whateverOrNil will
//return nil sometimes. Everything else will always non-nil.
func (p *parser) fileOrNil() *GFile {
	if trace {
		defer p.trace("file")()
	}

	f := new(GFile)
	f.pos = p.pos()

	//PackageClause .
	if !p.got(_Package) {
		p.syntaxError("package statement must be first")
		return nil
	}
	f.PkgName = p.name()
	p.want(_Semi)

	//don't bother continuing if PackageClause has error
	if p.first != nil {
		return nil
	}

	//{ ImportDecl ";" } .
	for p.got(_Import) {
		f.DeclList = p.appendGroup(f.DeclList, p.importDecl)
		p.want(_Semi)
	}

	//{ TopLevelDecl ";" } .
	for p.tok != _EOF {
		switch p.tok {
		case _Const:
			p.next()
			f.DeclList = p.appendGroup(f.DeclList, p.constDecl)
		case _Type:
			p.next()
			f.DeclList = p.appendGroup(f.DeclList, p.typeDecl)
		case _Var:
			p.next()
			f.DeclList = p.appendGroup(f.DeclList, p.varDecl)
		case _Func:
			p.next()
			if d := p.funcDeclOrNil(); d != nil {
				f.DeclList = append(f.DeclList, d)
			}
		case _Poly:
			p.next()
			if d := p.polyStructOrEvalOrNil(); d != nil {
				f.DeclList = append(f.DeclList, d)
			}
		default:
			if p.tok == _Lbrace && len(f.DeclList) > 0 && isEmptyFuncDecl(f.DeclList[len(f.DeclList)-1]) {
				//opening { of function or poly declaration on the next line
				p.syntaxError("unexpcted semicolon or newline before {")
			} else {
				p.syntaxError("non-declaration statement outside function or poly body")
			}
			p.advance(_Const, _Type, _Var, _Func, _Poly)
			continue
		}

		if p.tok != _EOF && !p.got(_Semi) {
			p.syntaxError("after top level declaration")
			p.advance(_Const, _Type, _Var, _Func, _Poly)
		}
	}

	f.Lines = p.line

	return f
}

func isEmptyFuncDecl(dcl Decl) bool {
	f, ok := dcl.(*FuncDecl)
	return ok && f.Body == nil
}

//list parses possibly empty, sep-separated list, optionally followed by sep.
//open and close can be one of the pairs _Lparen, _Rparen or _Lbrace, _Rbrace.
//sep is one of _Comma or _Semi.
//for each element, f is called. Once it returns true, no more elements are
//accepted, and returns the position of the closing token.
//list = "(" { f sep } ")" | "{" { f sep } "}" .
func (p *parser) list(open, sep, close token, f func() bool) Pos {
	p.want(open)

	var done bool
	for p.tok != _EOF && p.tok != close && !done {
		done = f()
		//sep is optional before close
		if !p.got(sep) && p.tok != close {
			p.syntaxError(fmt.Sprintf("expecting %s or %s", tokstring(sep), tokstring(close)))
			p.advance(_Rparen, _Rbrack, _Rbrace)
			if p.tok != close {
				return p.pos()
			}
		}
	}

	pos := p.pos()
	p.want(close)
	return pos
}

//appendGroup(f) = f | "(" { f ";" } ")" .
func (p *parser) appendGroup(list []Decl, f func(*Group) Decl) []Decl {
	if p.tok == _Lparen {
		g := new(Group)
		p.list(_Lparen, _Semi, _Rparen, func() bool {
			list = append(list, f(g))
			return false
		})
	} else {
		list = append(list, f(nil))
	}

	if debug {
		for _, d := range list {
			if d == nil {
				panic("nil list entry")
			}
		}
	}

	return list
}

//ImportSpec = [ "." | PackageName ] ImportPath .
//ImportPath = string_lit .
func (p *parser) importDecl(group *Group) Decl {
	if trace {
		defer p.trace("importDecl")()
	}

	d := new(ImportDecl)
	d.pos = p.pos()
	d.Group = group

	switch p.tok {
	case _Name:
		d.LocalPkgName = p.name()
	case _Dot:
		d.LocalPkgName = p.newName(".")
		p.next()
	}
	d.Path = p.oliteral()
	if d.Path == nil {
		p.syntaxError("missing import path")
		p.advance(_Semi, _Rparen)
		return nil
	}

	return d
}

//ConstSpec = IdentifierList [ [Type] "=" ExpressionList ] .
func (p *parser) constDecl(group *Group) Decl {
	if trace {
		defer p.trace("constDecl")()
	}

	d := new(ConstDecl)
	d.pos = p.pos()
	d.Group = group

	d.NameList = p.nameList(p.name())
	if p.tok != _EOF && p.tok != _Semi && p.tok != _Rparen {
		d.Type = p.typeOrNil()
		if p.gotAssign() {
			d.Values = p.exprList()
		}
	}

	return d
}

//TypeSpec = identifier Type .
func (p *parser) typeDecl(group *Group) Decl {
	if trace {
		defer p.trace("typeDecl")()
	}

	d := new(TypeDecl)
	d.pos = p.pos()
	d.Group = group

	d.Name = p.name()
	//d.Alias =
	if p.gotAssign() {
		p.syntaxError("in type declaration [Green does not support alias!]")
		p.advance(_Semi, _Rparen)
	}
	d.Type = p.typeOrNil()
	if d.Type == nil {
		d.Type = p.badExpr()
		p.syntaxError("in type declaration")
		p.advance(_Semi, _Rparen)
	}

	return d
}

//VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
func (p *parser) varDecl(group *Group) Decl {
	if trace {
		defer p.trace("varDecl")()
	}

	d := new(VarDecl)
	d.pos = p.pos()
	d.Group = group

	d.NameList = p.nameList(p.name())
	if p.gotAssign() {
		d.Values = p.exprList()
	} else {
		d.Type = p.type_()
		if p.gotAssign() {
			d.Values = p.exprList()
		}
	}

	return d
}

//FunctionDecl = "func" FunctionName ( Function | Signature ) .
//FunctionName = identifier .
//Function	   = Signature FunctionBody .
//MethodDecl   = "func" Receiver MethodName ( Function | Signature ) .
//Receiver     = Parameters .
func (p *parser) funcDeclOrNil() *FuncDecl {
	if trace {
		defer p.trace("funcDecl")()
	}

	f := new(FuncDecl)
	f.pos = p.pos()

	if p.tok == _Lparen {
		rcvr := p.paramList()
		switch len(rcvr) {
		case 0:
			p.error("method has no receiver")
		default:
			p.error("method has multiple receivers")
			fallthrough
		case 1:
			f.Recv = rcvr[0]
		}
	}

	if p.tok != _Name {
		p.syntaxError("expecting name or (")
		p.advance(_Lbrace, _Semi)
		return nil
	}

	f.Name = p.name()
	f.Type = p.funcType()
	if p.tok == _Lbrace {
		f.Body = p.funcBody()
	}

	return f
}

func (p *parser) funcBody() *BlockStmt {
	p.fnest++
	errcnt := p.errcnt
	body := p.blockStmt("")
	p.fnest--

	//don't check branches if error found.
	if p.mode&CheckBranches != 0 && errcnt == p.errcnt {
		checkBranches(body, p.errh)
	}

	return body
}

func (p *parser) polyEvalBody() *BlockPolyStmt {
	p.fnest++
	body := p.blockPolyStmt()
	p.fnest--

	return body
}

func (p *parser) polyStructOrEvalOrNil() Decl {
	if trace {
		defer p.trace("polyStructOrEval")()
	}

	if p.tok == _Lparen {
		//then it's an eval
		return p.polyEvalOrNil()
	} else {
		return p.polyStructOrNil()
	}
}

//PolyEvalDecl = "poly" "(" PolyRecvName ")" PolyEvalName ( PolyEvalBody | Signature ) .
//PolyRecvName = identifier .
//PolyEvalName = identifier .
func (p *parser) polyEvalOrNil() *PolyEvalDecl {
	if trace {
		defer p.trace("polyEval")()
	}

	f := new(PolyEvalDecl)
	f.pos = p.pos()

	p.want(_Lparen)
	if p.tok != _Name {
		p.syntaxError("expecting name")
		p.advance(_Lbrace, _Semi)
		return nil
	}
	f.RecvName = p.name()
	p.want(_Rparen)

	f.Type = p.funcType()
	if p.tok == _Lbrace {
		f.Body = p.polyEvalBody()
	}

	return f
}

//PolyStructDecl = "poly" PolyStructName PolyType .
//PolyStructName = identifier .
func (p *parser) polyStructOrNil() *PolyStructDecl {
	if trace {
		defer p.trace("polyStructDecl")()
	}

	f := new(PolyStructDecl)
	f.pos = p.pos()

	if p.tok != _Name {
		p.syntaxError("expecting name")
		p.advance(_Lbrace, _Semi)
		return nil
	}

	f.Name = p.name()
	f.Type = p.polyType()

	return f
}

//----------------------------------------------------
//Expressions.

func (p *parser) expr() Expr {
	if trace {
		defer p.trace("expr")()
	}

	return p.binaryExpr(0)
}

//Expression = UnaryExpr | Expression binary_op Expression
func (p *parser) binaryExpr(prec int) Expr {
	//don't trace binaryExpr - leads to too much nexting in trace.
	x := p.unaryExpr()
	for (p.tok == _Operator || p.tok == _Star) && p.prec > prec {
		t := new(Operation)
		t.pos = p.pos()
		t.Op = p.op
		t.X = x
		tprec := p.prec
		p.next()
		t.Y = p.binaryExpr(tprec)
		x = t
	}

	return x
}

//UnaryExpr = PrimaryExpr | unary_op UnaryExpr
func (p *parser) unaryExpr() Expr {
	if trace {
		defer p.trace("unaryExpr")()
	}

	switch p.tok {
	case _Operator, _Star:
		switch p.op {
		case Mul, Add, Sub, Not, Xor:
			x := new(Operation)
			x.pos = p.pos()
			x.Op = p.op
			p.next()
			x.X = p.unaryExpr()
			return x
		case And:
			x := new(Operation)
			x.pos = p.pos()
			x.Op = And
			p.next()
			//unaryExpr may have returned a parenthesized composite literal
			//see comment in operand ----- remove parentheses if any
			x.X = unparen(p.unaryExpr())
			return x
		}
	case _Arrow:
		//receive op (<-x) or receive-only channel (<-chan E)
		pos := p.pos()
		p.next()

		//if the next token is _Chane we still can't determine if it's a
		//channel (<-chan int) or a receive operation (<-chan int(ch)).
		//Even with CX type conversion syntax, this is still true.
		//We only know once the end of the unaryExpr has been found.

		x := p.unaryExpr()

		//two cases:
		//1. <-chan...   => <-x is a channel type
		//2. <-x         => <-x is a receive operation
		//First case, <- must be re-associated with the preparsed channel type:
		//   <-(chan E)  => (<-chan E)
		//   <-(chan<-E) => (<-chan (<-E))

		if _, ok := x.(*ChanType); ok {
			//x is a channel type => reassociate <-
			dir := SendOnly
			t := x
			for dir == SendOnly {
				c, ok := t.(*ChanType)
				if !ok {
					break
				}
				dir = c.Dir
				if dir == RecvOnly {
					//t is type <-chn E but <-<-chan E is not permitted
					//(report same error as for "type _ <-<-chan E")
					p.syntaxError("unexpected <-, expecting chan")
					//no need for advance. Already progressed.
				}
				c.Dir = RecvOnly
				t = c.Elem
			}
			if dir == SendOnly {
				//channel dir is <- but channel element E is not a channel
				//(report same error as for "type _ <-chan<-E")
				p.syntaxError(fmt.Sprintf("unexpected %s, expecting chan", String(t)))
				//no advanc3
			}
			return x
		}

		//x is not a channel type => receive op!
		o := new(Operation)
		o.pos = pos
		o.Op = Recv
		o.X = x
		return o
	}

	return p.pexpr(true)
}

//callStmt parses call-esque statements preceded by defer or CX/Green
func (p *parser) callStmt() *CallStmt {
	if trace {
		defer p.trace("callStmt")()
	}

	s := new(CallStmt)
	s.pos = p.pos()
	s.Tok = p.tok //_Defer or _Green
	p.next()

	x := p.pexpr(p.tok == _Lparen) //keep_parens so we can report error.
	if t := unparen(x); t != x {
		p.errorAt(x.Pos(), fmt.Sprintf("expression in %s must not be parenthesized", s.Tok))
		//no advance
		x = t
	}

	cx, ok := x.(*CallExpr)
	if !ok {
		p.errorAt(x.Pos(), fmt.Sprintf("expression in %s must be function call", s.Tok))
		cx = new(CallExpr)
		cx.pos = x.Pos()
		cx.Func = x //assume common error of missing parentheses.
	}

	s.Call = cx
	return s
}

//Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
//Literal     = BasicLit | CompositeLit | FunctionLit .
//BasicLit    = int_lit | float_lit | string_lit .
//OperandName = identifier | QualifiedIdent .
func (p *parser) operand(keep_parens bool) Expr {
	if trace {
		defer p.trace("operand " + p.tok.String())()
	}

	switch p.tok {
	case _Name:
		return p.name()
	case _Literal:
		return p.oliteral()
	case _Lparen:
		pos := p.pos()
		p.next()
		p.xnest++
		x := p.expr()
		p.xnest--
		p.want(_Rparen)

		//Go devs say don't record the presence of ()'s unless absolutely
		//necessary for error reporting. I believe them.

		//Parentheses are not permitted on lhs of := .
		//Parentheses are not permitted around T in a composite literal T{}.
		//If next token is {, assume x is composite literal type T (it may not
		//be, } could be the opening brace of a block, but we don't know yet).
		if p.tok == _Lbrace {
			keep_parens = true
		}

		//also, Parens not permitted around the expression in green/cx/defer.
		if keep_parens {
			px := new(ParenExpr)
			px.pos = pos
			px.X = x
			x = px
		}
		return x
	case _Func:
		pos := p.pos()
		p.next()
		t := p.funcType()
		if p.tok == _Lbrace {
			p.xnest++
			f := new(FuncLit)
			f.pos = pos
			f.Type = t
			f.Body = p.funcBody()
			p.xnest--
			return f
		}
		return t
	case _Lbrack, _Chan, _Map, _Struct, _Interface:
		return p.type_()
	default:
		x := p.badExpr()
		p.syntaxError("expecting expression")
		p.advance(_Rparen, _Rbrack, _Rbrace)
		return x
	}

	//Syntactically, composite literals are operands. bc Complit type may be
	//qualified iden which is handled by pexpr() (together with selector
	//exprs), complits are parsed there too (operand only called from pexpr())
}

//PrimaryExpr = Operand |
//              PrimaryExpr Selector |
//              PrimaryExpr Index |
//              PrimaryExpr Slice |
//              PrimaryExpr Arguments .
//Selector    = "." identifier .
//Index       = "[" Expression "]" .
//Slice       = "[" ( [ Expression ] ":" [ Expression ] ) |
//                  ( [ Expression ] ":" Expression ":" Expression )
//              "]" .
//Arguments   = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
func (p *parser) pexpr(keep_parens bool) Expr {
	if trace {
		defer p.trace("pexpr")()
	}

	x := p.operand(keep_parens)

loop:
	for {
		pos := p.pos()
		switch p.tok {
		case _Dot:
			p.next()
			switch p.tok {
			case _Name:
				//pexpr '.' Sym
				t := new(SelectorExpr)
				t.pos = pos
				t.X = x
				t.Sel = p.name()
				x = t
			default:
				p.syntaxError("expecting name")
				p.advance(_Semi, _Rparen)
			}
		case _Lbrack:
			p.next()
			p.xnest++

			var i Expr
			if p.tok != _Colon {
				i = p.expr()
				if p.got(_Rbrack) {
					//x[i]
					t := new(IndexExpr)
					t.pos = pos
					t.X = x
					t.Index = i
					x = t
					p.xnest--
					break
				}
			}

			//x[i:...
			t := new(SliceExpr)
			t.pos = pos
			t.X = x
			t.Index[0] = i
			p.want(_Colon)
			if p.tok != _Colon && p.tok != _Rbrack {
				//x[i:j...
				t.Index[1] = p.expr()
			}
			if p.got(_Colon) {
				t.Full = true
				//x[i:j:...]
				if t.Index[1] == nil {
					p.error("middle index required in 3-index slice")
				}
				if p.tok != _Rbrack {
					//x[i:j:k...
					t.Index[2] = p.expr()
				} else {
					p.error("final index required in 3-index slice")
				}
			}
			p.want(_Rbrack)

			x = t
			p.xnest--
		case _Lparen:
			t := new(CallExpr)
			t.pos = pos
			t.Func = x
			t.ArgList, t.HasDots = p.argList()
			x = t
		case _Lbrace:
			//operand may have returned a parenthesized complit type;
			//accept but complain on complit
			t := unparen(x)
			//determine if { belongs to a complit or blockstmt
			complit_ok := false
			switch t.(type) {
			case *Name, *SelectorExpr:
				if p.xnest >= 0 {
					//x is considered complit type
					complit_ok = true
				}
			case *ArrayType, *SliceType, *StructType, *MapType, *PolyType:
				//x is for sure comptype
				complit_ok = true
			}
			if !complit_ok {
				break loop
			}
			if t != x {
				p.syntaxError("cannot parenthesize type in composite literal")
				//already progressed, no need to advance
			}
			n := p.complitexpr()
			n.Type = x
			x = n
		default:
			break loop
		}
	}

	return x
}

//Element = Expression | LiteralValue
func (p *parser) bare_complitexpr() Expr {
	if trace {
		defer p.trace("bare_complitexpr")()
	}

	if p.tok == _Lbrace {
		//'{' start_complit braced_keyval_list '}'
		return p.complitexpr()
	}

	return p.expr()
}

//LiteralValue = "{" [ ElementList [ "," ] ] "}" .
func (p *parser) complitexpr() *CompositeLit {
	if trace {
		defer p.trace("complitexpr")()
	}

	x := new(CompositeLit)
	x.pos = p.pos()

	p.xnest++
	x.Rbrace = p.list(_Lbrace, _Comma, _Rbrace, func() bool {
		e := p.bare_complitexpr()
		if p.tok == _Colon {
			//key ':' value
			l := new(KeyValueExpr)
			l.pos = p.pos()
			p.next()
			l.Key = e
			l.Value = p.bare_complitexpr()
			e = l
			x.NKeys++
		}
		x.ElemList = append(x.ElemList, e)
		return false
	})
	p.xnest--

	return x
}

//-------------------------------------------------------
//Types

func (p *parser) type_() Expr {
	if trace {
		defer p.trace("type_")()
	}

	typ := p.typeOrNil()
	if typ == nil {
		typ = p.badExpr()
		p.syntaxError("expecting type")
		p.advance(_Comma, _Colon, _Semi, _Rparen, _Rbrack, _Rbrace)
	}

	return typ
}

func newIndirect(pos Pos, typ Expr) Expr {
	o := new(Operation)
	o.pos = pos
	o.Op = Mul
	o.X = typ
	return o
}

//Type     = TypeName | TypeLit | "(" Type ")" .
//TypeName = identifier | QualifiedIdent .
//TypeLit  = ArrayType | StructType | PointerType | FunctionType |
//           InterfaceType | SliceType | MapType | Channel_Type | PolyType .
func (p *parser) typeOrNil() Expr {
	if trace {
		defer p.trace("typeOrNil")()
	}

	pos := p.pos()
	switch p.tok {
	case _Star:
		//ptr type
		p.next()
		return newIndirect(pos, p.type_())
	case _Arrow:
		//recv chan type
		p.next()
		p.want(_Chan)
		t := new(ChanType)
		t.pos = pos
		t.Dir = RecvOnly
		t.Elem = p.chanElem()
		return t
	case _Func:
		//fntype
		p.next()
		return p.funcType()
	case _Lbrack:
		//'[' oexpr ']' ntype
		//'[' _DotDotDot ']' ntype
		p.next()
		p.xnest++
		if p.got(_Rbrack) {
			//[]T
			p.xnest--
			t := new(SliceType)
			t.pos = pos
			t.Elem = p.type_()
			return t
		}

		//[n]T
		t := new(ArrayType)
		t.pos = pos
		if !p.got(_DotDotDot) {
			t.Len = p.expr()
		}
		p.want(_Rbrack)
		p.xnest--
		t.Elem = p.type_()
		return t
	case _Chan:
		//_Chan non_recvchantype
		//_Chan _Comm ntype
		p.next()
		t := new(ChanType)
		t.pos = pos
		if p.got(_Arrow) {
			t.Dir = SendOnly
		}
		t.Elem = p.chanElem()
		return t
	case _Map:
		//_Map '[' ntype ']' ntype
		p.next()
		p.want(_Lbrack)
		t := new(MapType)
		t.pos = pos
		t.Key = p.type_()
		p.want(_Rbrack)
		t.Value = p.type_()
		return t
	case _Struct:
		return p.structType()
	case _Interface:
		return p.interfaceType()
	case _Name:
		return p.dotname(p.name())
	case _Lparen:
		p.next()
		t := p.type_()
		p.want(_Rparen)
		return t
	}

	return nil
}

func (p *parser) funcType() *FuncType {
	if trace {
		defer p.trace("funcType")
	}

	typ := new(FuncType)
	typ.pos = p.pos()
	typ.ParamList = p.paramList()
	typ.ResultList = p.funcResult()

	return typ
}

func (p *parser) chanElem() Expr {
	if trace {
		defer p.trace("chanElem")()
	}

	typ := p.typeOrNil()
	if typ == nil {
		typ = p.badExpr()
		p.syntaxError("missing channel element type")
		//no advance -- assume element is actually just absent
	}

	return typ
}

func (p *parser) dotname(name *Name) Expr {
	if trace {
		defer p.trace("dotname")()
	}

	if p.tok == _Dot {
		s := new(SelectorExpr)
		s.pos = p.pos()
		p.next()
		s.X = name
		s.Sel = p.name()
		return s
	}

	return name
}

//StructType = "struct" "{" { FieldDecl ";" } "}" .
func (p *parser) structType() *StructType {
	if trace {
		defer p.trace("structType")()
	}

	typ := new(StructType)
	typ.pos = p.pos()

	p.want(_Struct)
	p.list(_Lbrace, _Semi, _Rbrace, func() bool {
		p.fieldDecl(typ)
		return false
	})

	return typ
}

//InterfaceType = "interface" "{" { MethodSpec ";" } "}" .
func (p *parser) interfaceType() *InterfaceType {
	if trace {
		defer p.trace("interfaceType")()
	}

	typ := new(InterfaceType)
	typ.pos = p.pos()

	p.want(_Interface)
	p.list(_Lbrace, _Semi, _Rbrace, func() bool {
		if m := p.methodDecl(); m != nil {
			typ.MethodList = append(typ.MethodList, m)
		}
		return false
	})

	return typ
}

//Result = Parameters | Type
func (p *parser) funcResult() []*Field {
	if trace {
		defer p.trace("funcResult")()
	}

	if p.tok == _Lparen {
		return p.paramList()
	}

	pos := p.pos()
	if typ := p.typeOrNil(); typ != nil {
		f := new(Field)
		f.pos = pos
		f.Type = typ
		return []*Field{f}
	}

	return nil
}

func (p *parser) addField(styp *StructType, pos Pos, name *Name, typ Expr) {
	f := new(Field)
	f.pos = pos
	f.Name = name
	f.Type = typ
	styp.FieldList = append(styp.FieldList, f)
}

//FieldDecl      = ( IdentifierList Type | AnonymousField )
//AnonymousField = [ "*" ] TypeName
func (p *parser) fieldDecl(styp *StructType) {
	if trace {
		defer p.trace("fieldDecl")()
	}

	pos := p.pos()
	switch p.tok {
	case _Name:
		name := p.name()
		if p.tok == _Dot || p.tok == _Semi || p.tok == _Rbrace {
			typ := p.qualifiedName(name)
			p.addField(styp, pos, nil, typ)
			return
		}

		names := p.nameList(name)
		typ := p.type_()

		for _, name := range names {
			p.addField(styp, name.Pos(), name, typ)
		}
	case _Star:
		p.next()
		typ := newIndirect(pos, p.qualifiedName(nil))
		p.addField(styp, pos, nil, typ)
	default:
		p.syntaxError("expecting field name or embedded type")
		p.advance(_Semi, _Rbrace)
	}
}

func (p *parser) oliteral() *BasicLit {
	if p.tok == _Literal {
		b := new(BasicLit)
		b.pos = p.pos()
		b.Value = p.lit
		b.Kind = p.kind
		b.Bad = p.bad
		p.next()
		return b
	}
	return nil
}

//MethodSpec        = MethodName Signature | InterfaceTypeName .
//MethodName        = identifier .
//InterfaceTypeName = TypeName .
func (p *parser) methodDecl() *Field {
	if trace {
		defer p.trace("methodDecl")()
	}

	switch p.tok {
	case _Name:
		name := p.name()

		//accept potential namelist but obviously complain
		hasNameList := false
		for p.got(_Comma) {
			p.name()
			hasNameList = true
		}
		if hasNameList {
			p.syntaxError("name list not allowed in interface type")
		}

		f := new(Field)
		f.pos = name.Pos()
		if p.tok != _Lparen {
			//packname
			f.Type = p.qualifiedName(name)
			return f
		}

		f.Name = name
		f.Type = p.funcType()
		return f
	default:
		p.syntaxError("expecting method or interface name")
		p.advance(_Semi, _Rbrace)
		return nil
	}
}

//ParameterDecl = [ IdentifierList ] [ "..." ] Type
func (p *parser) paramDeclOrNil() *Field {
	if trace {
		defer p.trace("paramDecl")
	}

	f := new(Field)
	f.pos = p.pos()

	switch p.tok {
	case _Name:
		f.Name = p.name()
		switch p.tok {
		case _Name, _Star, _Arrow, _Func, _Lbrack, _Chan, _Map, _Struct, _Interface, _Lparen:
			f.Type = p.type_()
		case _DotDotDot:
			f.Type = p.dotsType()
		case _Dot:
			f.Type = p.dotname(f.Name)
			f.Name = nil
		}
	case _Arrow, _Star, _Func, _Lbrack, _Chan, _Map, _Struct, _Interface, _Lparen:
		f.Type = p.type_()
	case _DotDotDot:
		f.Type = p.dotsType()
	default:
		p.syntaxError("expecting )")
		p.advance(_Comma, _Rparen)
		return nil
	}

	return f
}

// ...Type
func (p *parser) dotsType() *DotsType {
	if trace {
		defer p.trace("dotsType")()
	}

	t := new(DotsType)
	t.pos = p.pos()

	p.want(_DotDotDot)
	t.Elem = p.typeOrNil()
	if t.Elem == nil {
		t.Elem = p.badExpr()
		p.syntaxError("final argument in variadic function missing type")
	}

	return t
}

//Parameters    = "(" [ ParameterList [ "," ] ] ")" .
//ParameterList = ParameterDecl { "," ParameterDecl } .
func (p *parser) paramList() (list []*Field) {
	if trace {
		defer p.trace("paramList")()
	}

	pos := p.pos()

	var named int //number of params which have explicit name & type
	p.list(_Lparen, _Comma, _Rparen, func() bool {
		if par := p.paramDeclOrNil(); par != nil {
			if debug && par.Name == nil && par.Type == nil {
				panic("parameter without name or type")
			}
			if par.Name != nil && par.Type != nil {
				named++
			}
			list = append(list, par)
		}
		return false
	})

	//distribute param types
	if named == 0 {
		for _, par := range list {
			if typ := par.Name; typ != nil {
				par.Type = typ
				par.Name = nil
			}
		}
	} else if named != len(list) {
		ok := true
		var typ Expr
		for i := len(list) - 1; i >= 0; i-- {
			if par := list[i]; par.Type != nil {
				typ = par.Type
				if par.Name == nil {
					ok = false
					n := p.newName("_")
					n.pos = typ.Pos()
					par.Name = n
				}
			} else if typ != nil {
				par.Type = typ
			} else {
				//par.Type == nil && typ == nil -- only par.Name
				ok = false
				t := p.badExpr()
				t.pos = par.Name.Pos()
				par.Type = t
			}
		}
		if !ok {
			p.syntaxErrorAt(pos, "mixed named and unnamed function parameters")
		}
	}

	return
}

func (p *parser) badExpr() *BadExpr {
	b := new(BadExpr)
	b.pos = p.pos()
	return b
}

//PolyType = "{" { PolyField ";" } "}" .
func (p *parser) polyType() *PolyType {
	if trace {
		defer p.trace("polyType")()
	}

	typ := new(PolyType)
	typ.pos = p.pos()

	p.list(_Lbrace, _Semi, _Rbrace, func() bool {
		p.polyField(typ)
		return false
	})

	return typ
}

//PolyField     = PolyFieldName PolyTypeList .
//PolyFieldName = identifier .
//PolyTypeList  = "(" ( ( Type | "$" ) { "," ( "$" | Type ) } ) ")" .
func (p *parser) polyField(ptyp *PolyType) {
	if trace {
		defer p.trace("polyField")()
	}

	pos := p.pos()
	if p.tok != _Name {
		p.syntaxError("expected poly field name")
		p.advance(_Rbrace, _Semi)
		return
	}

	f := new(PolyField)
	f.pos = pos
	f.Name = p.name()
	f.TupleType = []Expr{}

	p.list(_Lparen, _Comma, _Rparen, func() bool {
		if p.tok == _Self {
			x := p.newName(f.Name.Value)
			f.TupleType = append(f.TupleType, x)
			//remember to consume _Self
			p.want(_Self)
		} else {
			x := p.type_()
			f.TupleType = append(f.TupleType, x)
		}
		return false
	})

	if len(f.TupleType) == 0 {
		f.TupleType = nil
	}

	ptyp.FieldList = append(ptyp.FieldList, f)
}

//-------------------------------------------------------
//Statements

//Golang is retarded and represents inc/dec as x (+/-)= ImplicitOne
var ImplicitOne = &BasicLit{Value: "1"}

//SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
func (p *parser) simpleStmt(lhs Expr, keyword token) SimpleStmt {
	if trace {
		defer p.trace("simpleStmt")()
	}

	if keyword == _For && p.tok == _Range {
		//_Range expr
		if debug && lhs != nil {
			panic("invalid call of simpleStmt")
		}
		return p.newRangeClause(nil, false)
	}

	if lhs == nil {
		lhs = p.exprList()
	}

	if _, ok := lhs.(*ListExpr); !ok && p.tok != _Assign && p.tok != _Define {
		pos := p.pos()
		switch p.tok {
		case _AssignOp:
			op := p.op
			p.next()
			return p.newAssignStmt(pos, op, lhs, p.expr())
		case _IncOp:
			op := p.op
			p.next()
			return p.newAssignStmt(pos, op, lhs, ImplicitOne)
		case _Arrow:
			s := new(SendStmt)
			s.pos = pos
			p.next()
			s.Chan = lhs
			s.Value = p.expr()
			return s
		default:
			s := new(ExprStmt)
			s.pos = lhs.Pos()
			s.X = lhs
			return s
		}
	}

	switch p.tok {
	case _Assign, _Define:
		pos := p.pos()
		var op Operator
		if p.tok == _Define {
			op = Def
		}
		p.next()

		if keyword == _For && p.tok == _Range {
			//expr_list := _Range expr
			return p.newRangeClause(lhs, op == Def)
		}

		rhs := p.exprList()

		return p.newAssignStmt(pos, op, lhs, rhs)
	default:
		p.syntaxError("expecting := or = or comma")
		p.advance(_Semi, _Rbrace)
		//do the best with what we got
		if x, ok := lhs.(*ListExpr); ok {
			lhs = x.ElemList[0]
		}
		s := new(ExprStmt)
		s.pos = lhs.Pos()
		s.X = lhs
		return s
	}
}

func (p *parser) newRangeClause(lhs Expr, def bool) *RangeClause {
	r := new(RangeClause)
	r.pos = p.pos()
	p.next()
	r.Lhs = lhs
	r.Def = def
	r.X = p.expr()
	return r
}

func (p *parser) newAssignStmt(pos Pos, op Operator, lhs, rhs Expr) *AssignStmt {
	a := new(AssignStmt)
	a.pos = pos
	a.Op = op
	a.Lhs = lhs
	a.Rhs = rhs
	return a
}

func (p *parser) labeledStmtOrNil(label *Name) Stmt {
	if trace {
		defer p.trace("labeledStmt")()
	}

	s := new(LabeledStmt)
	s.pos = p.pos()
	s.Label = label

	p.want(_Colon)

	if p.tok == _Rbrace {
		e := new(EmptyStmt)
		e.pos = p.pos()
		s.Stmt = e
		return s
	}

	s.Stmt = p.stmtOrNil()
	if s.Stmt != nil {
		return s
	}

	p.syntaxErrorAt(s.pos, "missing statement after label")
	return nil
}

//context must be non-empty unless we know p.tok == _Lbrace
func (p *parser) blockStmt(context string) *BlockStmt {
	if trace {
		defer p.trace("blockStmt")()
	}

	s := new(BlockStmt)
	s.pos = p.pos()

	//C nerds might forget their braces.
	if !p.got(_Lbrace) {
		p.syntaxError("expecting { after " + context)
		p.advance(_Name, _Rbrace)
		s.Rbrace = p.pos()
		if p.got(_Rbrace) {
			return s
		}
	}

	s.List = p.stmtList()
	s.Rbrace = p.pos()
	p.want(_Rbrace)

	return s
}

func (p *parser) blockPolyStmt() *BlockPolyStmt {
	if trace {
		defer p.trace("blockPolyStmt")()
	}

	s := new(BlockPolyStmt)
	s.pos = p.pos()

	//C nerds are stupid, so check for lbrace.
	if !p.got(_Lbrace) {
		p.syntaxError("expecting { after poly evaluator")
		p.advance(_Name, _Rbrace)
		s.Rbrace = p.pos()
		if p.got(_Rbrace) {
			return s
		}
	}

	s.List = p.polyStmtList()
	s.Rbrace = p.pos()
	p.want(_Rbrace)

	return s
}

func (p *parser) polyStmtList() (l []PolyStmt) {
	if trace {
		defer p.trace("polyStmtList")()
	}

	for p.tok != _EOF && p.tok != _Rbrace {
		s := p.polyStmtOrNil()
		l = append(l, s)
		//";" is optional before "}"
		if !p.got(_Semi) && p.tok != _Rbrace {
			p.syntaxError("at end of statement")
			p.advance(_Semi, _Rbrace)
			p.got(_Semi) //avoid spurious emptystmt
		}
	}
	return
}

func (p *parser) polyStmtOrNil() PolyStmt {
	if trace {
		defer p.trace("polyStmt")()
	}

	s := PolyStmt{}
	s.pos = p.pos()

	s.PType = p.name()
	//Args is guaranteed to be non-empty.
	s.Args = []*Name{}
	p.list(_Lparen, _Comma, _Rparen, func() bool {
		s.Args = append(s.Args, p.name())

		return false
	})

	p.want(_Generate)
	s.Block = p.blockStmt("poly clause")

	return s
}

func (p *parser) declStmt(f func(*Group) Decl) *DeclStmt {
	if trace {
		defer p.trace("declStmt")()
	}

	s := new(DeclStmt)
	s.pos = p.pos()

	p.next() //_Const, _Type, or _Var
	s.DeclList = p.appendGroup(nil, f)

	return s
}

func (p *parser) forStmt() Stmt {
	if trace {
		defer p.trace("forStmt")()
	}

	s := new(ForStmt)
	s.pos = p.pos()

	s.Init, s.Cond, s.Post = p.header(_For)
	s.Body = p.blockStmt("for clause")

	return s
}

func (p *parser) header(keyword token) (init SimpleStmt, cond Expr, post SimpleStmt) {
	p.want(keyword)

	if p.tok == _Lbrace {
		if keyword == _If {
			p.syntaxError("missing condition in if statement")
		}
		return
	}

	outer := p.xnest
	p.xnest = -1

	if p.tok != _Semi {
		//accept potential varDecl but complain, duh
		if p.got(_Var) {
			p.syntaxError(fmt.Sprintf("var declaration not allowed in %s initializer", keyword.String()))
		}
		init = p.simpleStmt(nil, keyword)
		//if rangeClause, then we're good.
		if _, ok := init.(*RangeClause); ok {
			p.xnest = outer
			return
		}
	}

	var condStmt SimpleStmt
	var semi struct {
		pos Pos
		lit string
	}
	if p.tok != _Lbrace {
		if p.tok == _Semi {
			semi.pos = p.pos()
			semi.lit = p.lit
			p.next()
		} else {
			//Golang devs say this works better
			p.want(_Lbrace)
			if p.tok != _Lbrace {
				p.advance(_Lbrace, _Rbrace)
			}
		}
		if keyword == _For {
			if p.tok != _Semi {
				if p.tok == _Lbrace {
					p.syntaxError("expecting for loop condition")
					goto done
				}
				condStmt = p.simpleStmt(nil, 0)
			}
			p.want(_Semi)
			if p.tok != _Lbrace {
				post = p.simpleStmt(nil, 0)
				if a, _ := post.(*AssignStmt); a != nil && a.Op == Def {
					p.syntaxErrorAt(a.Pos(), "cannot declare in post statement of for loop")
				}
			}
		} else if p.tok != _Lbrace {
			condStmt = p.simpleStmt(nil, keyword)
		}
	} else {
		condStmt = init
		init = nil
	}

done:
	switch s := condStmt.(type) {
	case nil:
		if keyword == _If && semi.pos.IsKnown() {
			if semi.lit != "semicolon" {
				p.syntaxErrorAt(semi.pos, fmt.Sprintf("unexpected %s, expecting { after if clause", semi.lit))
			} else {
				p.syntaxErrorAt(semi.pos, "missing condition in if statement")
			}
		}
	case *ExprStmt:
		cond = s.X
	default:
		//common syntax error is '=' instead of '=='.
		var str string
		if as, ok := s.(*AssignStmt); ok && as.Op == 0 {
			str = "assignment (" + String(as.Lhs) + ") = (" + String(as.Rhs) + ")"
		} else {
			str = String(s)
		}
		p.syntaxErrorAt(s.Pos(), fmt.Sprintf("cannot use %s as value", str))
	}

	p.xnest = outer
	return
}

func (p *parser) ifStmt() *IfStmt {
	if trace {
		defer p.trace("ifStmt")()
	}

	s := new(IfStmt)
	s.pos = p.pos()

	s.Init, s.Cond, _ = p.header(_If)
	s.Then = p.blockStmt("if clause")

	if p.got(_Else) {
		switch p.tok {
		case _If:
			s.Else = p.ifStmt()
		case _Lbrace:
			s.Else = p.blockStmt("")
		default:
			p.syntaxError("else must be followed by if or statement block")
			p.advance(_Name, _Rbrace)
		}
	}

	return s
}

func (p *parser) switchStmt() *SwitchStmt {
	if trace {
		defer p.trace("switchStmt")
	}

	s := new(SwitchStmt)
	s.pos = p.pos()

	s.Init, s.Tag, _ = p.header(_Switch)

	if !p.got(_Lbrace) {
		p.syntaxError("missing { after switch clause")
		p.advance(_Case, _Default, _Rbrace)
	}
	for p.tok != _EOF && p.tok != _Rbrace {
		s.Body = append(s.Body, p.caseClause())
	}
	s.Rbrace = p.pos()
	p.want(_Rbrace)

	return s
}

func (p *parser) selectStmt() *SelectStmt {
	if trace {
		defer p.trace("selectStmt")()
	}

	s := new(SelectStmt)
	s.pos = p.pos()

	p.want(_Select)
	if !p.got(_Lbrace) {
		p.syntaxError("missing { after select clause")
		p.advance(_Case, _Default, _Rbrace)
	}
	for p.tok != _EOF && p.tok != _Rbrace {
		s.Body = append(s.Body, p.commClause())
	}
	s.Rbrace = p.pos()
	p.want(_Rbrace)

	return s
}

func (p *parser) caseClause() *CaseClause {
	if trace {
		defer p.trace("caseClause")()
	}

	c := new(CaseClause)
	c.pos = p.pos()

	switch p.tok {
	case _Case:
		p.next()
		c.Cases = p.exprList()
	case _Default:
		p.next()
	default:
		p.syntaxError("expecting case or default or }")
		p.advance(_Colon, _Case, _Default, _Rbrace)
	}

	c.Colon = p.pos()
	p.want(_Colon)
	c.Body = p.stmtList()

	return c
}

func (p *parser) commClause() *CommClause {
	if trace {
		defer p.trace("commClause")()
	}

	c := new(CommClause)
	c.pos = p.pos()

	switch p.tok {
	case _Case:
		p.next()
		c.Comm = p.simpleStmt(nil, 0)
		//invalid subtrees for c.Comm are flagged during typechecking.
		//yes, I know. Golang is retarded.
	case _Default:
		p.next()
	default:
		p.syntaxError("expecting case or default or }")
		p.advance(_Colon, _Case, _Default, _Rbrace)
	}

	c.Colon = p.pos()
	p.want(_Colon)
	c.Body = p.stmtList()

	return c
}

//Statement = Declaration | LabeledStmt | SimpleStmt | GreenStmt | ReturnStmt |
//            BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt | Block |
//            IfStmt | SwitchStmt | SelectStmt | ForStmt | DeferStmt .
func (p *parser) stmtOrNil() Stmt {
	if trace {
		defer p.trace("stmt " + p.tok.String())
	}

	//most statements ever will begin with identifier. do this first to optimize.
	if p.tok == _Name {
		lhs := p.exprList()
		if label, ok := lhs.(*Name); ok && p.tok == _Colon {
			return p.labeledStmtOrNil(label)
		}
		return p.simpleStmt(lhs, 0)
	}

	switch p.tok {
	case _Var:
		return p.declStmt(p.varDecl)
	case _Const:
		return p.declStmt(p.constDecl)
	case _Type:
		return p.declStmt(p.typeDecl)
	}

	switch p.tok {
	case _Lbrace:
		return p.blockStmt("")
	case _Operator, _Star:
		switch p.op {
		case Add, Sub, Mul, And, Xor, Not:
			return p.simpleStmt(nil, 0)
		}
	case _Literal, _Func, _Lparen, _Lbrack, _Struct, _Map, _Chan, _Interface, _Arrow:
		return p.simpleStmt(nil, 0)
	case _For:
		return p.forStmt()
	case _Switch:
		return p.switchStmt()
	case _Select:
		return p.selectStmt()
	case _If:
		return p.ifStmt()
	case _Fallthrough:
		s := new(BranchStmt)
		s.pos = p.pos()
		p.next()
		s.Tok = _Fallthrough
		return s
	case _Break, _Continue:
		s := new(BranchStmt)
		s.pos = p.pos()
		s.Tok = p.tok
		p.next()
		if p.tok == _Name {
			s.Label = p.name()
		}
		return s
	case _Green, _Defer:
		return p.callStmt()
	case _Goto:
		s := new(BranchStmt)
		s.pos = p.pos()
		s.Tok = _Goto
		p.next()
		s.Label = p.name()
		return s
	case _Return:
		s := new(ReturnStmt)
		s.pos = p.pos()
		p.next()
		if p.tok != _Semi && p.tok != _Rbrace {
			s.Results = p.exprList()
		}
		return s
	case _Semi:
		s := new(EmptyStmt)
		s.pos = p.pos()
		return s
	}

	return nil
}

//StatementList = { Statement ";" }
func (p *parser) stmtList() (l []Stmt) {
	if trace {
		defer p.trace("stmtList")()
	}

	for p.tok != _EOF && p.tok != _Rbrace && p.tok != _Case && p.tok != _Default {
		s := p.stmtOrNil()
		if s == nil {
			break
		}
		l = append(l, s)
		// ";" is optional before "}"
		if !p.got(_Semi) && p.tok != _Rbrace {
			p.syntaxError("at end of statement")
			p.advance(_Semi, _Rbrace, _Case, _Default)
			p.got(_Semi) // avoid spurious empty statement
		}
	}
	return
}

//Arguments = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
func (p *parser) argList() (list []Expr, hasDots bool) {
	if trace {
		defer p.trace("argList")()
	}

	p.xnest++
	p.list(_Lparen, _Comma, _Rparen, func() bool {
		list = append(list, p.expr())
		hasDots = p.got(_DotDotDot)
		return hasDots
	})
	p.xnest--

	return
}

//Common productions ;)

func (p *parser) newName(val string) *Name {
	n := new(Name)
	n.pos = p.pos()
	n.Value = val
	return n
}

func (p *parser) name() *Name {
	//tracing ignored. would be *TOO* verbose.

	if p.tok == _Name {
		n := p.newName(p.lit)
		p.next()
		return n
	}

	n := p.newName("_")
	p.syntaxError("expecting name")
	p.advance()
	return n
}

//IdentifierList = identifier { "," identifier } .
func (p *parser) nameList(first *Name) []*Name {
	if trace {
		defer p.trace("nameList")()
	}

	if debug && first == nil {
		panic("first name not provided")
	}

	l := []*Name{first}
	for p.got(_Comma) {
		l = append(l, p.name())
	}

	return l
}

func (p *parser) qualifiedName(name *Name) Expr {
	if trace {
		defer p.trace("qualifiedName")()
	}

	switch {
	case name != nil:
		//name is provided; OK
	case p.tok == _Name:
		name = p.name()
	default:
		name = p.newName("_")
		p.syntaxError("expecting name")
		p.advance(_Dot, _Semi, _Rbrace)
	}

	return p.dotname(name)
}

//ExpressionList = Expression { "," Expression } .
func (p *parser) exprList() Expr {
	if trace {
		defer p.trace("exprList")()
	}

	x := p.expr()
	if p.got(_Comma) {
		list := []Expr{x, p.expr()}
		for p.got(_Comma) {
			list = append(list, p.expr())
		}
		t := new(ListExpr)
		t.pos = x.Pos()
		t.ElemList = list
		x = t
	}
	return x
}

//unparen removes all parentheses around an expr.
func unparen(x Expr) Expr {
	for {
		p, ok := x.(*ParenExpr)
		if !ok {
			break
		}
		x = p.X
	}
	return x
}
