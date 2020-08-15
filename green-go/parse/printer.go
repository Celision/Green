package parse

import (
	"bytes"
	"io"
	"strings"
)

func Fprint(w io.Writer, x Node, linebreaks bool) (n int, err error) {
	p := printer{
		output:     w,
		linebreaks: linebreaks,
	}

	defer func() {
		n = p.written
		if e := recover(); e != nil {
			err = e.(localError).err
		}
	}()

	p.print(x)
	p.flush(_EOF)

	return
}

func String(n Node) string {
	var buf bytes.Buffer
	_, err := Fprint(&buf, n, false)
	if err != nil {
		panic(err)
	}
	return buf.String()
}

type ctrlSymbol int

const (
	none ctrlSymbol = iota
	semi
	blank
	newline
	indent
	outdent
)

type whitespace struct {
	last token
	kind ctrlSymbol
}

type printer struct {
	output     io.Writer
	written    int          //# of bytes written
	linebreaks bool         //print linebreaks instead of semis
	indent     int          //current indentation level
	nlcount    int          //# of consecutive newlines
	pending    []whitespace //pending whitespace
	lastTok    token        //last token (after any pending semi) processed by print
}

//write is thin wrapper around p.output.Write taking care of accounting and error handling
func (p *printer) write(data []byte) {
	n, err := p.output.Write(data)
	p.written += n
	if err != nil {
		panic(localError{err})
	}
}

var (
	tabBytes    = []byte("\t\t\t\t\t\t\t\t")
	newlineByte = []byte("\n")
	blankByte   = []byte(" ")
)

func (p *printer) writeBytes(data []byte) {
	if len(data) == 0 {
		panic("expected non-empty []byte")
	}
	if p.nlcount > 0 && p.indent > 0 {
		//write indentation
		n := p.indent
		for n > len(tabBytes) {
			p.write(tabBytes)
			n -= len(tabBytes)
		}
		p.write(tabBytes[:n])
	}
	p.write(data)
	p.nlcount = 0
}

func (p *printer) writeString(s string) {
	p.writeBytes([]byte(s))
}

func impliesSemi(tok token) bool {
	switch tok {
	case _Name, _Break, _Continue, _Fallthrough, _Return, _Rparen, _Rbrack, _Rbrace:
		return true
	}
	return false
}

func lineComment(text string) bool {
	return strings.HasPrefix(text, "//")
}

func (p *printer) addWhitespace(kind ctrlSymbol, text string) {
	p.pending = append(p.pending, whitespace{p.lastTok, kind})
	switch kind {
	case semi:
		p.lastTok = _Semi
	case newline:
		p.lastTok = 0
	}
}

func (p *printer) flush(next token) {
	sawNewLine := next == _EOF
	sawParen := next == _Rparen || next == _Rbrace
	for i := len(p.pending) - 1; i >= 0; i-- {
		switch p.pending[i].kind {
		case semi:
			k := semi
			if sawParen {
				sawParen = false
				k = none
			} else if sawNewLine && impliesSemi(p.pending[i].last) {
				sawNewLine = false
				k = none
			}
			p.pending[i].kind = k
		case newline:
			sawNewLine = true
		}
	}

	prev := none
	for i := range p.pending {
		switch p.pending[i].kind {
		case semi:
			p.writeString(";")
			p.nlcount = 0
			prev = semi
		case blank:
			if prev != blank {
				p.writeBytes(blankByte)
				p.nlcount = 0
				prev = blank
			}
		case newline:
			if p.nlcount <= 1 {
				p.write(newlineByte)
				p.nlcount++
				prev = newline
			}
		case indent:
			p.indent++
		case outdent:
			p.indent--
			if p.indent < 0 {
				panic("negative indentation")
			}
		}
	}

	p.pending = p.pending[:0]
}

func (p *printer) print(args ...interface{}) {
	for i := 0; i < len(args); i++ {
		switch x := args[i].(type) {
		case nil:
		case Node:
			p.printNode(x)
		case token:
			var s string
			if x == _Name {
				i++
				if i >= len(args) {
					panic("missing string argument after _Name")
				}
				s = args[i].(string)
			} else {
				s = x.String()
			}

			if x == _Semi {
				p.addWhitespace(semi, "")
			} else {
				p.flush(x)
				p.writeString(s)
				p.nlcount = 0
				p.lastTok = x
			}
		case Operator:
			if x != 0 {
				p.flush(_Operator)
				p.writeString(x.String())
			}
		case ctrlSymbol:
			switch x {
			case newline:
				if !p.linebreaks {
					x = blank
				}
				p.addWhitespace(x, "")
			}
		}
	}
}

func (p *printer) printNode(n Node) {
	switch n := n.(type) {
	case *BadExpr:
		p.print(_Name, "<bad expr>")
	case *Name:
		p.print(_Name, n.Value)
	case *BasicLit:
		p.print(_Name, n.Value)
	case *FuncLit:
		p.print(n.Type, blank, n.Body)
	case *CompositeLit:
		if n.Type != nil {
			p.print(n.Type)
		}
		p.print(_Lbrace)
		if n.NKeys > 0 && n.NKeys == len(n.ElemList) {
			p.printExprLines(n.ElemList)
		} else {
			p.printExprList(n.ElemList)
		}
		p.print(_Rbrace)
	case *ParenExpr:
		p.print(_Lparen, n.X, _Rparen)
	case *SelectorExpr:
		p.print(n.X, _Dot, n.Sel)
	case *IndexExpr:
		p.print(n.X, _Lbrack, n.Index, _Rbrack)
	case *SliceExpr:
		p.print(n.X, _Lbrack)
		if i := n.Index[0]; i != nil {
			p.printNode(i)
		}
		p.print(_Colon)
		if j := n.Index[1]; j != nil {
			p.printNode(j)
		}
		if k := n.Index[2]; k != nil {
			p.print(_Colon, k)
		}
	case *CallExpr:
		p.print(n.Func, _Lparen)
		p.printExprList(n.ArgList)
		if n.HasDots {
			p.print(_DotDotDot)
		}
		p.print(_Rparen)
	case *Operation:
		if n.Y == nil {
			p.print(n.Op, n.X)
		} else {
			p.print(n.X, blank, n.Op, blank, n.Y)
		}
	case *KeyValueExpr:
		p.print(n.Key, _Colon, blank, n.Value)
	case *ListExpr:
		p.printExprList(n.ElemList)
	case *ArrayType:
		var len interface{} = _DotDotDot
		if n.Len != nil {
			len = n.Len
		}
		p.print(_Lbrack, len, _Rbrack, n.Elem)
	case *SliceType:
		p.print(_Lbrack, _Rbrack, n.Elem)
	case *DotsType:
		p.print(_DotDotDot, n.Elem)
	case *StructType:
		p.print(_Struct)
		if len(n.FieldList) > 0 && p.linebreaks {
			p.print(blank)
		}
		p.print(_Lbrace)
		if len(n.FieldList) > 0 {
			p.print(newline, indent)
			p.printFieldList(n.FieldList)
			p.print(outdent, newline)
		}
		p.print(_Rbrace)
	case *FuncType:
		p.print(_Func)
		p.printSignature(n)
	case *InterfaceType:
		p.print(_Interface)
		if len(n.MethodList) > 0 && p.linebreaks {
			p.print(blank)
		}
		p.print(_Lbrace)
		if len(n.MethodList) > 0 {
			p.print(newline, indent)
			p.printMethodList(n.MethodList)
			p.print(outdent, newline)
		}
		p.print(_Rbrace)
	case *MapType:
		p.print(_Map, _Lbrack, n.Key, _Rbrack, n.Value)
	case *ChanType:
		if n.Dir == RecvOnly {
			p.print(_Arrow)
		}
		p.print(_Chan)
		if n.Dir == SendOnly {
			p.print(_Arrow)
		}
		p.print(blank, n.Elem)
	case *DeclStmt:
		p.printDecl(n.DeclList)
	case *EmptyStmt:
	case *LabeledStmt:
		p.print(outdent, n.Label, _Colon, indent, newline, n.Stmt)
	case *ExprStmt:
		p.print(n.X)
	case *SendStmt:
		p.print(n.Chan, blank, _Arrow, blank, n.Value)
	case *AssignStmt:
		p.print(n.Lhs)
		if n.Rhs == ImplicitOne {
			p.print(n.Op, n.Op)
		} else {
			p.print(blank, n.Op, _Assign, blank)
			p.print(n.Rhs)
		}
	case *CallStmt:
		p.print(n.Tok, blank, n.Call)
	case *ReturnStmt:
		p.print(_Return)
		if n.Results != nil {
			p.print(blank, n.Results)
		}
	case *BranchStmt:
		p.print(n.Tok)
		if n.Label != nil {
			p.print(blank, n.Label)
		}
	case *BlockStmt:
		p.print(_Lbrace)
		if len(n.List) > 0 {
			p.print(newline, indent)
			p.printStmtList(n.List, true)
			p.print(outdent, newline)
		}
		p.print(_Rbrace)
	case *BlockPolyStmt:
		p.print(_Lbrace)
		if len(n.List) > 0 {
			p.print(newline, indent)
			p.printPolyStmtList(n.List, true)
			p.print(outdent, newline)
		}
		p.print(_Rbrace)
	case *IfStmt:
		p.print(_If, blank)
		if n.Init != nil {
			p.print(n.Init, _Semi, blank)
		}
		p.print(n.Cond, blank, n.Then)
		if n.Else != nil {
			p.print(blank, _Else, blank, n.Else)
		}
	case *SwitchStmt:
		p.print(_Switch, blank)
		if n.Init != nil {
			p.print(n.Init, _Semi, blank)
		}
		if n.Tag != nil {
			p.print(n.Tag, blank)
		}
		p.printSwitchBody(n.Body)
	case *SelectStmt:
		p.print(_Select, blank)
		p.printSelectBody(n.Body)
	case *RangeClause:
		if n.Lhs != nil {
			tok := _Assign
			if n.Def {
				tok = _Define
			}
			p.print(n.Lhs, blank, tok, blank)
		}
		p.print(_Range, blank, n.X)
	case *ForStmt:
		p.print(_For, blank)
		if n.Init == nil && n.Post == nil {
			if n.Cond != nil {
				p.print(n.Cond, blank)
			}
		} else {
			if n.Init != nil {
				p.print(n.Init)
				if _, ok := n.Init.(*RangeClause); ok {
					p.print(blank, n.Body)
					break
				}
			}
			p.print(_Semi, blank)
			if n.Cond != nil {
				p.print(n.Cond)
			}
			p.print(_Semi, blank)
			if n.Post != nil {
				p.print(n.Post, blank)
			}
		}
		p.print(n.Body)
	case *ImportDecl:
		if n.Group == nil {
			p.print(_Import, blank)
		}
		if n.LocalPkgName != nil {
			p.print(n.LocalPkgName, blank)
		}
		p.print(n.Path)
	case *ConstDecl:
		if n.Group == nil {
			p.print(_Const, blank)
		}
		p.printNameList(n.NameList)
		if n.Type != nil {
			p.print(blank, n.Type)
		}
		if n.Values != nil {
			p.print(blank, _Assign, blank, n.Values)
		}
	case *TypeDecl:
		if n.Group == nil {
			p.print(_Type, blank)
		}
		p.print(n.Name, blank)
		p.print(n.Type)
	case *PolyStructDecl:
		p.print(_Poly, blank)
		p.printNode(n.Name)
		p.print(blank, _Lbrace)
		if n.Type != nil && len(n.Type.FieldList) > 0 {
			p.print(newline, indent)
			p.printPolyFieldList(n.Type.FieldList)
			p.print(outdent, newline)
		}
		p.print(_Rbrace)
	case *VarDecl:
		if n.Group == nil {
			p.print(_Var, blank)
		}
		p.printNameList(n.NameList)
		if n.Type != nil {
			p.print(blank, n.Type)
		}
		if n.Values != nil {
			p.print(blank, _Assign, blank, n.Values)
		}
	case *FuncDecl:
		p.print(_Func, blank)
		if r := n.Recv; r != nil {
			p.print(_Lparen)
			if r.Name != nil {
				p.print(r.Name, blank)
			}
			p.printNode(r.Type)
			p.print(_Rparen, blank)
		}
		p.print(n.Name)
		p.printSignature(n.Type)
		if n.Body != nil {
			p.print(blank, n.Body)
		}
	case *printGroup:
		p.print(n.Tok, blank, _Lparen)
		if len(n.Decls) > 0 {
			p.print(newline, indent)
			for _, d := range n.Decls {
				p.printNode(d)
				p.print(_Semi, newline)
			}
			p.print(outdent)
		}
		p.print(_Rparen)
	case *GFile:
		p.print(_Package, blank, n.PkgName)
		if len(n.DeclList) > 0 {
			p.print(_Semi, newline, newline)
			p.printDeclList(n.DeclList)
		}
	}
}

func (p *printer) printFields(fields []*Field, i, j int) {
	if i+1 == j && fields[i].Name == nil {
		p.printNode(fields[i].Type)
	} else {
		for k, f := range fields[i:j] {
			if k > 0 {
				p.print(_Comma, blank)
			}
			p.printNode(f.Name)
		}
		p.print(blank)
		p.printNode(fields[i].Type)
	}
}

func (p *printer) printFieldList(fields []*Field) {
	i0 := 0
	var typ Expr
	for i, f := range fields {
		if f.Name == nil || f.Type != typ {
			if i0 < i {
				p.printFields(fields, i0, i)
				p.print(_Semi, newline)
				i0 = i
			}
			typ = f.Type
		}
	}
	p.printFields(fields, i0, len(fields))
}

func (p *printer) printPolyFieldList(fields []*PolyField) {
	for i, _ := range fields {
		p.print(fields[i].Name, _Lparen)
		if fields[i].TupleType != nil {
			for j, _ := range fields[i].TupleType {
				p.print(fields[i].TupleType[j])
				if j < len(fields[i].TupleType)-1 {
					p.print(_Comma, blank)
				}
			}
		}
		p.print(_Rparen, _Semi, newline)
	}
}

func (p *printer) printMethodList(methods []*Field) {
	for i, m := range methods {
		if i > 0 {
			p.print(_Semi, newline)
		}
		if m.Name != nil {
			p.printNode(m.Name)
			p.printSignature(m.Type.(*FuncType))
		} else {
			p.printNode(m.Type)
		}
	}
}

func (p *printer) printNameList(list []*Name) {
	for i, x := range list {
		if i > 0 {
			p.print(_Comma, blank)
		}
		p.printNode(x)
	}
}

func (p *printer) printExprList(list []Expr) {
	for i, x := range list {
		if i > 0 {
			p.print(_Comma, blank)
		}
		p.printNode(x)
	}
}

func (p *printer) printExprLines(list []Expr) {
	if len(list) > 0 {
		p.print(newline, indent)
		for _, x := range list {
			p.print(x, _Comma, newline)
		}
		p.print(outdent)
	}
}

func groupFor(d Decl) (token, *Group) {
	switch d := d.(type) {
	case *ImportDecl:
		return _Import, d.Group
	case *ConstDecl:
		return _Const, d.Group
	case *TypeDecl:
		return _Type, d.Group
	case *VarDecl:
		return _Var, d.Group
	case *FuncDecl:
		return _Func, nil
	}

	panic("unreachable")
}

type printGroup struct {
	node
	Tok   token
	Decls []Decl
}

func (p *printer) printDecl(list []Decl) {
	tok, group := groupFor(list[0])

	if group == nil {
		if len(list) != 1 {
			panic("unreachable")
		}
		p.printNode(list[0])
		return
	}

	var pg printGroup
	pg.Tok = tok
	pg.Decls = list
	p.printNode(&pg)
}

func (p *printer) printDeclList(list []Decl) {
	i0 := 0
	var tok token
	var group *Group
	for i, x := range list {
		if s, g := groupFor(x); g == nil || g != group {
			if i0 < i {
				p.printDecl(list[i0:i])
				p.print(_Semi, newline)
				if g != group || s != tok || s == _Func {
					p.print(newline)
				}
				i0 = i
			}
			tok, group = s, g
		}
	}
	p.printDecl(list[i0:])
}

func (p *printer) printSignature(sig *FuncType) {
	p.printParameterList(sig.ParamList)
	if list := sig.ResultList; list != nil {
		p.print(blank)
		if len(list) == 1 && list[0].Name == nil {
			p.printNode(list[0].Type)
		} else {
			p.printParameterList(list)
		}
	}
}

func (p *printer) printParameterList(list []*Field) {
	p.print(_Lparen)
	if len(list) > 0 {
		for i, f := range list {
			if i > 0 {
				p.print(_Comma, blank)
			}
			if f.Name != nil {
				p.printNode(f.Name)
				if i+1 < len(list) {
					f1 := list[i+1]
					if f1.Name != nil && f1.Type == f.Type {
						continue // no need to print type
					}
				}
				p.print(blank)
			}
			p.printNode(f.Type)
		}
	}
	p.print(_Rparen)
}

func (p *printer) printStmtList(list []Stmt, braces bool) {
	for i, x := range list {
		p.print(x, _Semi)
		if i+1 < len(list) {
			p.print(newline)
		} else if braces {
			if _, ok := x.(*EmptyStmt); ok {
				p.print(x, _Semi)
			}
		}
	}
}

func (p *printer) printPolyStmtList(list []PolyStmt, braces bool) {
	for i, x := range list {
		if i+1 < len(list) {
			p.print(newline)
		} else if braces {
			p.print(x.PType)
			if x.Args != nil {
				p.print(_Lparen)
				for j, y := range x.Args {
					p.print(y)
					if j < len(x.Args)-1 {
						p.print(_Comma, blank)
					}
				}
				p.print(_Rparen)
			}
			p.print(blank, _Generate, blank, x.Block)
		}
	}
}

func (p *printer) printSwitchBody(list []*CaseClause) {
	p.print(_Lbrace)
	if len(list) > 0 {
		p.print(newline)
		for i, c := range list {
			p.printCaseClause(c, i+1 == len(list))
			p.print(newline)
		}
	}
	p.print(_Rbrace)
}

func (p *printer) printSelectBody(list []*CommClause) {
	p.print(_Lbrace)
	if len(list) > 0 {
		p.print(newline)
		for i, c := range list {
			p.printCommClause(c, i+1 == len(list))
			p.print(newline)
		}
	}
	p.print(_Rbrace)
}

func (p *printer) printCaseClause(c *CaseClause, braces bool) {
	if c.Cases != nil {
		p.print(_Case, blank, c.Cases)
	} else {
		p.print(_Default)
	}
	p.print(_Colon)
	if len(c.Body) > 0 {
		p.print(newline, indent)
		p.printStmtList(c.Body, braces)
		p.print(outdent)
	}
}

func (p *printer) printCommClause(c *CommClause, braces bool) {
	if c.Comm != nil {
		p.print(_Case, blank)
		p.print(c.Comm)
	} else {
		p.print(_Default)
	}
	p.print(_Colon)
	if len(c.Body) > 0 {
		p.print(newline, indent)
		p.printStmtList(c.Body, braces)
		p.print(outdent)
	}
}
