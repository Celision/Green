package parse

import (
	"fmt"
	"io"
	"unicode"
	"unicode/utf8"
)

//As mentioned in the scanner.go file for Golang, this file, source.go,
//tokens.go, and token_string.go are self-contained and thus should be
//made into their own package. Such will happen in a date soon enough.
type scanner struct {
	source
	mode   uint
	nlsemi bool //if set, newlines and EOF translate to ';'

	//current token data, valid after calling next()
	line, col uint
	blank     bool //line is blank up to col
	tok       token
	lit       string   //valid if tok is _Name, _Literal, or _Semi ("semicolon", "newline", or "EOF"); may be malformed if bad is true
	bad       bool     //valid if tok is _Literal, true if a syntax error occured, lit may be malformed
	kind      LitKind  //valid if tok is _Literal
	op        Operator //valid if tok is _Operator, _AssignOp, or _IncOp
	prec      int      //same thing as immediately above comment.
}

//scanner modes. Currently only one.
const (
	StandardScanMode = 1 << iota
)

func (s *scanner) init(src io.Reader, errh func(line, col uint, msg string), mode uint) {
	s.source.init(src, errh)
	s.mode = mode
	s.nlsemi = false
}

//errorf reports an error at the most recently read character position.
func (s *scanner) errorf(format string, args ...interface{}) {
	s.error(fmt.Sprintf(format, args...))
}

//errAtf reports an error at a byte column offset relative to the current token start.
func (s *scanner) errorAtf(offset int, format string, args ...interface{}) {
	s.errh(s.line, s.col+uint(offset), fmt.Sprintf(format, args...))
}

//setLit sets the scanner state for a recognized _Literal token
func (s *scanner) setLit(kind LitKind, ok bool) {
	s.nlsemi = true
	s.tok = _Literal
	s.lit = string(s.segment())
	s.bad = !ok
	s.kind = kind
}

//next() advances the scanner by reading the next token.
//On encoding, lex, or read error, errh is called.
//Since CX and Green do not include any special comment
//or directive parsing, the code has been modified from
//the original scanner.go to not include things for this.
func (s *scanner) next() {
	nlsemi := s.nlsemi
	s.nlsemi = false

redo:
	//skip whitespace
	s.stop()
	sline, scol := s.pos()
	for s.ch == ' ' || s.ch == '\t' || s.ch == '\n' && !nlsemi || s.ch == '\r' {
		s.nextch()
	}

	//token start

	s.line, s.col = s.pos()
	s.blank = s.line > sline || scol == colbase
	s.start()
	//if character is a letter or identifier character
	if isLetter(s.ch) || s.ch >= utf8.RuneSelf && s.atIdentChar(true) {
		s.nextch()
		s.ident()
		return
	}

	switch s.ch {
	case -1:
		if nlsemi {
			s.lit = "EOF"
			s.tok = _Semi
			break
		}
		s.tok = _EOF
	case '\n':
		s.nextch()
		s.lit = "newline"
		s.tok = _Semi
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		s.number(false)
	case '"':
		s.stdString()
	case '`':
		s.rawString()
	//case '\'':
	//	s.rune() //removed since CX has no need for runes.
	case '(':
		s.nextch()
		s.tok = _Lparen
	case '[':
		s.nextch()
		s.tok = _Lbrack
	case '{':
		s.nextch()
		s.tok = _Lbrace
	case ',':
		s.nextch()
		s.tok = _Comma
	case ';':
		s.nextch()
		s.lit = "semicolon"
		s.tok = _Semi
	case ')':
		s.nextch()
		s.nlsemi = true
		s.tok = _Rparen
	case ']':
		s.nextch()
		s.nlsemi = true
		s.tok = _Rbrack
	case '}':
		s.nextch()
		s.nlsemi = true
		s.tok = _Rbrace
	case ':':
		s.nextch()
		if s.ch == '=' {
			//_Define
			s.nextch()
			s.tok = _Define
			break
		}
		s.tok = _Colon
	case '#':
		s.nextch()
		s.tok = _Hash
	case '$':
		s.nextch()
		s.tok = _Self
	case '.':
		s.nextch()
		if isDecimal(s.ch) {
			s.number(true)
			break
		}
		if s.ch == '.' {
			//_DotDotDot
			s.nextch()
			if s.ch == '.' {
				s.nextch()
				s.tok = _DotDotDot
				break
			}
			s.rewind() //now s.ch holds first '.'
			s.nextch() //consume first '.' again
		}
		s.tok = _Dot
	case '+':
		s.nextch()
		s.op, s.prec = Add, precAdd
		if s.ch != '+' {
			goto assignop
		}
		s.nextch()
		s.nlsemi = true
		s.tok = _IncOp
	case '-':
		s.nextch()
		if s.ch == '>' {
			s.tok = _Generate
			break
		}
		s.op, s.prec = Sub, precAdd
		if s.ch != '-' {
			goto assignop
		}
		s.nextch()
		s.nlsemi = true
		s.tok = _IncOp
	case '*':
		s.nextch()
		s.op, s.prec = Mul, precMul
		//don't goto assignop, - want _Star token
		if s.ch == '=' {
			s.nextch()
			s.tok = _AssignOp
			break
		}
		s.tok = _Star
	case '/':
		s.nextch()
		if s.ch == '/' {
			//line comment
			s.nextch()
			s.lineComment()
			goto redo
		}
		if s.ch == '*' {
			//multiline comment
			s.nextch()
			s.fullComment()
			if line, _ := s.pos(); line > s.line && nlsemi {
				//a multiline comment acts like a newline;
				//it translates to a ';' if nlsemi is set.
				s.lit = "newline"
				s.tok = _Semi
				break
			}
			goto redo
		}
		s.op, s.prec = Div, precMul
		goto assignop
	case '%':
		s.nextch()
		s.op, s.prec = Rem, precMul
		goto assignop
	case '&':
		s.nextch()
		if s.ch == '&' {
			s.nextch()
			s.op, s.prec = AndAnd, precAndAnd
			s.tok = _Operator
			break
		}
		s.op, s.prec = And, precMul
		if s.ch == '^' {
			s.nextch()
			s.op = AndNot
		}
		goto assignop
	case '|':
		s.nextch()
		if s.ch == '|' {
			s.nextch()
			s.op, s.prec = OrOr, precOrOr
			s.tok = _Operator
			break
		}
		s.op, s.prec = Or, precAdd
		goto assignop
	case '^':
		s.nextch()
		s.op, s.prec = Xor, precAdd
		goto assignop
	case '<':
		s.nextch()
		if s.ch == '=' {
			s.nextch()
			s.op, s.prec = Leq, precCmp
			s.tok = _Operator
			break
		}
		if s.ch == '<' {
			s.nextch()
			s.op, s.prec = Shl, precMul
			goto assignop
		}
		if s.ch == '-' {
			s.nextch()
			s.tok = _Arrow
			break
		}
		s.op, s.prec = Lss, precCmp
		s.tok = _Operator
	case '>':
		s.nextch()
		if s.ch == '=' {
			s.nextch()
			s.op, s.prec = Geq, precCmp
			s.tok = _Operator
			break
		}
		if s.ch == '>' {
			s.nextch()
			s.op, s.prec = Shr, precMul
			goto assignop
		}
		s.op, s.prec = Gtr, precCmp
		s.tok = _Operator
	case '=':
		s.nextch()
		if s.ch == '=' {
			s.nextch()
			s.op, s.prec = Eql, precCmp
			s.tok = _Operator
			break
		}
		s.tok = _Assign
	case '!':
		s.nextch()
		if s.ch == '=' {
			s.nextch()
			s.op, s.prec = Neq, precCmp
			s.tok = _Operator
			break
		}
		s.op, s.prec = Not, 0
		s.tok = _Operator
	default:
		s.errorf("invalid character %#U", s.ch)
		s.nextch()
		goto redo
	}

	return

assignop:
	if s.ch == '=' {
		s.nextch()
		s.tok = _AssignOp
		return
	}
	s.tok = _Operator
}

//finds possible identifiers or keywords.
func (s *scanner) ident() {
	for isLetter(s.ch) || isDecimal(s.ch) {
		s.nextch()
	}

	if s.ch >= utf8.RuneSelf {
		for s.atIdentChar(false) {
			s.nextch()
		}
	}

	lit := s.segment()
	if len(lit) >= 2 {
		if tok := keywordMap[hash(lit)]; tok != 0 && tokStrFast(tok) == string(lit) {
			s.nlsemi = contains(1<<_Break|1<<_Continue|1<<_Fallthrough|1<<_Return, tok)
			s.tok = tok
			if s.tok == _CX {
				s.tok = _Green //saves us time later using duplicated things.
			}
			return
		}
	}

	s.nlsemi = true
	s.lit = string(lit)
	s.tok = _Name
}

//assumes tok is a valid token, and uses lookup buffer to get string.
func tokStrFast(tok token) string {
	return _token_name[_token_index[tok-1]:_token_index[tok]]
}

func (s *scanner) atIdentChar(first bool) bool {
	switch {
	case unicode.IsLetter(s.ch) || s.ch == '_':
		//ok
	case unicode.IsDigit(s.ch):
		if first {
			s.errorf("idenfifier cannot begin with digit %#U", s.ch)
		}
	case s.ch >= utf8.RuneSelf:
		s.errorf("invalid character %#U in identifier", s.ch)
	default:
		return false
	}
	return true
}

//hash is a perfect hash function for keywords.
func hash(s []byte) uint {
	return (uint(s[0])<<5 ^ uint(s[1]) + uint(len(s))) & uint(len(keywordMap)-1)
}

var keywordMap [1 << 10]token // size must be power of two

func init() {
	// populate keywordMap
	/*for tok := _Aff; tok <= _While; tok++ {
		fmt.Println(tok)
	}*/
	for tok := _Aff; tok <= _While; tok++ {
		//fmt.Println(tok)
		h := hash([]byte(tok.String()))
		if keywordMap[h] != 0 {
			panic("imperfect hash")
		}
		keywordMap[h] = tok
	}
}

func lower(ch rune) rune     { return ('a' - 'A') | ch } // returns lower-case ch iff ch is ASCII letter
func isLetter(ch rune) bool  { return 'a' <= lower(ch) && lower(ch) <= 'z' || ch == '_' }
func isDecimal(ch rune) bool { return '0' <= ch && ch <= '9' }
func isHex(ch rune) bool     { return '0' <= ch && ch <= '9' || 'a' <= lower(ch) && lower(ch) <= 'f' }

//digits
func (s *scanner) digits(base int, invalid *int) (digsep int) {
	if base <= 10 {
		max := rune('0' + base)
		for isDecimal(s.ch) || s.ch == '_' {
			ds := 1
			if s.ch == '_' {
				ds = 2
			} else if s.ch >= max && *invalid < 0 {
				_, col := s.pos()
				*invalid = int(col - s.col) //records invalid rune index
			}
			digsep |= ds
			s.nextch()
		}
	} else {
		for isHex(s.ch) || s.ch == '_' {
			ds := 1
			if s.ch == '_' {
				ds = 2
			}
			digsep |= ds
			s.nextch()
		}
	}
	return
}

func (s *scanner) number(seenPoint bool) {
	ok := true
	kind := IntLit
	base := 10
	prefix := rune(0) //either 0 (decimal), '0' (octal), 'x', 'o', or 'b'.
	digsep := 0       //bit 0: digit present, bit 1: '_' present
	invalid := -1     //index of invalid digit in literal, or < 0

	//integer part
	if !seenPoint {
		if s.ch == '0' {
			s.nextch()
			switch lower(s.ch) {
			case 'x':
				s.nextch()
				base, prefix = 16, 'x'
			case 'o':
				s.nextch()
				base, prefix = 8, 'o'
			case 'b':
				s.nextch()
				base, prefix = 2, 'b'
			default:
				base, prefix = 8, '0'
				digsep = 1 //leading 0
			}
		}
		digsep |= s.digits(base, &invalid)
		if s.ch == '.' {
			if prefix == 'o' || prefix == 'b' {
				s.errorf("invalid radix point in %s literal", baseName(base))
				ok = false
			}
			s.nextch()
			seenPoint = true
		}
	}

	if seenPoint {
		kind = FloatLit
		digsep |= s.digits(base, &invalid)
	}

	if digsep&1 == 0 && ok {
		s.errorf("%s literal has no digits", baseName(base))
		ok = false
	}

	if e := lower(s.ch); e == 'e' || e == 'p' {
		if ok {
			switch {
			case e == 'e' && prefix != 0 && prefix != '0':
				s.errorf("%q exponent requires decimal mantissa", s.ch)
			case e == 'p' && prefix != 'x':
				s.errorf("%q exponent requires hexadecimal mantissa", s.ch)
				ok = false
			}
		}
		s.nextch()
		kind = FloatLit
		if s.ch == '+' || s.ch == '-' {
			s.nextch()
		}
		digsep = s.digits(10, nil) | digsep&2 //don't lose sep bit
		if digsep&1 == 0 && ok {
			s.errorf("exponent has no digits")
			ok = false
		}
	} else if prefix == 'x' && kind == FloatLit && ok {
		s.errorf("hexadecimal mantissa requires a 'p' exponent")
		ok = false
	}

	s.setLit(kind, ok) //we can use s.lit below now hooray!

	if kind == IntLit && invalid >= 0 && ok {
		s.errorAtf(invalid, "invalid digit %q in %s literal", s.lit[invalid], baseName(base))
		ok = false
	}

	if digsep&2 != 0 && ok {
		if i := invalidSep(s.lit); i >= 0 {
			s.errorAtf(i, "'_' must separate successive digits")
			ok = false
		}
	}

	s.bad = !ok
}

func baseName(base int) string {
	switch base {
	case 2:
		return "binary"
	case 8:
		return "octal"
	case 10:
		return "decimal"
	case 16:
		return "hexadecimal"
	}

	panic("invalid base given to baseName")
}

//invalidSep returns the index of the first invalid seperator in x (or -1)
func invalidSep(x string) int {
	x1 := ' ' //prefix char, only care if == x
	d := '.'  //digit, one of '_', '0' (digit) or '.' (other)
	i := 0

	//a prefix will count as a digit <-- said some retarded Go dev
	if len(x) >= 2 && x[0] == '0' {
		x1 = lower(rune(x[1]))
		if x1 == 'x' || x1 == 'o' || x1 == 'b' {
			d = '0'
			i = 2
		}
	}

	//mantissa and exponent
	for ; i < len(x); i++ {
		p := d //previous digit
		d = rune(x[i])
		switch {
		case d == '_':
			if p != '0' {
				return i
			}
		case isDecimal(d) || x1 == 'x' && isHex(d):
			d = '0'
		default:
			if p == '_' {
				return i - 1
			}
			d = '.'
		}
	}
	if d == '_' {
		return len(x) - 1
	}

	return -1
}

func (s *scanner) stdString() {
	ok := true
	s.nextch()

	for {
		if s.ch == '"' {
			s.nextch()
			break
		}
		if s.ch == '\\' {
			s.nextch()
			if !s.escape('"') {
				ok = false
			}
			continue
		}
		if s.ch == '\n' {
			s.errorf("newline in string")
			ok = false
			break
		}
		if s.ch < 0 {
			s.errorAtf(0, "string not terminated")
			ok = false
			break
		}
		s.nextch()
	}

	s.setLit(StringLit, ok)
}

func (s *scanner) rawString() {
	ok := true
	s.nextch()

	for {
		if s.ch == '`' {
			s.nextch()
			break
		}
		if s.ch < 0 {
			s.errorAtf(0, "string not terminated")
			ok = false
			break
		}
		s.nextch()
	}

	s.setLit(StringLit, ok)
}

func (s *scanner) comment(text string) {
	//s.errorAtf(0, "%s", text) //no directive processing support.
}

func (s *scanner) skipLine() {
	//don't consume newline, it's needed for semicolon insertion logic
	for s.ch >= 0 && s.ch != '\n' {
		s.nextch()
	}
}

func (s *scanner) lineComment() {
	//opening has already been consumed.

	s.skipLine()
}

func (s *scanner) skipComment() bool {
	for s.ch >= 0 {
		for s.ch == '*' {
			s.nextch()
			if s.ch == '/' {
				s.nextch()
				return true
			}
		}
		s.nextch()
	}
	s.errorAtf(0, "comment not terminated")
	return false
}

func (s *scanner) fullComment() {
	//opening already vored.

	if s.skipComment() {
		//do not give a single flying f**k.
	}
}

func (s *scanner) escape(quote rune) bool {
	var n int
	var base, max uint32

	switch s.ch {
	case quote, 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\':
		s.nextch()
		return true
	case '0', '1', '2', '3', '4', '5', '6', '7':
		n, base, max = 3, 8, 255
	case 'x':
		s.nextch()
		n, base, max = 2, 16, 255
	case 'u':
		s.nextch()
		n, base, max = 4, 16, unicode.MaxRune
	case 'U':
		s.nextch()
		n, base, max = 8, 16, unicode.MaxRune
	default:
		if s.ch < 0 {
			return true //complain to caller about EOF
		}
		s.errorf("unknown escape")
		return false
	}

	var x uint32
	for i := n; i > 0; i-- {
		if s.ch < 0 {
			return true //complain to caller about EOF`
		}
		d := base
		if isDecimal(s.ch) {
			d = uint32(s.ch) - '0'
		} else if 'a' <= lower(s.ch) && lower(s.ch) <= 'f' {
			d = uint32(lower(s.ch)) - 'a' + 10
		}
		if d >= base {
			s.errorf("invalid character %q in %s escape", s.ch, baseName(int(base)))
			return false
		}
		x = x*base + d
		s.nextch()
	}

	if x > max && base == 8 {
		s.errorf("octal escape value %d > 255", x)
		return false
	}

	if x > max || 0xD800 <= x && x < 0xE000 /* surrogate range */ {
		s.errorf("escape is invalid Unicode code point %#U", x)
		return false
	}

	return true
}
