package parse

import (
	"io"
	"unicode/utf8"
)

/**
 * b, when >= 0, is beginning of segment of most recently read characters.
 *
 * r is the byte immediately after the most recently character (ch)
 *
 * e is the byte immediately after the last byte read into the buffer.
 *
 * Buffer is terminated with utf8.RuneSelf, making ASCII easy to test.
 */
type source struct {
	in        io.Reader                        //actual reader for source object
	errh      func(line, col uint, msg string) //err handler function
	buf       []byte                           //bytes buffer.
	ioerr     error                            //possible pending I/O error
	b, r, e   int                              //above comment.
	line, col uint                             //source position of ch (0-based, lmao).
	ch        rune                             //most recently read character
	chw       int                              //width in bytes of ch

}

const sentinel = utf8.RuneSelf

func (s *source) init(in io.Reader, errh func(line, col uint, msg string)) {
	s.in = in
	s.errh = errh

	if s.buf == nil {
		s.buf = make([]byte, nextSize(0))
	}
	s.buf[0] = sentinel
	s.ioerr = nil
	s.b, s.r, s.e = -1, 0, 0
	s.line, s.col = 0, 0
	s.ch = ' '
	s.chw = 0
}

//starting point for lexical line and column numbers.
const linebase = 1
const colbase = 1

func (s *source) pos() (line, col uint) {
	return linebase + s.line, colbase + s.col
}

//error reports the error message at the current source position (s.pos()).
func (s *source) error(msg string) {
	line, col := s.pos()
	s.errh(line, col, msg)
}

//start() starts a new active lexical segment of the source, including s.ch.
//as long as stop() has not been called, the acted segment's bytes
//(excluding s.ch) can be retrieved by calling segment().
func (s *source) start()          { s.b = s.r - s.chw }
func (s *source) stop()           { s.b = -1 }
func (s *source) segment() []byte { return s.buf[s.b : s.r-s.chw] }

//rewind() rewinds the scanner's read position and the character s.ch
//to the beginning of the currently active segment, which must not
//contain any newlines (otherwise positional info will be invalid, oops).
//Currently, rewind is only needed for handling the sequence ".."; it must
//never be called outside an active segment. CX does not currently require
//testing for this sequence either, so this method is only included for the
//sake of comprehension and sanity.
func (s *source) rewind() {
	//must check for active segment
	if s.b < 0 {
		panic("no active segment")
	}

	s.col -= uint(s.r - s.b)
	s.r = s.b
	s.nextch()
}

//nextch scans the active buffer for the next valid character and stores it
//in s.ch.
/**
 * Developer Comment here. I'm not a fan of GOTO, but here is a case that makes
 * lexical and grammatical sense. I've kept it in the canonical form as it
 * appears in the original source.go file. -Geoffrey Kublin
 */
func (s *source) nextch() {
redo:
	s.col += uint(s.chw)
	if s.ch == '\n' {
		s.line++
		s.col = 0
	}

	//common case: ascii character(s).
	if s.ch = rune(s.buf[s.r]); s.ch < sentinel {
		s.r++
		s.chw = 1
		if s.ch == 0 {
			s.error("invalid NUL character")
			goto redo
		}
		return
	}

	//Condition 1: ensures no chw exceeds the maximum for a UTF8 single character sequence.
	//Condition 2: stops the loop when a full rune can be recovered from the active segment.'
	//Condition 3: stops the loop on an I/O error.
	for s.e-s.r < utf8.UTFMax && !utf8.FullRune(s.buf[s.r:s.e]) && s.ioerr == nil {
		s.fill()
	}

	//EOF
	if s.r == s.e {
		if s.ioerr != io.EOF {
			s.error("I/O error: " + s.ioerr.Error())
			s.ioerr = nil
		}
		s.ch = -1
		s.chw = 0
		return
	}

	s.ch, s.chw = utf8.DecodeRune(s.buf[s.r:s.e])
	s.r += s.chw

	//checks for rune errors from the above decoding call to the utf8 package.
	if s.ch == utf8.RuneError && s.chw == 1 {
		s.error("invalid UTF-8 encoding")
		goto redo
	}

	//check if the unicode byte-order mark (BOM) is present.
	//only valid if beginning of file.
	const BOM = 0xFEFF
	if s.ch == BOM {
		if s.line > 0 || s.col > 0 {
			s.error("invalid BOM in the middle of the file.")
		}
		goto redo
	}
}

//fill() reads more source bytes into s.buf
//It returns with at least one more byte in the buffer OR with s.ioerr != nil.
func (s *source) fill() {
	//determine content to preserve
	b := s.r
	if s.b >= 0 {
		b = s.b
		s.b = 0 //after buffer is grown or content is moved down.
	}
	content := s.buf[b:s.e]

	//grow buffer or move content down
	if len(content)*2 > len(s.buf) {
		s.buf = make([]byte, nextSize(len(s.buf)))
		copy(s.buf, content)
	} else if b > 0 {
		copy(s.buf, content)
	}
	s.r -= b
	s.e -= b

	//read more data. Try a limited # of times, in this case, 10.
	for i := 0; i < 10; i++ {
		var n int
		n, s.ioerr = s.in.Read(s.buf[s.e : len(s.buf)-1]) //-1 to leave space for sentinel
		if n < 0 {
			panic("negative read") //incorrect underlying io.Reader implementation
		}
		if n > 0 || s.ioerr != nil {
			s.e += n
			s.buf[s.e] = sentinel
			return
		}
		//n == 0
	}

	s.buf[s.e] = sentinel
	s.ioerr = io.ErrNoProgress
}

//nextSize is internally used function for the source buffer.
//returns the next bigger size for a buffer of a given size.
//part of the optimizations for the lexer.
func nextSize(size int) int {
	const min = 4 << 10 // 4K: minimum buffer size
	const max = 1 << 20 // 1M: maximum buffer size which is still doubled
	if size < min {
		return min
	}
	if size <= max {
		return size << 1
	}
	return size + max
}
