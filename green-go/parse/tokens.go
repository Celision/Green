package parse

type token uint

const (
	_ token = iota
	_EOF

	//names and literals
	_Name    //name
	_Literal //literal

	//operators and operations
	//_Operator is excluding '*' (_Star)
	_Operator //op
	_AssignOp //op=
	_IncOp    //opop
	_Assign   //=
	_Define   //:=
	_Arrow    //<-
	_Star     //*

	//delimiters
	_Lparen    //(
	_Lbrack    //[
	_Lbrace    //{
	_Rparen    //)
	_Rbrack    //]
	_Rbrace    //}
	_Comma     //,
	_Semi      //;
	_Colon     //:
	_Dot       //.
	_Self      //$
	_Hash      //#
	_DotDotDot //...

	//keywords
	_Aff         //aff
	_Atomic      //atomic
	_Break       //break
	_Case        //case
	_Chan        //chan
	_Const       //const
	_Continue    //continue
	_CX          //cx
	_Default     //default
	_Defer       //defer
	_Else        //else
	_Fallthrough //fallthrough
	_For         //for
	_Func        //func
	//_Go          //go, for backwards compatibility.
	_Goto      //goto
	_Green     //green
	_If        //if
	_Import    //import
	_Interface //interface, for backwards compatbility.
	_Lime      //lime
	_Map       //map
	_Package   //package
	_Poly      //poly
	_Range     //range
	_Register  //register
	_Return    //return
	_Select    //select
	_Struct    //struct
	_Switch    //switch
	_Type      //type
	_Var       //var
	_Volatile  //volatile
	_While     //while

	//arrow for poly
	_Generate //->
	//empty line comment to exclude it from .String
	tokenCount //
)

const (
	Break       = _Break
	Continue    = _Continue
	Fallthrough = _Fallthrough
	Goto        = _Goto

	//for CallStmt
	CX    = _CX
	Green = _Green
)

//ensures that there are only at most 64 token types.
const _ uint64 = 1 << (tokenCount - 1)

func contains(tokset uint64, tok token) bool {
	return tokset&(1<<tok) != 0
}

type LitKind uint8

const (
	IntLit LitKind = iota
	FloatLit
	//RuneLit //removed since CX does not need runes.
	StringLit
)

type Operator uint

const (
	_ Operator = iota
	//Def is the : in :=
	Def  //:
	Not  //!
	Recv //<-

	//precOrOr
	OrOr //||

	//precAndAnd
	AndAnd //&&

	//precCmp
	Eql //==
	Neq //!=
	Lss //<
	Leq //<=
	Gtr //>
	Geq //>=

	//precAdd
	Add //+
	Sub //-
	Or  //|
	Xor //^

	//precMul
	Mul    //*
	Div    ///
	Rem    //%
	And    //&
	AndNot //&^
	Shl    //<<
	Shr    //>>
)

const (
	_ = iota
	precOrOr
	precAndAnd
	precCmp
	precAdd
	precMul
)
