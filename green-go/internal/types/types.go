package types

type Node struct{ _ int }

type EType uint8

const (
	Txxx EType = iota
	TINT8
	TUINT8
	TINT16
	TUINT16
	TINT32
	TUINT32
	TINT64
	TUINT64

	TUINTPTR

	TFLOAT32
	TFLOAT64

	TBOOL

	TPTR
	TFUNC
	TSLICE
	TARRAY
	TSTRUCT
	TCHAN
	TMAP
	TINTER
	TFORW
	TANY
	TSTRING
	TUNSAFEPTR

	TPOLYSTRUCT //poly type
	TPOLYTUPLE  //poly subtype
	TPOLYEVAL   //second-class type

	//pseudo-types for literals
	TIDEAL //untyped numerical constants
	TNIL
	TBLANK

	//pseudo-types for frames
	TFUNCARGS
	TCHANARGS

	//while we don't have ssa yet, here we provide types
	TSSA   //internal types used by SSA backend (flags, mem, etc.)
	TTUPLE //type pair used by SSA backend

	//affordance types
	TAFF     //ideal type for affordance
	TAFFINF  //affordance for informing (response to query)
	TAFFQRY  //affordance for querying (request for query)
	TAFFRQST //affordance for requesting mutation
	TAFFPFRM //affordance for performing mutation
	TAFFNIL  //affordance for locking affordances

	NTYPE
)

type ChanDir uint8

func (c ChanDir) CanRecv() bool { return c&Crecv != 0 }
func (c ChanDir) CanSend() bool { return c&Csend != 0 }

const (
	Crecv ChanDir = 1 << 0
	Csend ChanDir = 1 << 1
	Cboth ChanDir = Crecv | Csend
)

//Types stores pointers to predeclared named types
//like TANY, for "any" type recognized for special args.
//or TBLANK, TNIL, or TUNSAFEPTR.
var Types [NTYPE]*Type

//predeclared alias types.
var (
	Bytetype  *Type
	Errortype *Type

	//represent untyped string and boolean constants
	Idealstring *Type
	Idealbool   *Type

	//Types to represent untyped numeric constants
	Idealint   = New(TIDEAL)
	Idealfloat = New(TIDEAL)
)

//Type represents Green/CX type.
type Type struct {
	//Extra contains extra etype-specific fields.
	//for optimization, etype-specific structs containing exactly
	//one pointer-shaped field are stored as values rather than pointers when possible
	// TMAP: *Map; TFORW: *Forward; TFUNC: *Func; TSTRUCT: *Struct; TINTER: *Interface; TFUNCARGS: FuncArgs; TCHANARGS: ChanArgs; TCHAN: *Chan; TPTR: Ptr; TARRAY: *Array; TSLICE: Slice; TPOLYSTRUCT: *PolyStruct; TPOLYTUPLE: *PolyTuple
	Extra interface{}

	Width int64 //valid if Align > 0; width of type in bytes

	methods    Fields
	allMethods Fields

	Nod  *Node //canonical OTYPE node
	Orig *Type //original type (type literal or predefined type)

	//cache of composite types, with this type being the element type
	Cache struct {
		ptr   *Type //*T or nil
		slice *Type //[]T or nil
	}

	Sym    *Sym  //symbol containing name, for named types
	Vargen int32 //unique name for OTYPE/ONAME

	Etype EType //kind of type
	Aling uint8 //required memory alignment of type, in bytes (0 means this and Width haven't been computed yet)

	flags bitset8
}

const (
	typeNotInHeap  = 1 << iota //type cannot be heap allocated
	typeBroke                  //broken type definition
	typeNoalg                  //suppress hash and eq algorithm generation
	typeDeferwidth             //width compute deferred and type is on deferredTypeStack
	typeRecur
)

func (t *Type) NotInHeap() bool  { return t.flags&typeNotInHeap != 0 }
func (t *Type) Broke() bool      { return t.flags&typeBroke != 0 }
func (t *Type) Noalg() bool      { return t.flags&typeNoalg != 0 }
func (t *Type) Deferwidth() bool { return t.flags&typeDeferwidth != 0 }
func (t *Type) Recur() bool      { return t.flags&typeRecur != 0 }

func (t *Type) SetNotInHeap(b bool)  { t.flags.set(typeNotInHeap, b) }
func (t *Type) SetBroke(b bool)      { t.flags.set(typeBroke, b) }
func (t *Type) SetNoalg(b bool)      { t.flags.set(typeNoalg, b) }
func (t *Type) SetDeferwidth(b bool) { t.flags.set(typeDeferwidth, b) }
func (t *Type) SetRecur(b bool)      { t.flags.set(typeRecur, b) }

/**
 * From this point forward, support for
 * struct embedding, interfaces, chans,
 * goroutines/greenroutines, and defer
 * is going to be dropped until a later
 * date when implementation of such is
 * more doable.
 */

//Pkg returns the package t appeared in
//only defined for function, struct, interface, polystruct, and polyeval types.
//This info is used for the green/types API once it is exposed.
func (t *Type) Pkg() *Pkg {
	switch t.Etype {
	case TFUNC:
		return t.Extra.(*Func).pkg
	case TSTRUCT:
		return t.Extra.(*Struct).pkg
	case TPOLYEVAL:
		return t.Extra.(*PolyEval).pkg
	case TPOLYSTRUCT:
		return t.Extra.(*PolyStruct).pkg
	default:
		Fatalf("Pkg: unexpected kind: %v", t)
		return nil
	}
}

//TODO(CALLISTER) -- implement the above.

func (t *Type) SetPkg(pkg *Pkg) {
	switch t.Etype {
	case TFUNC:
		t.Extra.(*Func).pkg = pkg
	case TSTRUCT:
		t.Extra.(*Struct).pkg = pkg
	case TPOLYEVAL:
		t.Extra.(*PolyEval).pkg = pkg
	case TPOLYSTRUCT:
		t.Extra.(*PolyStruct).pkg = pkg
	default:
		Fatalf("Pkg: unexpected kind: %v", t)
	}
}

type Map struct {
	Key  *Type
	Elem *Type

	Bucket *Type //internal struct type representing hash bucket
	Hmap   *Type //internal struct type representing map header
	Hiter  *Type //internal struct type representing hash iterator state
}

func (t *Type) MapType() *Map {
	t.wantEtype(TMAP)
	return t.Extra.(*Map)
}

type Forward struct {
	Copyto      []*Type  //where to copy the eventual value to
	Embedlineno src.XPos //first use of this type as an embeded type
}
