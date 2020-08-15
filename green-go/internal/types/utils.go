package types

const BADWITDH = -1000000000

type bitset8 uint8

func (b *bitset8) set(f uint, bb bool) {
	if bb {
		b |= uint8(f)
	} else {
		b &= (0xFF - uint8(f))
	}
}
