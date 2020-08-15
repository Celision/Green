package parse

import "strconv"

const _Operator_name = ":!<-||&&==!=<<=>>=+-|^*/%&&^<<>>"

var _Operator_index = [...]uint8{0, 1, 2, 4, 6, 8, 10, 12, 13, 15, 16, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 30, 32}

func (i Operator) String() string {
	i -= 1
	if i >= Operator(len(_Operator_index)-1) {
		return "Operator(" + strconv.FormatInt(int64(i+1), 10) + ")"
	}
	return _Operator_name[_Operator_index[i]:_Operator_index[i+1]]
}
