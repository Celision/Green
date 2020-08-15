package parse

import "strconv"

const _token_name = "EOFnameliteralopop=opop=:=<-*([{)]},;:.$#...affatomicbreakcasechanconstcontinuecxdefaultdeferelsefallthroughforfuncgotogreenifimportinterfacelimemappackagepolyrangeregisterreturnselectstructswitchtypevarvolatilewhile->"

var _token_index = [...]uint8{0, 3, 7, 14, 16, 19, 23, 24, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 44, 47, 53, 58, 62, 66, 71, 79, 81, 88, 93, 97, 108, 111, 115, 119, 124, 126, 132, 141, 145, 148, 155, 159, 164, 172, 178, 184, 190, 196, 200, 203, 211, 216, 218, 218}

func (i token) String() string {
	i -= 1
	if i >= token(len(_token_index)-1) {
		return "token(" + strconv.FormatInt(int64(i+1), 10) + ")"
	}
	return _token_name[_token_index[i]:_token_index[i+1]]
}
