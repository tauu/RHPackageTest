(* Mathematica Test File *)


Test[
	NZeroQ[CauchyBasisD[Line[{1, \[Infinity]}], 1, 0.]]
	,
	True
	,
	TestID->"CauchyBasisDInf"
]


Test[
	NZeroQ[CauchyBasisD[Line[{\[Infinity],1}], 1, 0.]]
	,
	True
	,
	TestID->"CauchyBasisDInfR"
]


Test[
	Chop[CauchyBasisD[Line[{1, \[Infinity]}], 2, 0.]
		-CauchyBasisD[Line[{1, \[Infinity]}], 2, 0.000001],10^-4]
	,
	0
	,
	TestID->"CauchyBasisDInf2"
]