(* Mathematica Test File *)

maxMemory = 256*1024*1024;

Test[
	MemoryConstrained[
		PainleveII[{I,0,-I}, 10],
		maxMemory
	]
	,
	-1.104753255289869*10^(-10)
	(* actual result from Prahofer and Spohn Data *) 
	,
	TestID->"PainleveII-20110103-HastingsMcLeod-Positive-10"
	,
	EquivalenceFunction -> NEqualRelative
]

Test[
	MemoryConstrained[
		(PainleveII[{I,0,-I}, 6]+9.947694360291132`*^-6)/(10*9.947694360291132`*^-6 ),
		maxMemory
	]
	,
	0
	(* actual result from Prahofer and Spohn Data *) 
	,
	TestID->"PainleveII-20110103-HastingsMcLeod-Positive-6"
	,
	EquivalenceFunction -> NEqual
]

Test[
	MemoryConstrained[
		PainleveIID[{I, 0, -I},6.],
		maxMemory
	]
	,
	0.00002476520039732634`
	(* actual result from Prahofer and Spohn Data *) 
	,
	TestID->"PainleveII-20110103-HastingsMcLeod-PositiveD-6"
	,
	EquivalenceFunction -> NEqual
]

