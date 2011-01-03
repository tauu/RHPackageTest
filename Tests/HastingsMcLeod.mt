(* Mathematica Test File *)

maxMemory = 256*1024*1024;

Test[
	MemoryConstrained[
		(PainleveII[{I,0,-I}, 10]+1.104753255289869*10^(-10))/1.104753255289869*10^(-10),
		maxMemory
	]
	,
	0
	(* actual result from Prahofer and Spohn Data *) 
	,
	TestID->"PainleveII-20110103-HastingsMcLeod-Positive-10"
	,
	EquivalenceFunction -> NEqual
]

Test[
	MemoryConstrained[
		(PainleveII[{I,0,-I}, 6]+8.510993571662368*10^(-10))/8.510993571662368*10^(-10),
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