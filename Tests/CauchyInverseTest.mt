(* Mathematica Test File *)


(* ::Section:: *)
(* Real interval *)




(* ::Subsection:: *)
(*  Cauchy Inverse *)


eps = 10.^(-7);
a=0.1;
b=0.2 ;

cf = Fun[1/# &, Line[{a,b}]];
cfaeps = Fun[1/# &, Line[{a+eps,b}]];
cfbeps = Fun[1/# &, Line[{a,b+eps}]];

cfa = SetDomain[cf,cfaeps];
cfb = SetDomain[cf,cfbeps];


Test[
	Chop[
CauchyInverse[cf, 10000000.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-Decay"
]

Test[
	Chop[
(CauchyInverse[cfa, 1. I] - CauchyInverse[cf, 1. I])/eps - 
 CauchyInverseDomainGrad[1, 0][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainGrad-a"
]

Test[
	Chop[
(CauchyInverse[cfb, 1. I] - CauchyInverse[cf, 1. I])/eps - 
 CauchyInverseDomainGrad[0, 1][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainGrad-b"
]


Test[
	Chop[
(CauchyInverse[cfaeps,1.I]-CauchyInverse[cf,1.I])/eps-CauchyInverseDomainD[1,0][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainD-a"
]

Test[
	Chop[
(CauchyInverse[cfbeps,1.I]-CauchyInverse[cf,1.I])/eps-CauchyInverseDomainD[0,1][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainD-b"
]






(* ::Subsection:: *)
(*  Cauchy Inverse Integral *)


g[z_] = CauchyInverseIntegral[cf,z];

Test[
	Chop[
CauchyInverseIntegral[cf, 1000000.]-DCT[cf][[2]] ((Log[1/4 (RightEndpoint[cf] - LeftEndpoint[cf])] - 
     Log[IntervalToInnerCircle[
       MapToInterval[cf, 1000000.]]]))/(4 MapToIntervalD[cf, 0.`]) 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-Decay"
]

Test[
	Chop[
g'[1. I]-CauchyInverse[cf,1. I]
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-D"
]

Test[
	Chop[
CauchyInverseIntegral[cf, a + eps^2 I] - CauchyInverseIntegral[+1, cf, a] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-a"
]

Test[
	Chop[
CauchyInverseIntegral[cf, a - eps^2 I] - CauchyInverseIntegral[-1, cf, a] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-a"
]

Test[
	Chop[
CauchyInverseIntegral[cf, b + eps^2 I] - CauchyInverseIntegral[+1, cf, b] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-b"
]

Test[
	Chop[
CauchyInverseIntegral[cf, b - eps^2 I] - CauchyInverseIntegral[-1, cf, b] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-b"
]

Test[
	Chop[
CauchyInverseIntegral[cf, .15 + eps^2 I] - CauchyInverseIntegral[+1, cf, .15] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-mid"
]

Test[
	Chop[
CauchyInverseIntegral[cf, .15 - eps^2 I] - CauchyInverseIntegral[-1, cf, .15] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-mid"
]

Test[
	Chop[
CauchyInverseIntegral[cf, eps^2 I] - CauchyInverseIntegral[+1, cf, 0.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-left"
]

Test[
	Chop[
CauchyInverseIntegral[cf, -eps^2 I] - CauchyInverseIntegral[-1, cf, 0.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-left"
]
  
  
  Test[
	Chop[
CauchyInverseIntegral[cf,1.+ eps^2 I] - CauchyInverseIntegral[+1, cf, 1.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-right"
]

Test[
	Chop[
CauchyInverseIntegral[cf,1. -eps^2 I] - CauchyInverseIntegral[-1, cf, 1.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-right"
]
  
 
(* ::Subsection:: *)
(* SPCauchy Inverse Integral D *)

Test[
Chop[(SPCauchyInverseIntegral[cfa] - SPCauchyInverseIntegral[cf])/eps - 
  SPCauchyInverseIntegralDomainGrad[1, 0][cf] // Norm,10^(-6)]
  ,
  0
  ,
  TestID->"SPCauchyInverseIntegralDomaainGrad-a"
]
  
Test[
	Chop[
		(SPCauchyInverseIntegral[cfaeps] - SPCauchyInverseIntegral[cf])/eps - 
 SPCauchyInverseIntegralDomainD[1, 0][cf],10^(-5)]//Norm
 ,
 0
 ,
 TestID->"SPCauchyInverseIntegralDomainD-a"
]

Test[
	Chop[
		(SPCauchyInverseIntegral[cfbeps] - SPCauchyInverseIntegral[cf])/eps - 
 SPCauchyInverseIntegralDomainD[0, 1][cf],10^(-5)]//Norm
 ,
 0
 ,
 TestID->"SPCauchyInverseIntegralDomainD-b"
]


Test[
	Chop[
		1/eps (CauchyInverse[SPCauchyInverseIntegral[cfa], 1. I] - 
    CauchyInverse[SPCauchyInverseIntegral[cf], 
     1. I])- (
     CauchyInverse[SPCauchyInverseIntegralDomainGrad[1, 0][cf],1. I] + 
     CauchyInverseDomainGrad[1, 0][SPCauchyInverseIntegral[cf], 1. I]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainGrad-a"
]

Test[
	Chop[
		1/eps (CauchyInverse[SPCauchyInverseIntegral[cfaeps], 1. I] - 
    CauchyInverse[SPCauchyInverseIntegral[cf], 
     1. I])- (
     CauchyInverse[SPCauchyInverseIntegralDomainD[1, 0][cf],1. I] + 
     CauchyInverseDomainGrad[1, 0][SPCauchyInverseIntegral[cf], 1. I]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainD-a"
]
Test[
	Chop[
		1/eps (CauchyInverse[SPCauchyInverseIntegral[cfbeps], 1. I] - 
    CauchyInverse[SPCauchyInverseIntegral[cf], 
     1. I])- (CauchyInverse[SPCauchyInverseIntegralDomainD[0, 1][cf],
     1. I] + 
   CauchyInverseDomainGrad[0, 1][SPCauchyInverseIntegral[cf], 1. I]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainD-b"
]


Test[
	Chop[
	(CauchyInverseIntegral[cfa, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainGrad[1, 0][cf, 1. I]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGrad-a"
]

Test[
	Chop[
	(CauchyInverseIntegral[cfb, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainGrad[0, 1][cf, 1. I]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGrad-b"
]

Test[
	Chop[
	(CauchyInverseIntegral[+1,cfa, .15] - 
  CauchyInverseIntegral[+1,cf, .15])/eps - 
 CauchyInverseIntegralDomainGrad[1, 0][+1,cf, .15]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGrad-a-mid+1"
]



   
Test[
	Chop[(CauchyInverseIntegral[cfaeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[1, 0][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-a"
] 

Test[
	Chop[(CauchyInverseIntegral[cfbeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[0, 1][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-b"
] 



Test[
	Chop[(CauchyInverseIntegral[+1,cfaeps, a] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[1, 0][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-Domain-a-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfaeps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[1, 0][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-Domain-a-a"
] 




(* ::Section:: *)
(* Complex interval *)


(* ::Subsection:: *)
(*  Cauchy Inverse *)



eps = 10.^(-7);
a=0.1;
b=0.2 I;

cf = Fun[1/# &, Line[{a,b}]];
cfaeps = Fun[1/# &, Line[{a+eps,b}]];
cfbeps = Fun[1/# &, Line[{a,b+eps}]];

Test[
	Chop[
CauchyInverse[cf, 10000000.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-Decay-Complex"
]



(* ::Subsection:: *)
(* Cauchy Inverse Integral *)

g[z_] = CauchyInverseIntegral[cf,z];

Test[
	Chop[
CauchyInverseIntegral[cf, 10000000.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-Decay-Complex"
]

Test[
	Chop[
g'[1. I]-CauchyInverse[cf,1. I]
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-D-Complex"
]

Test[
	Chop[
CauchyInverseIntegral[cf, a - eps^2] - CauchyInverseIntegral[+1, cf, a] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-Complex"
]
  
   
Test[
	Chop[(CauchyInverseIntegral[cfaeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[1, 0][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-a-Complex"
] 

Test[
	Chop[(CauchyInverseIntegral[cfbeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[0, 1][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-b-Complex"
] 



Test[
	Chop[(CauchyInverseIntegral[+1,cfaeps, a] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[1, 0][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-Domain-a-a-Complex"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfaeps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[1, 0][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-Domain-a-a-Complex"
] 





(* ::Section:: *)
(* List *)


eps = 10.^(-7);
a=0.1;
b=0.2 I;
c=0.3;
d=-0.5 I + 0.4;
cf = Fun[1/# &, {Line[{a,b}], Line[{c,d}]}];
cfaeps = Fun[
   1/# &, {Line[{0.1 + eps, 0.2 I}], Line[{0.3, -0.5 I + 0.4}]}];
cfbeps = Fun[
   1/# &, {Line[{0.1 , 0.2 I+ eps}], Line[{0.3, -0.5 I + 0.4}]}];
cfceps = Fun[
   1/# &, {Line[{0.1 , 0.2 I}], Line[{0.3+ eps, -0.5 I + 0.4}]}];
cfdeps = Fun[
   1/# &, {Line[{0.1 , 0.2 I}], Line[{0.3, -0.5 I + 0.4+ eps}]}];  


Test[
	Chop[
CauchyInverseIntegral[cf[[1]], a - eps^2] - 
 CauchyInverseIntegral[+1, cf[[1]], a]
 ,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-List-1-Jump"
] 
  
   
Test[
	Chop[(CauchyInverseIntegral[cfaeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{1, 0}, {0, 0}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-a"
] 

Test[
	Chop[(CauchyInverseIntegral[cfbeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{0, 1}, {0, 0}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-b"
] 

Test[
	Chop[(CauchyInverseIntegral[cfceps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-c"
] 

Test[
	Chop[(CauchyInverseIntegral[cfdeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {0, 1}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-d"
] 

Test[
	Chop[(CauchyInverseIntegral[+1,cfceps, a] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-List-Domain-c-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfceps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-List-Domain-c-a"
] 

Test[
	Chop[(CauchyInverseIntegral[+1,cfdeps, a] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {0, 1}][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-List-Domain-d-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfdeps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {0, 1}][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-List-Domain-d-a"
] 



Test[
	Chop[(CauchyInverseIntegral[+1,cfaeps, a] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{1, 0}, {0, 0}][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-List-Domain-a-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfaeps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{1, 0}, {0, 0}][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-List-Domain-a-a"
] 

