(* Mathematica Test File *)


(* ::Section:: *)
(* Single *)


eps = 10.^(-7);
a=0.1;
b=0.2 I;

cf = Fun[1/# &, Line[{a,b}]];
cfaeps = Fun[1/# &, Line[{a+eps,b}]];
cfbeps = Fun[1/# &, Line[{a,b+eps}]];

Test[
	Chop[
CauchyInverseIntegral[cf, a - eps^2] - CauchyInverseIntegral[+1, cf, a] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump"
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

