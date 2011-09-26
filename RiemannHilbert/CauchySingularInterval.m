(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["RiemannHilbert`",{"RiemannHilbert`Common`"}];


CauchyInverseInverseFunction;
CauchyInverseFunction;
StieljesInverseFunction;
CauchyInverseFunctionD;
StieljesInverseFunctionD;
RTransform;

Begin["Private`"];


CauchyInverseInverseFunction[vf_IFun,r_]:=Module[{M,Mi,y},
M=LFun[ShiftList[DCT[vf]/2,1]//MakeFFTIndexRange//InverseFFT,UnitCircle];
Mi=Select[M-r//ComplexRoots,Abs[#]<=1&]//First;
MapFromInterval[vf,CircleToInterval[Mi]]
];
CauchyInverseInverseFunction[s_?SignQ,lfu_?UnitCircleFunQ,z_]:=Module[{rts,prts,mrts},rts=ComplexRoots[CauchyInverse[s,lfu]-z];prts=Select[rts,s (Abs[#1]-1)<=0&];Which[Length[prts]==1,First[prts],True,{}]]
CauchyInverseInverseFunction[lf_LFun,z_]:=Join[{CauchyInverseInverseFunction[+1,lf,z]},{CauchyInverseInverseFunction[-1,lf,z]}]//Flatten//If[Length[#]==1,First[#],#]&
CauchyInverseInverseFunction[s_?SignQ,lf_,z_]:=
MapFromCircle[lf,CauchyInverseInverseFunction[s,lf//ToUnitCircle,z+s Cauchy[lf//ToUnitCircle,MapToCircle[lf,\[Infinity]]]]];
RTransform[vf_,r_]:=CauchyInverseInverseFunction[vf,r]-1/r




CauchyInverseInverseFunction[pf_PFun,z_]:=(pf//Values//First)/z+(pf//Domain//First)


Hilbert[SingFun[if_IFun,{1/2,1/2}]]:=SingFun[Fun[-(if//DCT//ToChebyshevUSeries//GrowShiftRight)//InverseDCT,if//Domain],{0,0}];
HilbertInverse[SingFun[if_IFun,{0,0}]]:=SingFun[-Fun[(if//DCT//ShiftLeft//ToChebyshevTSeries//PadRight[#,Max[Length[#],2]]&)//InverseDCT,if//Domain],{1/2,1/2}];


Cauchy[sf:SingFun[_,{1/2,1/2}],z_]:=-I CauchyInverse[Hilbert[sf][[1]],z];
Cauchy[s_,sf:SingFun[_,{1/2,1/2}],z_]:=-I CauchyInverse[s,Hilbert[sf][[1]],z];
CauchyInverse[sf:SingFun[_,{0,0}],z_]:=-I CauchyInverse[sf[[1]],z];
CauchyInverse[s_,sf:SingFun[_,{0,0}],z_]:=-I CauchyInverse[s,sf[[1]],z];


CauchyInverseFunction[sf_SingFun,z_]:=CauchyInverseInverseFunction[-I (sf//Hilbert//First),z];
StieljesInverseFunction[sf_SingFun,z_]:=CauchyInverseFunction[sf,z/ (-2 \[Pi]\[NonBreakingSpace]I)];


RTransform[sf_SingFun,z_]:=StieljesInverseFunction[sf,z]-1/z


CauchyInverseInverseFunctionD[sfH_IFun,z_]:=1/BoundedCauchyInverseD[sfH,CauchyInverseInverseFunction[sfH,z]];
CauchyInverseFunctionD[sf:SingFun[_,{1/2,1/2}],z_]:=CauchyInverseInverseFunctionD[-I (sf//Hilbert//First),z];
StieljesInverseFunctionD[sf_SingFun,z_]:=CauchyInverseFunctionD[sf,z/ (-2 \[Pi]\[NonBreakingSpace]I)]/ (-2 \[Pi]\[NonBreakingSpace]I);

CauchyInverseInverseFunctionD[2][sfH_IFun,z_]:=-CauchyInverseInverseFunctionD[sfH,z]^3BoundedCauchyInverseD[2][sfH,CauchyInverseInverseFunction[sfH,z]];
CauchyInverseFunctionD[2][sf:SingFun[_,{1/2,1/2}],z_]:=CauchyInverseInverseFunctionD[2][-I (sf//Hilbert//First),z];
StieljesInverseFunctionD[2][sf_SingFun,z_]:=CauchyInverseFunctionD[2][sf,z/ (-2 \[Pi]\[NonBreakingSpace]I)]/ (-2 \[Pi]\[NonBreakingSpace]I)^2;


End[];
EndPackage[];
