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



BeginPackage["RiemannHilbert`RandomMatrices`",{"RiemannHilbert`","RiemannHilbert`Common`"}];


RandomSymmetric;
RandomOrthogonal;
HistogramPlot;

Begin["Private`"];



RandomSymmetric[n_,dist_:NormalDistribution[0,1/Sqrt[2]]]:=RandomVariate[dist,{n,n}]//(#+Transpose[#])/Sqrt[2 n]&;
RandomOrthogonal[n_,dist_:NormalDistribution[0,1/Sqrt[2]]]:=RandomVariate[dist,{n,n}]//QRDecomposition//First;
HistogramPlot[M_,dst_LFun,opts:OptionsPattern[]]:=Module[{Evs},
Evs=Table[M[]//Eigenvalues,{k,100}];
Show[Histogram[Evs//Flatten,60,"PDF"],Plot[dst[x],{x,-5,5},PlotStyle->{DarkRed,Thick}],opts]
];

HistogramPlot[M_,dst_,opts:OptionsPattern[]]:=Module[{Evs},
Evs=Table[M[]//Eigenvalues,{k,100}];
Show[Histogram[Evs//Flatten,60,"PDF"],Plot[dst[x],{x,dst//Domain//LeftEndpoint,dst//Domain//RightEndpoint},PlotStyle->{DarkRed,Thick}],opts]
];


End[];
EndPackage[];