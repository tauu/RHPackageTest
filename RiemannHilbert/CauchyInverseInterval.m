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



CauchyInverseBasis;
CauchyInversePlus;
SPCauchyInverseIntegral;
CauchyInverseIntegral;
CauchyInverseIntegralS;
CauchyInverseIntegralPlus;
CauchyInverseCurves;
CauchyInverseSeriesAtInfinity;
CauchyInverseBasisD;
CauchyInverseBasisDomainD;
CauchyInverseCurvesD;
CauchyInverseSeriesAtInfinityDomainD;
CauchyInverseSeriesAtInfinityD;
SPCauchyInverseIntegralDomainGrad;
SPCauchyInverseIntegralDomainD;
CauchyInverseIntegralPlusDomainGrad;
CauchyInverseIntegralPlusDomainD;
CauchyInverseIntegralPlusD;
CauchyInverseIntegralDomainD;
CauchyInverseIntegralDomainGrad;
CauchyInverseIntegralDomainGradBoundary;
CauchyInverseIntegralDomainDBoundary;
CauchyInverseDomainGrad;
CauchyInverseDomainD;
BoundedCauchyInverse;
EquilibriumMeasureSupport::usage="EquilibriumMeasureSupport[V] Computes the support of the equilibrium measure (currently only for convex V";
EquilibriumMeasure::usage="EquilibriumMeasure[V,x] Computes the equilibrium measure at a point x inside the support";
Begin["Private`"];



BoundedCauchyInverseBasis[UnitInterval,1,z_]:=1/2;
BoundedCauchyInverseBasis[UnitInterval,k_,z_?(#~NEqual~1.&)]:=1/2;
BoundedCauchyInverseBasis[UnitInterval,k_,z_?(#~NEqual~-1.&)]:=(-1)^(k-1)/2;
BoundedCauchyInverseBasis[UnitInterval,k_,z_]:=IntervalToInnerCircle[z]^(k-1)/2;
BoundedCauchyInverseBasis[_?SignQ,UnitInterval,1,z_]:=1/2;
BoundedCauchyInverseBasis[_?SignQ,UnitInterval,k_,z_?(#~NEqual~1.&)]:=1/2;
BoundedCauchyInverseBasis[_?SignQ,UnitInterval,k_,z_?(#~NEqual~-1.&)]:=(-1)^(k-1)/2;
BoundedCauchyInverseBasis[+1,UnitInterval,k_,z_]:=IntervalToBottomCircle[z]^(k-1)/2;
BoundedCauchyInverseBasis[-1,UnitInterval,k_,z_]:=IntervalToTopCircle[z]^(k-1)/2;

BoundedCauchyInverseBasis[d_,k_,z_]:=BoundedCauchyInverseBasis[UnitInterval,k,MapToInterval[d,z]];
BoundedCauchyInverseBasis[s_?SignQ,d_,k_,z_]:=BoundedCauchyInverseBasis[s,UnitInterval,k,MapToInterval[d,z]];


CauchyInverseBasis[d_,1,z_]:=1/2-MapToInterval[d,z]/(2 Sqrt[MapToInterval[d,z]+1] Sqrt[MapToInterval[d,z]-1]);
CauchyInverseBasis[d_,k_,z_]:=BoundedCauchyInverseBasis[d,k,z];
CauchyInverseBasis[s_?SignQ,d_,1,z_]:=1/2+s I MapToInterval[d,z]/(2 Sqrt[1-MapToInterval[d,z]^2]);
CauchyInverseBasis[s_?SignQ,d_,k_,z_]:=BoundedCauchyInverseBasis[s,d,k,z];

CauchyInverse[f_IFun?(NZeroQ[Mean[#]]&),z_]:=MapDot[CauchyInverseBasis[f,#+1,z]&,f//DCT//Rest];
CauchyInverse[s_?SignQ,f_IFun?(NZeroQ[Mean[#]]&),z_]:=MapDot[CauchyInverseBasis[s,f,#+1,z]&,f//DCT//Rest];

CauchyInverse[f_IFun,z_]:=MapDot[CauchyInverseBasis[f,#,z]&,f//DCT];
CauchyInverse[s_?SignQ,f_IFun,z_]:=MapDot[CauchyInverseBasis[s,f,#,z]&,f//DCT];

BoundedCauchyInverse[f_IFun,z_]:=MapDot[BoundedCauchyInverseBasis[f,#,z]&,f//DCT];
BoundedCauchyInverse[s_?SignQ,f_IFun,z_]:=MapDot[BoundedCauchyInverseBasis[s,f,#,z]&,f//DCT];

CauchyInversePlus[f_IFun,z_]/;DomainMemberQ[f,z]:=f[z];
CauchyInversePlus[f_IFun,z_]:=2 CauchyInverse[f,z];

HilbertInverse[f_IFun,z_]:=-Sqrt[1-MapToInterval[f,z]^2] MapDot[ChebyshevU[#-1,MapToInterval[f,z]]&,f//DCT//Rest];


CauchyInverseMatrix[s_?SignQ,f_IFun]:=Transpose[Array[CauchyInverseBasis[s,f,#,Points[f]]&,Length[f]]].TransformMatrix[f];
CauchyInverseMatrix[s_?SignQ,f_IFun,g_IFun]/;Domain[f]==Domain[g]:=With[{pts=Points[g]},Transpose[Array[CauchyInverseBasis[s,f,#,pts]&,Length[f]]].TransformMatrix[f]];
CauchyInverseMatrix[_?SignQ,f_IFun,g_IFun]:=With[{pts=Points[g]},Transpose[Array[CauchyInverseBasis[f,#,pts]&,Length[f]]].TransformMatrix[f]];


CauchyInversePlusMatrix::usage="Returns the matrix corresponding to a list {f,g} f^+ + g^+ + f^- + g^-";
CauchyInversePlusMatrix[l_List]:=Join@@((RightJoin@@#)&/@MatrixMap[If[#[[1]]===#[[2]] ,IdentityMatrix[Length[#[[1]]]],2 CauchyInverseMatrix[+1,#[[2]],#[[1]]]]&,Outer[List,l,l]]);

CauchyInverseCurves[l_List]:=FromValueList[l,LinearSolve[CauchyInversePlusMatrix[l],l//ToValueList]];

CauchyInverse[l_List,z_]:=Plus@@(CauchyInverse[#,z]&/@CauchyInverseCurves[l]);
CauchyInverse[s_?SignQ,l_List,z_]:=Plus@@(If[DomainMemberQ[#,z],CauchyInverse[s,#,z],CauchyInverse[#,z]]&/@CauchyInverseCurves[l]);

CauchyInverseSeriesAtInfinity[f_IFun]:=DCT[f][[2]]/(4 MapToIntervalSeriesAtInfinity[f,1]);
CauchyInverseSeriesAtInfinity[l_List]:=Plus@@(CauchyInverseSeriesAtInfinity[#]&/@CauchyInverseCurves[l]);



CauchyInverseBasisD[d_,1,z_]:=MapToIntervalD[d,z]/(2 (-1+MapToInterval[d,z])^(3/2) (1+MapToInterval[d,z])^(3/2));
CauchyInverseBasisD[s_?SignQ,d_,1,z_]:=-I s MapToIntervalD[d,z]/(2 Sqrt[1-MapToInterval[d,z]^2] (MapToInterval[d,z]^2-1));


CauchyInverseBasisD[d_,k_,z_]:=(k-1)IntervalToInnerCircle[MapToInterval[d,z]]^(k-2)/2IntervalToInnerCircle'[MapToInterval[d,z]] MapToIntervalD[d,z];


CauchyInverseBasisD[+1,d_,k_,z_]:=(k-1)IntervalToBottomCircle[MapToInterval[d,z]]^(k-2)/2IntervalToBottomCircle'[MapToInterval[d,z]] MapToIntervalD[d,z];
CauchyInverseBasisD[-1,d_,k_,z_]:=(k-1)IntervalToTopCircle[MapToInterval[d,z]]^(k-2)/2IntervalToTopCircle'[MapToInterval[d,z]] MapToIntervalD[d,z];



CauchyInverseBasisDomainD[spc__][d_,1,z_]:=MapToIntervalDomainD[spc][d,z]/(2 (-1+MapToInterval[d,z])^(3/2) (1+MapToInterval[d,z])^(3/2));
CauchyInverseBasisDomainD[spc__][s_?SignQ,d_,1,z_]:=-I s MapToIntervalDomainD[spc][d,z]/(2 Sqrt[1-MapToInterval[d,z]^2] (MapToInterval[d,z]^2-1));


CauchyInverseBasisDomainD[spc__][d_,k_,z_]:=(k-1)IntervalToInnerCircle[MapToInterval[d,z]]^(k-2)/2IntervalToInnerCircle'[MapToInterval[d,z]] MapToIntervalDomainD[spc][d,z];


CauchyInverseBasisDomainD[spc__][+1,d_,k_,z_]:=(k-1)IntervalToBottomCircle[MapToInterval[d,z]]^(k-2)/2IntervalToBottomCircle'[MapToInterval[d,z]] MapToIntervalDomainD[spc][d,z];
CauchyInverseBasisDomainD[spc__][-1,d_,k_,z_]:=(k-1)IntervalToTopCircle[MapToInterval[d,z]]^(k-2)/2IntervalToTopCircle'[MapToInterval[d,z]] MapToIntervalDomainD[spc][d,z];








CauchyInverseMatrixD[{spc__},{0,0}][_,f_IFun,g_IFun]:=With[{pts=Points[g]},Transpose[Array[CauchyInverseBasisDomainD[spc][f,#,pts]&,Length[f]]].TransformMatrix[f]];
CauchyInverseMatrixD[{0,0},{spc__}][_,f_IFun,g_IFun]:=With[{pts=Points[g]},Transpose[Array[CauchyInverseBasisD[f,#,pts]PointsD[spc][g]&,Length[f]]].TransformMatrix[f]];


CauchyInversePlusMatrixD[spca__][fl_List]:=Join@@((RightJoin@@#)&/@MatrixMap[Which[#[[1]]===#[[2]] ,
ZeroMatrix[Length[#[[1,1]]]],
#[[1,2]]==#[[2,2]]=={0,0},
ZeroMatrix[Length[#[[1,1]]],Length[#[[2,1]]]],
True,
2 CauchyInverseMatrixD[#[[2,2]],#[[1,2]]][+1,#[[2,1]],#[[1,1]]]
]&,Outer[List,Thread[{fl,{spca}}],Thread[{fl,{spca}}],1]]);

CauchyInverseCurvesD[spc__][l_List]:=Module[{plusmatin},
plusmatin=CauchyInversePlusMatrix[l]//Inverse;

FromValueList[l,
-plusmatin.CauchyInversePlusMatrixD[spc][l].plusmatin.(l//ToValueList)+plusmatin.ToValueListD[spc][l]
]];







CauchyInverseDomainGrad[spc__][f_IFun,z_]:=MapDot[CauchyInverseBasisDomainD[spc][f,#,z]&,f//DCT];
CauchyInverseDomainGrad[spc__][s_?SignQ,f_IFun,z_]:=MapDot[CauchyInverseBasisDomainD[spc][s,f,#,z]&,f//DCT];


CauchyInverseDomainD[spc__][f_IFun,z_]:=CauchyInverseDomainGrad[spc][f,z]+MapDot[CauchyInverseBasis[f,#,z]&,ValuesDomainD[spc][f]//DCT];
CauchyInverseDomainD[spc__][s_?SignQ,f_IFun,z_]:=CauchyInverseDomainGrad[spc][s,f,z]+MapDot[CauchyInverseBasis[s,f,#,z]&,ValuesDomainD[spc][f]//DCT];


CauchyInverseSeriesAtInfinityDomainD[spc__][f_IFun]:=DCT[ValuesDomainD[spc][f]][[2]]/(4 MapToIntervalSeriesAtInfinity[f,1])-(DCT[f][[2]] MapToIntervalSeriesAtInfinityD[spc][f,1])/(4 MapToIntervalSeriesAtInfinity[f,1]^2);

CauchyInverseSeriesAtInfinityValuesD[f_IFun]:=TransformMatrix[f][[2,All]]/(4 MapToIntervalSeriesAtInfinity[f,1]);


CauchyInverseSeriesAtInfinityD[spc__][fl_List]:=Plus@@((DCT[#][[2]]/(4 MapToIntervalSeriesAtInfinity[#,1])&/@CauchyInverseCurvesD[spc][fl])-(If[#[[2]]=={0,0},0,DCT[#[[1]]][[2]]MapToIntervalSeriesAtInfinityD[Sequence@@#[[2]]][#[[1]],1]/(4 MapToIntervalSeriesAtInfinity[#[[1]],1]^2)]&/@Thread[{CauchyInverseCurves[fl],{spc}}]));



CauchyInverseD[f_IFun,z_]:=MapDot[CauchyInverseBasisD[f,#,z]&,f//DCT];
CauchyInverseD[s_?SignQ,f_IFun,z_]:=MapDot[CauchyInverseBasisD[s,f,#,z]&,f//DCT];
CauchyInversePlusD[f_IFun,z_]:=2 CauchyInverseD[f,z];
CauchyInversePlusD[f_IFun,z_]/;DomainMemberQ[f,z]:=f'[z];



SPCauchyInverseIntegral[f_IFun?IntervalFunQ]:=1/MapToIntervalD[f,0.] IFun[1/2  ( MapOuter[Which[#==1,0,#==2,2/(#-1),True,1/(#-1)]&,(DCT[f]//GrowShiftRight)]-PadRight[MapOuter[If[#==1,0,1/(#-1)]&,(DCT[f]//GrowShiftLeft)],Length[f]+1])//InverseDCT,Domain[f]];

CauchyInverseIntegralLogTerm[f_IFun,z_]:=DCT[f][[2]]/(4 MapToIntervalD[f,0.]) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]-Log[IntervalToInnerCircle[MapToInterval[f,z]]]);
CauchyInverseIntegralLogTerm[s_?SignQ,f_IFun,z_]/;LeftEndpoint[f]~NEqual~z:=DCT[f][[2]]/(4MapToIntervalD[f,0.]) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]+s \[Pi]   I);
CauchyInverseIntegralLogTerm[s_?SignQ,f_IFun,z_]/;RightEndpoint[f]~NEqual~z:=DCT[f][[2]]/(4MapToIntervalD[f,0.]) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]);
CauchyInverseIntegralLogTerm[s_?SignQ,f_IFun,z_]:=DCT[f][[2]]/(4MapToIntervalD[f,0.]) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]+s ArcCos[MapToInterval[f,z]]   I);
CauchyInverseIntegralLogTermLeft[s_?SignQ,f_IFun,z_]:=DCT[f][[2]]/(4MapToIntervalD[f,0.]) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]-Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]]+s I \[Pi]);

CauchyInverseIntegral[f_IFun?IntervalFunQ,z_]:=
CauchyInverse[SPCauchyInverseIntegral[f],z]+CauchyInverseIntegralLogTerm[f,z];

CauchyInverseIntegralS[s_?SignQ,f_IFun?IntervalFunQ,z_]:=
CauchyInverse[s,SPCauchyInverseIntegral[f],z]+CauchyInverseIntegralLogTerm[s,f,z];

CauchyInverseIntegral[s_?SignQ,f_IFun?IntervalFunQ,z_]/;DomainMemberQ[f,z]:=
CauchyInverseIntegralS[s,f,z];

CauchyInverseIntegral[s_?SignQ,f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=
CauchyInverse[SPCauchyInverseIntegral[f],z]+CauchyInverseIntegralLogTermLeft[s,f,z];
CauchyInverseIntegral[_?SignQ,f_IFun?IntervalFunQ,z_]:=CauchyInverseIntegral[f,z];

CauchyInverseIntegralPlus[f_IFun?IntervalFunQ,z_]/;DomainMemberQ[f,z]:=SPCauchyInverseIntegral[f][z];
CauchyInverseIntegralPlus[f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=CauchyInversePlus[SPCauchyInverseIntegral[f],z]-1/(2 MapToIntervalD[f,0.]) DCT[f][[2]](Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]]);
CauchyInverseIntegralPlus[f_IFun?IntervalFunQ,z_]:=2 CauchyInverseIntegral[f,z];



CauchyInverseIntegral[l_List,z_]:=Plus@@(CauchyInverseIntegral[#,z]&/@CauchyInverseCurves[l]);
CauchyInverseIntegral[s_?SignQ,l_List,z_]:=Plus@@(CauchyInverseIntegral[s,#,z]&/@CauchyInverseCurves[l]);

CauchyInverseIntegralPlus[l_,z_]:=Plus@@(CauchyInverseIntegralPlus[#,z]&/@CauchyInverseCurves[l]);



SPCauchyInverseIntegralDomainGrad[spc__][f_]:=-(MapToIntervalDDomainD[spc][f,0.]/MapToIntervalD[f,0.]^2)IFun[1/2  ( MapOuter[Which[#==1,0,#==2,2/(#-1),True,1/(#-1)]&,(DCT[f]//GrowShiftRight)]-PadRight[MapOuter[If[#==1,0,1/(#-1)]&,(DCT[f]//GrowShiftLeft)],Length[f]+1])//InverseDCT,Domain[f]];

SPCauchyInverseIntegralDomainD[spc__][f_]:=SPCauchyInverseIntegralDomainGrad[spc][f]+1/MapToIntervalD[f,0.] IFun[1/2  ( MapOuter[Which[#==1,0,#==2,2/(#-1),True,1/(#-1)]&,(DCT[ValuesDomainD[spc][f]]//GrowShiftRight)]-PadRight[MapOuter[If[#==1,0,1/(#-1)]&,(DCT[ValuesDomainD[spc][f]]//GrowShiftLeft)],Length[f]+1])//InverseDCT,Domain[f]];

SPCauchyInverseIntegralValuesD[f_IFun?IntervalFunQ]:=1/MapToIntervalD[f,0.] Inverse[TransformMatrix[f]].ColumnMap[1/2 MapOuter[Which[#==1,0,#==2,2/(#-1),True,1/(#-1)]&,(#//ShiftRight)]-MapOuter[If[#==1,0,1/(#-1)]&,(#//GrowShiftLeft)]&,IdentityMatrix[f//Length]].TransformMatrix[f];



CauchyInverseIntegralLogTermDomainGrad[1,0][f_IFun,z_]:=-DCT[f][[2]] MapToIntervalDDomainD[1,0][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]-Log[IntervalToInnerCircle[MapToInterval[f,z]]])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(-1/ (RightEndpoint[f]-LeftEndpoint[f])-(IntervalToInnerCircleD[MapToInterval[f,z ]]MapToIntervalDomainD[1,0][f,z])/IntervalToInnerCircle[MapToInterval[f,z]]));
CauchyInverseIntegralLogTermDomainGrad[0,1][f_IFun,z_]:=-DCT[f][[2]] MapToIntervalDDomainD[0,1][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]-Log[IntervalToInnerCircle[MapToInterval[f,z]]])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(1/ (RightEndpoint[f]-LeftEndpoint[f])-(IntervalToInnerCircleD[MapToInterval[f,z ]]MapToIntervalDomainD[0,1][f,z])/IntervalToInnerCircle[MapToInterval[f,z]]));

CauchyInverseIntegralLogTermDomainGrad[1,0][s_,f_IFun,z_]/;RightEndpoint[f]~NEqual~z:=-DCT[f][[2]] MapToIntervalDDomainD[1,0][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(-1/ (RightEndpoint[f]-LeftEndpoint[f])));
CauchyInverseIntegralLogTermDomainGrad[0,1][s_,f_IFun,z_]/;LeftEndpoint[f]~NEqual~z:=-DCT[f][[2]] MapToIntervalDDomainD[0,1][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]+s I \[Pi])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(1/ (RightEndpoint[f]-LeftEndpoint[f])));



CauchyInverseIntegralLogTermDomainGrad[1,0][s_,f_IFun,z_]:=-DCT[f][[2]] MapToIntervalDDomainD[1,0][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]+s I ArcCos[MapToInterval[f,z]])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(-1/ (RightEndpoint[f]-LeftEndpoint[f])+s I ArcCos'[MapToInterval[f,z]]MapToIntervalDomainD[1,0][f,z ]));
CauchyInverseIntegralLogTermDomainGrad[0,1][s_,f_IFun,z_]:=-DCT[f][[2]] MapToIntervalDDomainD[0,1][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]+s I ArcCos[MapToInterval[f,z]])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(1/ (RightEndpoint[f]-LeftEndpoint[f])+s I ArcCos'[MapToInterval[f,z]]MapToIntervalDomainD[0,1][f,z ]));




CauchyInverseIntegralLogTermLeftDomainGrad[1,0][s_,f_IFun,z_]:=-DCT[f][[2]] MapToIntervalDDomainD[1,0][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]-Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]]+s I \[Pi])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(-1/ (RightEndpoint[f]-LeftEndpoint[f])-(IntervalToInnerCircleD[MapToInterval[f,z ]]MapToIntervalDomainD[1,0][f,z])/IntervalToInnerCircle[MapToInterval[f,z]]));
CauchyInverseIntegralLogTermLeftDomainGrad[0,1][s_,f_IFun,z_]:=-DCT[f][[2]] MapToIntervalDDomainD[0,1][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]-Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]]+s I \[Pi])+(DCT[f][[2]]/(4 MapToIntervalD[f,0.])(1/ (RightEndpoint[f]-LeftEndpoint[f])-(IntervalToInnerCircleD[MapToInterval[f,z ]]MapToIntervalDomainD[0,1][f,z])/IntervalToInnerCircle[MapToInterval[f,z]]));

CauchyInverseIntegralLogTermDomainGradBoundary[1,0][s_,f_IFun,z_]/;LeftEndpoint[f]~NEqual~z:=-DCT[f][[2]] MapToIntervalDDomainD[1,0][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]+s  \[Pi] I )+DCT[f][[2]]/(4 MapToIntervalD[f,0.])(-1/ (RightEndpoint[f]-LeftEndpoint[f]) );
CauchyInverseIntegralLogTermDomainGradBoundary[1,0][s_,f_IFun,z_]/;RightEndpoint[f]~NEqual~z:=-DCT[f][[2]] MapToIntervalDDomainD[1,0][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])] )+DCT[f][[2]]/(4 MapToIntervalD[f,0.])(-1/ (RightEndpoint[f]-LeftEndpoint[f]) );
CauchyInverseIntegralLogTermDomainGradBoundary[0,1][s_,f_IFun,z_]/;LeftEndpoint[f]~NEqual~z:=-DCT[f][[2]] MapToIntervalDDomainD[0,1][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])]+s  \[Pi] I )+DCT[f][[2]]/(4 MapToIntervalD[f,0.])(1/ (RightEndpoint[f]-LeftEndpoint[f]) );
CauchyInverseIntegralLogTermDomainGradBoundary[0,1][s_,f_IFun,z_]/;RightEndpoint[f]~NEqual~z:=-DCT[f][[2]] MapToIntervalDDomainD[0,1][f,0.]/(4 MapToIntervalD[f,0.]^2) (Log[1/4 (RightEndpoint[f]-LeftEndpoint[f])] )+DCT[f][[2]]/(4 MapToIntervalD[f,0.])(1/ (RightEndpoint[f]-LeftEndpoint[f]) );



CauchyInverseIntegralDomainGrad[spc__][s_?SignQ,f_IFun?IntervalFunQ,z_]/;LeftEndpoint[f]~NEqual~z||RightEndpoint[f]~NEqual~z:=
CauchyInverse[s,SPCauchyInverseIntegralDomainGrad[spc][f],z]+CauchyInverseIntegralLogTermDomainGrad[spc][s,f,z];

CauchyInverseIntegralDomainGrad[spc__][s_?SignQ,f_IFun?IntervalFunQ,z_]/;DomainMemberQ[f,z]:=
CauchyInverse[s,SPCauchyInverseIntegralDomainGrad[spc][f],z]+CauchyInverseDomainGrad[spc][s,SPCauchyInverseIntegral[f],z]+CauchyInverseIntegralLogTermDomainGrad[spc][s,f,z];

 CauchyInverseIntegralDomainGrad[spc__][s_?SignQ,f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=
CauchyInverse[SPCauchyInverseIntegralDomainGrad[spc][f],z]+CauchyInverseDomainGrad[spc][SPCauchyInverseIntegral[f],z]+CauchyInverseIntegralLogTermLeftDomainGrad[spc][s,f,z];

CauchyInverseIntegralDomainGrad[spc__][f_IFun?IntervalFunQ,z_]:=CauchyInverse[SPCauchyInverseIntegralDomainGrad[spc][f],z]+CauchyInverseDomainGrad[spc][SPCauchyInverseIntegral[f],z]+CauchyInverseIntegralLogTermDomainGrad[spc][f,z];


CauchyInverseIntegralDomainGradBoundary[spc__][s_?SignQ,f_IFun?IntervalFunQ,z_]:=CauchyInverseIntegralLogTermDomainGradBoundary[spc][s,f,z]+CauchyInverse[s,SPCauchyInverseIntegralDomainGrad[spc][f],z];

CauchyInverseIntegralDomainD[spc__][f_IFun?IntervalFunQ,z_]:=
CauchyInverseIntegralDomainGrad[spc][f,z]+CauchyInverseIntegral[DomainD[spc][f],z];

CauchyInverseIntegralDomainD[spc__][s_?SignQ,f_IFun?IntervalFunQ,z_]/;DomainMemberQ[f,z]:=
CauchyInverseIntegralDomainGrad[spc][s,f,z]+CauchyInverseIntegral[s,DomainD[spc][f],z];

CauchyInverseIntegralDomainD[spc__][s_?SignQ,f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=
CauchyInverseIntegralDomainGrad[spc][s,f,z]+CauchyInverseIntegral[s,DomainD[spc][f],z];

CauchyInverseIntegralDomainD[spc__][_,f_IFun?IntervalFunQ,z_]:=CauchyInverseIntegralDomainD[spc][f,z];

CauchyInverseIntegralDomainDBoundary[spc__][s_?SignQ,f_IFun?IntervalFunQ,z_]:=
CauchyInverseIntegralDomainGradBoundary[spc][s,f,z]+CauchyInverseIntegral[s,DomainD[spc][f],z];


CauchyInverseIntegralPlusDomainGrad[spc__][f_IFun?IntervalFunQ,z_]/;DomainMemberQ[f,z]:=SPCauchyInverseIntegralDomainGrad[spc][f][z];

CauchyInverseIntegralPlusDomainGrad[spc__][f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=2(MapDot[CauchyInverseBasisDomainD[spc][f,#,z]&,SPCauchyInverseIntegral[f]//DCT]+MapDot[CauchyInverseBasis[f,#,z]&,SPCauchyInverseIntegralDomainGrad[spc][f]//DCT])-(
-MapToIntervalDDomainD[spc][f,0.]/(2 MapToIntervalD[f,0.]^2) DCT[f][[2]](Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]])+1/(2 MapToIntervalD[f,0.]) DCT[f][[2]](IntervalToInnerCircle'[MapToInterval[f,z]] MapToIntervalDomainD[spc][f,z]/IntervalToInnerCircle[MapToInterval[f,z]])
);

CauchyInverseIntegralPlusDomainGrad[spc__][f_IFun?IntervalFunQ,z_]:=2 CauchyInverseIntegralDomainGrad[spc][f,z];


CauchyInverseIntegralPlus[f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=CauchyInversePlus[SPCauchyInverseIntegral[f],z]-1/(2 MapToIntervalD[f,0.]) DCT[f][[2]](Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]]);
CauchyInverseIntegralPlus[f_IFun?IntervalFunQ,z_]:=2 CauchyInverseIntegral[f,z];


CauchyInverseIntegralPlusDomainD[spc__][f_IFun?IntervalFunQ,z_]/;DomainMemberQ[f,z]:=CauchyInverseIntegralPlusDomainGrad[spc][f,z]+BaryDomainD[spc][SPCauchyInverseIntegral[f],z];


CauchyInverseIntegralPlusDomainD[spc__][f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=2(MapDot[CauchyInverseBasisDomainD[spc][f,#,z]&,SPCauchyInverseIntegral[f]//DCT]+MapDot[CauchyInverseBasis[f,#,z]&,SPCauchyInverseIntegralDomainD[spc][f]//DCT])-(
-MapToIntervalDDomainD[spc][f,0.]/(2 MapToIntervalD[f,0.]^2) DCT[f][[2]](Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]])+1/(2 MapToIntervalD[f,0.]) DCT[ValuesDomainD[spc][f]][[2]](Log[Abs[IntervalToInnerCircle[MapToInterval[f,z]]]])+1/(2 MapToIntervalD[f,0.]) DCT[f][[2]](IntervalToInnerCircle'[MapToInterval[f,z]] MapToIntervalDomainD[spc][f,z]/IntervalToInnerCircle[MapToInterval[f,z]])
);
CauchyInverseIntegralPlusDomainD[spc__][f_IFun?IntervalFunQ,z_]:=2 CauchyInverseIntegralDomainD[spc][f,z];



CauchyInverseIntegralD[f_IFun?IntervalFunQ,z_]:=
CauchyInverseD[SPCauchyInverseIntegral[f],z]-1/(4 MapToIntervalD[f,0.]) DCT[f][[2]]IntervalToInnerCircle'[MapToInterval[f,z]]MapToIntervalD[f,z]/IntervalToInnerCircle[MapToInterval[f,z]];


CauchyInverseIntegralPlusD[f_IFun?IntervalFunQ,z_]/;DomainMemberQ[f,z]:=SPCauchyInverseIntegral[f]'[z];
CauchyInverseIntegralPlusD[f_IFun?IntervalFunQ,z_]/;NZeroQ[Im[MapToInterval[f,z]]]&&Re[MapToInterval[f,z]]<=-1.:=CauchyInversePlusD[SPCauchyInverseIntegral[f],z]-1/(2 MapToIntervalD[f,0.]) DCT[f][[2]](IntervalToInnerCircle'[MapToInterval[f,z]]MapToIntervalD[f,z]/IntervalToInnerCircle[MapToInterval[f,z]]);
CauchyInverseIntegralPlusD[f_IFun?IntervalFunQ,z_]:=2 CauchyInverseIntegralD[f,z];


CauchyInverseIntegralPlusD[l_List,z_]:=Plus@@(CauchyInverseIntegralPlusD[#,z]&/@CauchyInverseCurves[l]);


CauchyInverseIntegralPlusDomainD[spc__][fl_List,z_]:=Plus@@(
Which[
DomainMemberQ[#[[1]],z]&&#[[3]]=={0,0},
SPCauchyInverseIntegral[#[[2]]][z],
DomainMemberQ[#[[1]],z],
SPCauchyInverseIntegralDomainGrad[Sequence@@#[[3]]][#[[1]]][z]+SPCauchyInverseIntegral[#[[2]]][z]+BaryDomainD[Sequence@@#[[3]]][SPCauchyInverseIntegral[#[[1]]],z],
#[[3]]=={0,0},
CauchyInverseIntegralPlus[#[[2]],z],
True,
CauchyInverseIntegralPlusDomainGrad[Sequence@@#[[3]]][#[[1]],z]+CauchyInverseIntegralPlus[#[[2]],z]
]&/@Thread[{CauchyInverseCurves[fl],CauchyInverseCurvesD[spc][fl],{spc}}]
);


CauchyInverseIntegralDomainD[spc__][fl_List,z_]:=Plus@@(
Which[
#[[3]]=={0,0},
CauchyInverseIntegral[#[[2]],z],
True,
CauchyInverseIntegralDomainGrad[Sequence@@#[[3]]][#[[1]],z]+CauchyInverseIntegral[#[[2]],z]
]&/@Thread[{CauchyInverseCurves[fl],CauchyInverseCurvesD[spc][fl],{spc}}]
);


CauchyInverseIntegralDomainD[spc__][s_,fl_List,z_]:=Module[{crv,crvD,sc},
Plus@@(
(
{crv,crvD,sc}=#;
Which[
sc=={0,0},
CauchyInverseIntegral[s,crvD,z],
DomainMemberQ[crv,z],
CauchyInverseIntegral[s,crvD,z]+CauchyInverseIntegralDomainGrad[Sequence@@sc][s,crv,z],
True,
CauchyInverseIntegralDomainGrad[Sequence@@sc][crv,z]+CauchyInverseIntegral[s,crvD,z]
]
)&/@Thread[{CauchyInverseCurves[fl],CauchyInverseCurvesD[spc][fl],{spc}}]
)
];


CauchyInverseIntegralDomainDBoundary[spc__][s_,fl_List,z_]:=Module[{crv,crvD,sc},
Plus@@(
(
{crv,crvD,sc}=#;
Which[
sc=={0,0},
CauchyInverseIntegral[s,crvD,z]+CauchyInverseIntegralD[crv,z],
LeftEndpoint[crv]~NEqual~z||RightEndpoint[crv]~NEqual~z,
CauchyInverseIntegral[s,crvD,z]+CauchyInverseIntegralDomainGradBoundary[Sequence@@sc][s,crv,z],
True,
CauchyInverseIntegralDomainGrad[Sequence@@sc][crv,z]+CauchyInverseIntegral[s,crvD,z]
]
)&/@Thread[{CauchyInverseCurves[fl],CauchyInverseCurvesD[spc][fl],{spc}}]
)
];



EquilibriumMeasureSupport[V_,retin_:{-1,1}]:=Module[{ret,retold,F,J},
F[{a_,b_}]:=Module[{Vf},
Vf=Fun[V,Line[{a,b}]];
{DCT[Vf'][[1]],(b-a)/8 DCT[Vf'][[2]]-1}];
J[{a_,b_}]:=Module[{Vf,x},
Vf=Fun[V,Line[{a,b}]];
x=IFun[Points[UnitInterval,Length[Vf]],Vf//Domain];
({
 {DCT[(1/2-x/2)Vf''][[1]], DCT[(1/2+x/2)Vf''][[1]]},
 {1/8 ((b-a)DCT[(1/2-x/2)Vf''][[2]]-DCT[Vf'][[2]]), 1/8 ((b-a)DCT[(1/2+x/2)Vf''][[2]]+DCT[Vf'][[2]])}
})
];
ret=retin;
retold={0,0};
While[Norm[ret-retold]>$MachineTolerance,
retold=ret;
ret=ret-LeastSquares[J[ret],F[ret]];
];
ret//Sort];
EquilibriumMeasure[V_,retin_:{-1,1},x_]:=-(1/(2\[Pi]))HilbertInverse[IFun[V',Line[EquilibriumMeasureSupport[V,retin]]],x];


End[];
EndPackage[];
