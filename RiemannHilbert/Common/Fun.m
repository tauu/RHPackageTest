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



BeginPackage[$CommonPackage];



Fun::usage="
Decides between IFun and LFun, and allows Line[{a,b,c}]";





ZeroAtInfinityFun::usage="Constructs a Fun with the default value of zero at infinity";
IdentityAtInfinityFun::usage="Constructs a Fun with the default value of the identity matrix at infinity";

FunListDot::usage="FunListDot[{f1,f2,...,fn},{g1,g2,...,gn}] returns {f1.g1,f2.g2,...,fn.gn}";

DCTPlot::usage="LogPlots the norm of the Chebyshev coefficients of an IFun";
FFTPlot::usage="LogPlots the norm of the Fourier coefficients of an LFun";

ComplexRoots::usage="Returns all roots";


Curve::usage="Turns an IFun into a domain";



FunQ::usage="Tests if an object is in IFun or LFun.";
ListFunQ::usage="Tests if an object is in IFun whose values are a list.";
VectorFunQ::usage="Tests if an object is in IFun whose values are a vector.";
MatrixFunQ::usage="Tests if an object is in IFun whose values are a matrix.";
ArrayFunQ::usage="Tests if an object is in IFun whose values are an array.";

AddIdentityMatrix::usage=
"Adds IdentityMatrix[2] to a list of ifuns.";

SubtractIdentityMatrix;

DomainPlot::usage=
"DomainPlot[ifun] plots the domain of ifun. DomainPlot[list] plots the union of the domains of a list of IFuns";

Options[DomainPlot]=Options[Graphics];

DomainIntegrate::usage=
"DomainIntegrate[ifun] returns the definite integral of the function ifun over its domain.";
ReduceDimension::usage=
"Reduces the length of an IFun by one.";
ReduceDimensionIntegrate::usage=
"ReduceDimensionIntegrate[ifun] returns the ReduceDimension[Integrate[ifun]].";

Domain::usage=
"Domain[ifun] returns the domain of an IFun.";
Values::usage=
"Values[ifun] returns the values of an IFun at Points[ifun].";
Points::usage=
"Points[ifun] returns the the mapped Chebyshev\[Dash]Lobatto points.  Points[d,n] returns n mapped Chebyshev\[Dash]Lobatto points over a domain d.";

SetDomain::usage="SetDomain[f,d] changes the domain of the Fun (or List of Funs) f to d";



DomainQ::usage=
"Test whether something is a domain (Line, Arc, Circle, Curve, etc).";


ComplexRoots::usage="Returns all roots";


Curve::usage="Turns an IFun into a domain";

OneFun;

FunQ;
ReImLinePlot;
MatrixFunQ;
ArrayFunQ;
ToArrayOfFuns;
ToArrayFun;
ListFunQ;
ToMatrixOfFuns;
$FunFormat;
ScalarFunQ;
VectorFunQ;
Fun;
ZeroAtInfinityFun;
MeanZero;




SetupFun[head_]:=Module[{MapToValues},
FunQ[_head]:=True;
head/:Map[f_,g_head]:=head[f/@Values[g],Domain[g]];

Length[if_head]^:=if//Values//Length;
Points[if_head]:=Points[if//Domain,if//Length];

FastPlus[f__head]:=head[Plus@@(Values/@{f}),Domain[{f}[[1]]]];
FastTimes[f__head]:=head[Times@@(Values/@{f}),Domain[{f}[[1]]]];

f_head+g_head^:=f~FastPlus~g;

Times[f_head,g_head]^:=f~FastTimes~g;
head/:f_?ConstantQ+g_head:=head[f+Values[g],g//Domain];
head/:g_head+f_?ConstantQ:=head[Values[g]+f,g//Domain];
head/:Times[f_?ConstantQ,g_head]:=head[f Values[g],g//Domain];
head/:Times[g_head,f_?ConstantQ]:=head[Values[g]f,g//Domain];
head/:f_head^c_?ConstantQ:=head[Values[f]^c,f//Domain];
head/:c_?ConstantQ^f_head:=head[c^Values[f],f//Domain];
Dot[f_head?ArrayFunQ,g_head?ArrayFunQ]^:=ToArrayFun[ToArrayOfFuns[f].ToArrayOfFuns[g]];

head/:Dot[f_List?(!ArrayFunQ[#]&),g_head?ArrayFunQ]:=ToArrayFun[f.ToArrayOfFuns[g]];

MapToValues[op_]:=(op[if_head]^:=head[op[Values[if]],if//Domain]);
MapToValues/@{Abs,Arg,Re,Im,Conjugate,Exp,Tan,ArcSin,Sec,Sin,Cos,Log,ArcTanh};

Inverse[if_head]^:=Inverse/@if;
Transpose[f_head]^:=Transpose/@f;
Max[f_head]^:=f//Values//Max;
Min[f_head]^:=f//Values//Min;
Norm[f_head]^:=f//Values//Flatten//Norm;

NEqual[f_head,g_head]:=Norm[f-g]<$MachineTolerance;

First[f_head]^:=f//Values//First;
Last[f_head]^:=f//Values//Last;
Dimensions[f_head]^:=f//First//Dimensions;

head/:f_head?MatrixFunQ[[i_,j_]]:=(f//ToMatrixOfFuns)[[i,j]]//ToArrayFun;
head/:f_head?ListFunQ[[i_]]:=(f//ToArrayOfFuns)[[i]]//ToArrayFun;
MeanZero[f_head]:=f-Mean[f];
]


<<`IFun`;
<<`LFun`;
<<`PFun`;



RealLine::usage="The real line Line[{-\[Infinity],\[Infinity]}]";
Orientation;

Begin["Private`"];

DomainMemberQ[f_?FunQ,x_]:=DomainMemberQ[f//Domain,x];
DomainQ[_]:=False;

End[];






Begin["Private`"];


Points[l:{__?FunQ}]:=Points/@l//Flatten;
FinitePoints[l:{__?FunQ}]:=FinitePoints/@l//Flatten;

FinitePoints[d_,n_]:=Points[d,n];
End[];






FunQ[_]:=False;
ListFunQ[_]:=False;
ScalarFunQ[_]:=False;
VectorFunQ[_]:=False;
MatrixFunQ[_]:=False;
ArrayFunQ[_]:=False;
ToMatrixOfFuns::usage="Changes an IFun whose values are a matrix to a matrix of IFuns";
ToMatrixFun::usage="Changes a matrix of IFuns to an IFun whose values are a matrix";

ToArrayOfFuns;
ToArrayFun;
ReImLinePlot;



LinePlot;
ReImLineLogPlot;
ReImLineLogLogPlot;
$FunFormat={ImageSize->Small};



Begin["Private`"];



ListFunQ[f_?FunQ]:=f//Values//First//ListQ;
ScalarFunQ[f_?FunQ]:=f//Values//First//ScalarQ;
VectorFunQ[f_?FunQ]:=f//Values//First//VectorQ;
MatrixFunQ[f_?FunQ]:=f//Values//First//MatrixQ;
ArrayFunQ[f_?FunQ]:=f//Values//First//ArrayQ;




ToMatrixOfFuns[f_]:=MatrixMap[Head[f][#,Domain[f]]&,Values[f]//ToMatrixOfLists];
ToMatrixFun[f_]:=Head[First[Flatten[f[[1]]]]][MatrixMap[Values,f]//ToListOfMatrices,Domain[f[[1,1]]]];

ToArrayOfFuns[f_?MatrixFunQ]:=ToMatrixOfFuns[f];ToArrayOfFuns[f_?VectorFunQ]:=Map[Head[f][#,Domain[f]]&,Values[f]//ToArrayOfLists];
ToArrayOfFuns[f_?ScalarFunQ]:=f;
ToArrayFun[f_]:=Head[Flatten[{f}][[1]]][ArrayMap[Values,f]//ToListOfArrays,Domain[First[Flatten[{f}]]]];

AddIdentityMatrix[l_?FunQ]:=(IdentityMatrix[2]+#)&/@l;
AddIdentityMatrix[l_List]:=AddIdentityMatrix/@l;
SubtractIdentityMatrix[l_?FunQ]:=(#-IdentityMatrix[2])&/@l;
SubtractIdentityMatrix[l_List]:=SubtractIdentityMatrix/@l;


ReImLinePlot[f_?ArrayFunQ,opts___]:=ArrayMap[ReImLinePlot[#,opts]&,f//ToArrayOfFuns]//MatrixForm;


MatrixMap[f_,g_?FunQ]:=MatrixMap[f,g//ToMatrixOfFuns];
ArrayMap[f_,g_?ScalarFunQ]:=f[g];
ArrayMap[f_,g_?FunQ]:=ArrayMap[f,g//ToArrayOfFuns];


InfinityInDomainQ[f_?FunQ]:=InfinityInDomainQ[Domain[f]];
FiniteLength[f_]:=f//FinitePoints//Length;

FiniteRealPoints[if_?FunQ]:=if//FinitePoints//Re;







LinePlot[f_,opts___]:=ListLinePlot[Thread[{FiniteRealPoints[f],FiniteValues[f]}],opts];
ReImLinePlot[f_,opts___]:=ListLinePlot[{Thread[{FiniteRealPoints[f],FiniteValues[f]//Re}],Thread[{FiniteRealPoints[f],FiniteValues[f]//Im}]},opts];
ReImLineLogPlot[f_,opts___]:=ListLineLogPlot[{Thread[{FiniteRealPoints[f],FiniteValues[f]//Re}],Thread[{FiniteRealPoints[f],FiniteValues[f]//Im}]},opts];
ReImLineLogLogPlot[f_,opts___]:=ListLineLogLogPlot[{Thread[{FiniteRealPoints[f],FiniteValues[f]//Re}],Thread[{FiniteRealPoints[f],FiniteValues[f]//Im}]},opts];
LinePlot[f_List,opts___]:=ListLinePlot[Map[Thread[{FiniteRealPoints[#],FiniteValues[#]}]&,f],opts];

LineLogPlot[f_List,opts___]:=ListLineLogPlot[Map[Thread[{FiniteRealPoints[#],FiniteValues[#]}]&,f],opts];



End[];



Begin["Private`"];


DomainPlot[if_?FunQ,opts___]:=DomainPlot[if//Domain,opts];

DomainPlot[l_List,opts___]:=Show[DomainPlot[#,opts]&/@l,PlotRange->All];



NEqual[f:{__?FunQ},g:{__?FunQ}]:=Norm[Norm/@(f-g)]<$MachineTolerance;



FiniteLength[f_]:=f//FinitePoints//Length;



$FunFormat={ImageSize->Small};
End[];




Begin["Private`"];

SetDomain[f_?FunQ,d_]:=Head[f][Values[f],d];

SetDomain[f_List,d_List]:=SetDomain@@#&/@Thread[{f,d}];
End[];



ToArrayOfListOfFuns;
ToListOfArrayFuns;
ToValueList;
FromValueList;
MatrixMultVectorFun;
RightMatrixMultVectorFun;
RightMatrixMultMatrixFun;
MatrixMultMatrixFun;



Begin["Private`"];


ToArrayOfListOfFuns[f_List]:=ToArrayOfLists[ToArrayOfFuns/@f];
ToListOfArrayFuns[f_List]:=(ToArrayFun/@Thread[f]);

ToValueList[GI_List]:=Flatten[ArrayMap[FiniteValues,#]&/@(ToArrayOfFuns/@GI//ToArrayOfLists)];
ToValueList[f_?FunQ]:=ArrayMap[FiniteValues,ToArrayOfFuns[f]]//Flatten;



FromValueList[GI_List?(VectorFunQ[First[#]]&),ls_]:=ZeroAtInfinityFun[#[[2]],Domain[#[[1]]]]&/@
Thread[
{GI,ToListOfArrays/@
Thread[PartitionList[PartitionList[ls,Flatten[ArrayMap[FiniteLength,ToArrayOfListOfFuns[GI]]]],Length/@ToArrayOfLists[ToArrayOfFuns/@GI]]]}];

FromValueList[GI_List?(MatrixFunQ[First[#]]&),ls_]:=ZeroAtInfinityFun[#[[2]],Domain[#[[1]]]]&/@Thread[{GI,ToListOfMatrices/@(PartitionList[Partition[PartitionList[ls,Flatten[ArrayMap[FiniteLength,ToArrayOfListOfFuns[GI]]]],Length[GI]],Dimensions[First[GI]]]//ToListOfMatrices)}];


FromValueList[GI_List?(ScalarFunQ[First[#]]&),ls_]:=ZeroAtInfinityFun[#[[2]],Domain[#[[1]]]]&/@Thread[{GI,PartitionList[ls,FiniteLength/@GI]}];

MatrixMultVectorFun[G_List]:=Join@@(RightJoin@@#&/@MatrixMap[SparseDiagonalMatrix[ToValueList[#]]&,G//ToArrayOfListOfFuns]);

RightMatrixMultVectorFun[G_List]:=MatrixMultVectorFun[Transpose/@G];


RightMatrixMultMatrixFun[G_List]:=BlockDiagonalMatrix[{MatrixMultVectorFun[Transpose/@(G)],MatrixMultVectorFun[Transpose/@(G)]}];

MatrixMultMatrixFun[G_List]:=Join[(RightJoin@@(SparseDiagonalMatrix/@Flatten[Thread[{1,0} MatrixMap[ToValueList,G//ToArrayOfListOfFuns]],1])),
(RightJoin@@(SparseDiagonalMatrix/@Flatten[Reverse/@Thread[{1,0} MatrixMap[ToValueList,G//ToArrayOfListOfFuns]],1])),
(RightJoin@@(SparseDiagonalMatrix/@Flatten[Thread[{1,0}Reverse[ MatrixMap[ToValueList,G//ToArrayOfListOfFuns]]],1])),
(RightJoin@@(SparseDiagonalMatrix/@Flatten[Reverse/@Thread[{1,0}Reverse[ MatrixMap[ToValueList,G//ToArrayOfListOfFuns]]],1]))];

End[];





TransformMatrix;
DerivativeMatrix;
IntegrateMatrix;
FiniteTransformMatrix;
BoundedIntegrateMatrix;
ReduceDimensionMatrix;


Begin["Private`"];



TransformMatrix[f_?FunQ]:=TransformMatrix[f//Domain,f//Length];


FiniteTransformMatrix[d_?DomainQ,n_Integer]:=TransformMatrix[d,n];
FiniteTransformMatrix[f_]:=FiniteTransformMatrix[f//Domain,f//Length];

DerivativeMatrix[1][f_?FunQ]:=DerivativeMatrix[1][f//Domain,f//Length];
DerivativeMatrix[k_Integer][pars__]:=MatrixPower[DerivativeMatrix[1][pars],k];
DerivativeMatrix[d_?DomainQ,n_Integer]:=DerivativeMatrix[1][d,n];
DerivativeMatrix[f_?FunQ]:=DerivativeMatrix[1][f];




IntegrateMatrix[f_]:=IntegrateMatrix[f//Domain,f//Length];


Fun[f_,l_List,n_List]:=Flatten[Fun[f,#[[1]],#[[2]]]&/@Thread[{l,n}]];

Fun[f_,l_List,opts___]:=Flatten[Fun[f,#,opts]&/@l];



ZeroAtInfinityFun[f_?NotListOrPatternQ,d_,opts___]:=Fun[If[InfinityQ[#],0 f[0.],f[#]/.Underflow[]->0]&,d,opts];




IdentityAtInfinityFun[G_?NotListOrPatternQ,pars___]:=Fun[If[InfinityQ[#],If[G[0.]//MatrixQ,IdentityMatrix[Length[G[0.]]],1],G[#]/.Underflow[]->0]&,pars];


FunListDot[f_,g_]:=#[[1]].#[[2]]&/@Thread[{f,g}];


DomainPlot[Curve[cf_,___],opts:OptionsPattern[]]:=Show[ComplexPlot[cf,opts,PlotStyle->{Thick,Blue}],
{Arrowheads[Medium],Blue,Arrow[{cf//Values//Last//{Re[#],Im[#]}&,(cf//Values//Last)+0.01Exp[I Arg[((cf//Values//Last)-(cf//Values)[[-2]])]]//{Re[#],Im[#]}&}]}//Graphics];



Curve[f_,opts___]~NEqual~Curve[g_,opts___]:=If[Length[f]==Length[g],(f-g//Values//Norm)<$MachineTolerance,False];


OneFun[lf_?FunQ]:=Head[lf][1&,lf//Domain,lf//Length];


End[];
EndPackage[];
