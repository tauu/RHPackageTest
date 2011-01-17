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



`Private`PainleveIIPositive["AS"] =`AblowitzSegurPositive`AblowitzSegurPositivePainleveII;
`Private`PainleveIIPositiveD["AS"] =`AblowitzSegurPositive`AblowitzSegurPositivePainleveIID;
Begin["`AblowitzSegurPositive`"];


\[Theta][z_]:=I(4/3 z^3 + z);
x//Clear;
G//Clear;
s//Clear;
G[_,_][_?InfinityQ]:=IdentityMatrix[2];
G[{x_,s_},1][z_]=\!\(\*
TagBox[
RowBox[{"(", "", GridBox[{
{"1", "0"},
{
RowBox[{" ", 
RowBox[{
SuperscriptBox["E", 
RowBox[{"2", " ", 
SuperscriptBox["x", 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"\[Theta]", "[", "z", "]"}]}]], " ", "s"}]}], "1"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}], "", ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);
G[{x_,s_},6][z_]=\!\(\*
TagBox[
RowBox[{"(", "", GridBox[{
{"1", 
RowBox[{
RowBox[{"-", " ", 
SuperscriptBox["E", 
RowBox[{
RowBox[{"-", "2"}], "  ", 
SuperscriptBox["x", 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"\[Theta]", "[", "z", "]"}]}]]}], " ", "s"}]},
{"0", "1"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}], "", ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);
Gx[{x_,s_},1][z_]=\!\(\*
TagBox[
RowBox[{"(", "", GridBox[{
{"0", "0"},
{
RowBox[{" ", 
RowBox[{"3", " ", 
SuperscriptBox["x", 
RowBox[{"1", "/", "2"}]], " ", 
RowBox[{"\[Theta]", "[", "z", "]"}], " ", 
SuperscriptBox["E", 
RowBox[{"2", " ", 
SuperscriptBox["x", 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"\[Theta]", "[", "z", "]"}]}]], "  ", "s"}]}], "0"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}], "", ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);
Gx[{x_,s_},6][z_]=\!\(\*
TagBox[
RowBox[{"(", "", GridBox[{
{"0", 
RowBox[{"3", " ", 
SuperscriptBox["x", 
RowBox[{"1", "/", "2"}]], 
RowBox[{"\[Theta]", "[", "z", "]"}], " ", 
SuperscriptBox["E", 
RowBox[{
RowBox[{"-", "2"}], "  ", 
SuperscriptBox["x", 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"\[Theta]", "[", "z", "]"}]}]], "  ", "s"}]},
{"0", "0"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}], "", ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);


M=6.;Cdefs[n_]:={Function[x,({
 {.5 I, -.5 I},
 {x, -x}
})],({
 {Line[{-M,M}], n}
})};
Gl[{x_,s_}]:=({
 {G[{x,s},1], G[{x,s},6]}
});
Glx[{x_,s_}]:=({
 {Gx[{x,s},1], Gx[{x,s},6]}
});
n=70;slvr//Clear;slvr:=slvr=ScaledRHSolver[Cdefs[n]];


AblowitzSegurPositivePainleveII[s_,x_]:=-(I/\[Pi])Sqrt[x]Total[DomainIntegrate/@slvr[x,Gl[{x,-s}]]][[1,2]];
AblowitzSegurPositivePainleveIID[s_,x_]:=Module[{slvrx,Cm,Cp,Ul},
slvrx=slvr[x];
Cm=ScaledCauchyOperator[-1,slvrx];
Cp=ScaledCauchyOperator[1,slvrx];
Ul=slvrx[Gl[{x,-s}]];
I(-(1/(\[Pi] 2)) x^(-1/2) Total[DomainIntegrate/@Ul][[1,2]] -1/\[Pi] Sqrt[x]((Cm[Ul]~FunListDot~ConstructCurve[Cdefs[n],Glx[{x,-s}],x]~FunListDot~(Inverse/@Cp[Ul])//DomainIntegrate))[[1,2]])
]


End[]
