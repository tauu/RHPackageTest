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


FreePlus;
FreeTimes;
FreeCompress;


Begin["Private`"];
DiskPoints[n_]:=Table[Points[Circle[0,r],n],{r,1/(n-1),1.,1/(n-1)}]//Flatten;
SlitPlanePoints[lf_LFun,n_]:=Select[MapFromCircle[lf,Join[DiskPoints[n],1/DiskPoints[n]]],FiniteQ];
SlitPlanePoints[if_IFun,n_]:=MapFromInterval[if,Table[Points[Circle[0,r],n],{r,1/(n-1),1.,1/(n-1)}]//Flatten//CircleToInterval];
SlitPlanePoints[sf_SingFun,n_]:=SlitPlanePoints[sf//First,n];

FreePlus[sfA_,sfB_,rng_:{-80,80},n_:10]/;Head[sfA]==LFun||Head[sfB]==LFun:=Quiet[
Module[{GAB,GABD,GABDD,xia,xib,Apts,Bpts,sIptsA,sIptsB,sIpts,sgpts,gpts,ret,AB,a,b},
GAB[y_]:=StieljesInverseFunction[sfA,y]+StieljesInverseFunction[sfB,y]-1/y;
Apts=SlitPlanePoints[sfA,n];
Bpts=SlitPlanePoints[sfB,n];



(sIptsA=Stieljes[sfA,Apts]);(sIptsB=Stieljes[sfB,Bpts]);(sIpts=Union[sIptsA,sIptsB]);


{sgpts,gpts}=(((ret=GAB[#];
If[Sign[Im[#]]==Sign[Im[ret]]||!NumberQ[ret],Null,{#,ret}])&/@sIpts)/.Null->Sequence[])//Transpose;
AB=LFun[ShiftList[ LeastSquares[CauchyInverseBasis[RealLine,Span@@rng,#]&/@gpts,sgpts],rng[[2]]+1],RealLine];
-1/(2 \[Pi])HilbertInverse[AB]//Re
],
{First::first,Thread::tdlen}]

FreePlus[sfA_,sfB_,m_:50,n_:30]:=Quiet[
Module[{GAB,GABD,GABDD,xia,xib,Apts,Bpts,sIptsA,sIptsB,sIpts,sgpts,gpts,ret,AB,a,b},
GAB[y_]:=StieljesInverseFunction[sfA,y]+StieljesInverseFunction[sfB,y]-1/y;
GABD[y_]:=StieljesInverseFunctionD[sfA,y]+StieljesInverseFunctionD[sfB,y]+1/y^2//Re;
GABDD[y_]:=StieljesInverseFunctionD[2][sfA,y]+StieljesInverseFunctionD[2][sfB,y]-2/y^3//Re;
{xia,xib}={NewtonMethod[GABD,GABDD,-.1],NewtonMethod[GABD,GABDD,.1]};
{a,b}=GAB/@{xia,xib}//Re;
Apts=SlitPlanePoints[sfA,n];
Bpts=SlitPlanePoints[sfB,n];


(sIptsA=Stieljes[sfA,Apts]);(sIptsB=Stieljes[sfB,Bpts]);(sIpts=Union[sIptsA,sIptsB]);


{sgpts,gpts}=(((ret=GAB[#];
If[Sign[Im[#]]==Sign[Im[ret]]||!NumberQ[ret]||NZeroQ[Im[#]]&&Re[#]>xib||NZeroQ[Im[#]]&&Re[#]<xia,Null,{#,ret}])&/@sIpts)/.Null->Sequence[])//Transpose;
AB=SingFun[IFun[LeastSquares[Table[BoundedCauchyInverseBasis[Line[{a,b}],k,gpts],{k,m}]//Transpose,sgpts]//InverseDCT,Line[{a,b}]],{0,0}];
-1/(2 \[Pi])HilbertInverse[AB]//Re
],
{First::first,Thread::tdlen}];


FreeCompress[sfA_SingFun,\[Alpha]_,m_:50,n_:30]:=Quiet[
Module[{GAB,GABD,GABDD,xia,xib,Apts,Bpts,sIptsA,sIptsB,sIpts,sgpts,gpts,ret,AB,a,b},
GAB[y_]:=StieljesInverseFunction[sfA,\[Alpha] y]/\[Alpha]+(1-1/(\[Alpha]^2) )/y;
GABD[y_]:=StieljesInverseFunctionD[sfA,\[Alpha] y]-(1-1/(\[Alpha]^2) )/y^2//Re;
GABDD[y_]:=\[Alpha] StieljesInverseFunctionD[2][sfA,\[Alpha] y]+2 (1-1/(\[Alpha]^2) )/y^3//Re;
{xia,xib}={NewtonMethod[GABD,GABDD,-.1],NewtonMethod[GABD,GABDD,.1]};
{a,b}=GAB/@{xia,xib}//Re;
Apts=SlitPlanePoints[sfA,n];


(sIpts=Stieljes[sfA,Apts]);


{sgpts,gpts}=(((ret=GAB[#];
If[Sign[Im[#]]==Sign[Im[ret]]||!NumberQ[ret]||NZeroQ[Im[#]]&&Re[#]>xib||NZeroQ[Im[#]]&&Re[#]<xia,Null,{#,ret}])&/@sIpts)/.Null->Sequence[])//Transpose;
AB=SingFun[IFun[LeastSquares[Table[BoundedCauchyInverseBasis[Line[{a,b}],k,gpts],{k,m}]//Transpose,sgpts]//InverseDCT,Line[{a,b}]],{0,0}];
-1/(2 \[Pi])HilbertInverse[AB]//Re
],
{First::first,Thread::tdlen}];

FreeCompress[sfA_LFun,\[Alpha]_,rng_:{-80,80},n_:10]:=Quiet[
Module[{GAB,GABD,GABDD,xia,xib,Apts,Bpts,sIptsA,sIptsB,sIpts,sgpts,gpts,ret,AB,a,b},
GAB[y_]:=StieljesInverseFunction[sfA,\[Alpha] y]/\[Alpha]+(1-1/(\[Alpha]^2) )/y;

Apts=SlitPlanePoints[sfA,n];


(sIpts=Stieljes[sfA,Apts]);


{sgpts,gpts}=(((ret=GAB[#];
If[Sign[Im[#]]==Sign[Im[ret]]||!NumberQ[ret]||NZeroQ[Im[#]]&&Re[#]>xib||NZeroQ[Im[#]]&&Re[#]<xia,Null,{#,ret}])&/@sIpts)/.Null->Sequence[])//Transpose;
AB=LFun[ShiftList[ LeastSquares[CauchyInverseBasis[RealLine,Span@@rng,#]&/@gpts,sgpts],rng[[2]]+1],RealLine];
-1/(2 \[Pi])HilbertInverse[AB]//Re
],
{First::first,Thread::tdlen}];


End[];
EndPackage[];