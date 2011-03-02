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


IFun::usage="IFun[\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\),d,n] constructs an \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\)-th order Chebyshev approximation of \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\) over the domain d, which is either a Line or Arc\!\(\*
StyleBox[\".\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)IFun\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)d\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)chooses n adaptively. \!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)IFun\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)d\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)InterpolationPrecision\[Rule]tol] chooses n adaptively, so that the last two Chebyshev coefficients are less than tol. If f is a constructed IFun, then f[x] evaluates the approximation at the point x. Operations that can be applied to an IFun include standard mathematical functions (Abs, Sin, Exp, etc.) and operators Integrate (returning the indefinite integral) and Derivative[k].";

Options[IFun]:={InterpolationPrecision->$MachineTolerance};


DomainPlot::usage=
"DomainPlot[ifun] plots the domain of ifun. DomainPlot[list] plots the union of the domains of a list of IFuns";

Options[DomainPlot]=Options[Graphics];


Arc::usage=
"Arc[z0,r,{t0,t1}] represents the arc centred at z0 of radius r from argument t0 to t1.";
Ellipse::usage=
"Ellipse[{a_,b_},r] represents the Bernstein ellipse arond the interval (a,b).";

DomainIntegrate::usage=
"DomainIntegrate[ifun] returns the definite integral of the function ifun over its domain.";


DCT::usage=
"DCT[ifun] returns the Chebyshev coefficients of an IFun.";
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

LeftEndpoint::usage=
"LeftEndpoint[d] returns the left endpoint of the domain d. If d is an IFun, then it is equivalent to LeftEndpoint[Domain[d]]." ;
RightEndpoint::usage=
"RightEndpoint[d] returns the right endpoint of the domain d. If d is an IFun, then it is equivalent to RightEndpoint[Domain[d]]." ;

Stretch::usage=
"Stretch is a possible parameter for Line used in determining the conformal map.";

Centre::usage=
"Centre is a possible parameter for Line used in determining the conformal map.";

MapToInterval::usage=
"MapToInterval[d,x] maps the point x via the conformal map that maps the domain d to the unit interval. If d is an IFun, then it is equivalent to MapToInterval[Domain[d],x].";
MapToIntervalD::usage=
"MapToIntervalD[d,x] is the derivative of MapToInterval[d,x].";
MapFromInterval::usage=
"MapFromInterval[d,x] maps the point x via the conformal map that maps the unit interval to the domain d. If d is an IFun, then it is equivalent to MapFromInterval[Domain[d],x].";
MapFromIntervalD::usage=
"MapFromIntervalD[d,x] is the derivative of MapFromInterval[d,x].";

ComplexMapToInterval::usage=
"MapToInterval[d,x] maps the point x via the conformal map that maps the domain d to the unit interval. If d is an IFun, then it is equivalent to MapToInterval[Domain[d],x].";
ComplexMapToIntervalD::usage=
"MapToIntervalD[d,x] is the derivative of MapToInterval[d,x].";
ComplexMapFromInterval::usage=
"MapFromInterval[d,x] maps the point x via the conformal map that maps the unit interval to the domain d. If d is an IFun, then it is equivalent to MapFromInterval[Domain[d],x].";
ComplexMapFromIntervalD::usage=
"MapFromIntervalD[d,x] is the derivative of MapFromInterval[d,x].";

UnitInterval::usage=
"The unit interval Line[{-1.,1.}].";

SetDomain::usage="SetDomain[f,d] changes the domain of the Fun (or List of Funs) f to d";

ToUnitInterval::usage=
"ToUnitInterval[ifun] maps ifun to an IFun defined over the unit interval.";

DomainQ::usage=
"Test whether something is a domain (Line, Arc, Circle, Curve, etc).";

IntervalDomainQ::usage=
"Test whether something is a domain mapped from the unit interval.";

BoundedDomainQ::usage="Test whether a domain is bounded.";

ZeroAtInfinityIFun::usage="Constructs a Fun with the default value of zero at infinity";
IdentityAtInfinityIFun::usage="Constructs a Fun with the default value of the identity matrix at infinity";

DCTPlot::usage="LogPlots the norm of the Chebyshev coefficients of an IFun";

ComplexRoots::usage="Returns all roots";

Curve::usage="Turns an IFun into a domain";


SelectWithPoint::usage="Select from a list of Funs the Fun which contains a point";
Endpoints::usage="Returns all endpoints of a list of funs";
FiniteEndpoints::usage="Returns all finite endpoints of a list of funs";

LegendreTransform::usage="Computes Legendre Coefficients";
EvaluateMatrix::usage="Matrix for evaluating at a point";


BoundedIntegrate::usage="BoundedIntegrate[lf] integrates an LFun with its -1 coefficient removed";

OneFun;

AddIdentityMatrix::usage=
"Adds IdentityMatrix[2] to a list of ifuns.";

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


ToChebyshevUSeries;
ToChebyshevTSeries;


Begin["Private`"];

ToChebyshevUSeries[ts:{_}]:=ts;
ToChebyshevUSeries[ts_]:=DoubleFirst[ts]/ 2-PadRight[(ts//Rest//Rest)/ 2,Length[ts]];
ToChebyshevTSeries[us:{_}]:=us;
ToChebyshevTSeries[us:{_,_}]:={1,2}.us;
ToChebyshevTSeries[us_List]:=Module[{ds,k},
ds=ZeroVector[Length[us]];
ds[[-1]]=2 us[[-1]];
ds[[-2]]=2 us[[-2]];
Do[
ds[[k]]=2 us[[k]]+ds[[k+2]];
,{k,-3,-Length[us]+1,-1}];
ds[[1]]=us[[1]]+ds[[3]]/2;
ds];
End[];



ChebyshevPoints;
ChebyshevLobattoPoints;
NChebyshevPoints;
NChebyshevLobattoPoints;
InverseDCT;



Begin["Private`"];


ChebyshevPoints[0,a_:-1,b_:1]:={};
ChebyshevPoints[n_,a_:-1,b_:1]:=(b+a)/2+(b-a)/2 Cos[(2 Range[n,1,-1] -1)/(2 n) Pi];
ChebyshevLobattoPoints[n_,a_:-1,b_:1]:=(b+a)/2+(b-a)/2 Cos[Pi Range[n-1,0,-1]/(n-1)];
NChebyshevPoints[v__]:=N[ChebyshevPoints[v]];
NChebyshevLobattoPoints[v__]:=N[ChebyshevLobattoPoints[v]];



DCT[f:{__?ScalarQ}]:=AlternatingVector[Length[f]] HalfFirstAndLast[FourierDCT[f,1]Sqrt[2/(Length[f]-1)]];
InverseDCT[f:{__?ScalarQ}]:= (Length[f]-1)/2 AlternatingVector[Length[f]] DoubleFirstAndLast[DCT[DoubleFirstAndLast[f]]]//Reverse;
DCT[f:{__?VectorQ}]:=DCT/@ToArrayOfLists[f]//ToListOfArrays;
DCT[f:{__?MatrixQ}]:=MatrixMap[DCT,ToArrayOfLists[f]]//ToListOfArrays;
InverseDCT[f:{__?VectorQ}]:=InverseDCT/@ToArrayOfLists[f]//ToListOfArrays;
InverseDCT[f:{__?MatrixQ}]:=MatrixMap[InverseDCT,ToArrayOfLists[f]]//ToListOfArrays;

ChebyshevLobattoBarycentricInterpolation[f_List,x_]:=Module[{w,j,p,k,n},
k=Length[f];
n=NChebyshevLobattoPoints[k];
w[1]=1/2;
w[j_]=(-1)^(j+1);
w[k]=(-1)^(k+1) 1/2;
If[MemberQ[n,N[Chop[x,$MachineEpsilon]]],f[[Position[n,N[Chop[x,$MachineEpsilon]]][[1,1]]]],
\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(k\)]\(w[j]/\((x - n[[j]])\) f[[j]]\)\)/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(k\)]\(w[j]/\((x - n[[j]])\)\)\)]
];
End[];


CircleToInterval::usage="CircleToInterval[z] conformally maps the unit circle to the unit interval.";
IntervalToTopCircle::usage="IntervalToTopCircle[x] maps the unit interval to the top half of the circle.";
IntervalToBottomCircle::usage="IntervalToBottomCircle[x] maps the unit interval to the bottom half of the circle.";
IntervalToOuterCircle;
IntervalToInnerCircle;
IntervalToInnerCircleD;
IntervalToOuterCircleD;
IntervalToTopCircleD;
IntervalToBottomCircleD;

IntervalToRealLine;
RealLineToInterval;
IntervalToPositivePeriodicInterval;
IntervalToNegativePeriodicInterval;
IntervalToHalfLine;
HalfLineToInterval;

InfinityInDomainQ::usage="InfinityInDomainQ[d] tests if a domain is unbounded.";
RightEndpointInfinityQ::usage="RightEndpointInfinityQ[d] tests if a domain's right endpoint is unbounded.";
LeftEndpointInfinityQ::usage="LeftEndpointInfinityQ[d] tests if a domain's left endpoint is unbounded.";
ReverseOrientation::usage="ReverseOrientation[ifun] gives ifun with the orientation reversed.";

LeftContourArg::usage="LeftContourArg[d] gives the angle in the complex plane of the left endpoint of a domain d.";
RightContourArg::usage="RightContourArg[d] gives the angle in the complex plane of the right endpoint of a domain d.";

DomainMemberQ::usage="DomainMemberQ[d,z] tests whether z is numerically on the domain d.";

UnitIntervalFunQ::usage="Tests whether an object is an IFun whose domain is the unit interval.";
IntervalFunQ::usage="Tests whether an object is an IFun whose domain is an interval.";


MapToIntervalSeriesAtInfinity::usage="Gives the coefficient of the asymptotic series at \[Infinity].";

SetAttributes[MapToInterval,Listable];
SetAttributes[MapFromInterval,Listable];

SetAttributes[ComplexMapToInterval,Listable];
SetAttributes[ComplexMapFromInterval,Listable];


SetAttributes[MapToIntervalD,Listable];
SetAttributes[MapFromIntervalD,Listable];

SetAttributes[ComplexMapToIntervalD,Listable];
SetAttributes[ComplexMapFromIntervalD,Listable];




Begin["Private`"];



Unprotect[Line];
Line/:Line[{a_,b_}]+c_:=Line[{a,b}+c];
Line/:c_ Line[{a_,b_}]:=Line[c {a,b}];
Protect[Line];



CircleToInterval[z_]:=1/2 (z+1/z);
SetAttributes[IntervalToTopCircle,Listable];
IntervalToTopCircle[x_]:=x+I Sqrt[1-x]Sqrt[1+x];
SetAttributes[IntervalToBottomCircle,Listable];
IntervalToBottomCircle[x_]:=x-I Sqrt[1-x]Sqrt[1+x];
SetAttributes[IntervalToOuterCircle,Listable];
IntervalToOuterCircle[t_]:=t+Sqrt[t-1]Sqrt[t+1];
SetAttributes[IntervalToInnerCircle,Listable];
IntervalToInnerCircle[t_]:=t-Sqrt[t-1]Sqrt[t+1];
IntervalToInnerCircle[t_?InfinityQ]:=0;

IntervalToInnerCircleD[x_]:=1-x/(Sqrt[-1+x] Sqrt[1+x]);
IntervalToOuterCircleD[x_]:=1+x/(Sqrt[-1+x] Sqrt[1+x]);
IntervalToTopCircleD[x_]:=1-(I x)/Sqrt[1-x^2];
IntervalToBottomCircleD[x_]:=1-(I x)/Sqrt[1-x^2];

IntervalToRealLine[x_]:=x/Sqrt[1-x^2];
RealLineToInterval[u_]:=u/Sqrt[1+u^2];



InfinityInDomainQ[_]:=False;
InfinityInDomainQ[Line[{_?InfinityQ,_},___]]:=True;
InfinityInDomainQ[Line[{_,_?InfinityQ},___]]:=True;

RightEndpointInfinityQ[_]:=False;
RightEndpointInfinityQ[Line[{_,_?InfinityQ},___]]:=True;
LeftEndpointInfinityQ[_]:=False;
LeftEndpointInfinityQ[Line[{_?InfinityQ,_},___]]:=True;

LeftEndpoint[Line[{a_,b_},___]]:=a;
RightEndpoint[Line[{a_,b_},___]]:=b;

ReverseOrientation[Line[{a_,b_},l___]]:=Line[{b,a},l];

LeftContourArg[Line[{a_,b_},___]]:=(b-a)//Arg;
RightContourArg[Line[{a_,b_},___]]:=(a-b)//Arg;

IntervalDomainQ[_]:=False;
IntervalDomainQ[_Line]:=True;

BoundedDomainQ[_]:=False;
BoundedDomainQ[_?(!LeftEndpointInfinityQ[#]&&!RightEndpointInfinityQ[#]&)]:=True;


DomainMemberQ[d_?IntervalDomainQ,x_]:=MapToInterval[d,x]//(NumberQ[#]&&Abs[Im[#]]<10 $MachineTolerance&&Abs[#]<=1.+10 $MachineTolerance&);


SetAttributes[IntervalToHalfLine,Listable];
IntervalToHalfLine[_?(#~NEqual~1.&)]:=\[Infinity] ;
IntervalToHalfLine[_?InfinityQ]:=-1.;
IntervalToHalfLine[x_]:=(x+1)/(1-x) ;

SetAttributes[HalfLineToInterval,Listable];
HalfLineToInterval[y_?InfinityQ]:=1.;
HalfLineToInterval[y_]/;y==-1:=\[Infinity] ;
HalfLineToInterval[y_]:=(-1+y)/(1+y) ;



MapToInterval[Line[{a_,b_?InfinityQ},Stretch->L_],z_]:=HalfLineToInterval[Exp[-I Arg[b]](z-a)/L];
MapToIntervalD[Line[{a_,b_?InfinityQ},Stretch->L_],z_]:=(2 E^(I Arg[b]) L)/(-a+E^(I Arg[b]) L+z)^2;
MapFromInterval[Line[{a_,b_?InfinityQ},Stretch->L_],x_]:=L Exp[I Arg[b]] IntervalToHalfLine[x]+a;

MapFromIntervalD[Line[{a_,b_?InfinityQ},Stretch->L_],1.]:=0.;
MapFromIntervalD[Line[{a_,b_?InfinityQ},Stretch->L_],x_]:=(2 E^(I Arg[b]) L)/(-1+x)^2;

MapToInterval[Line[{a_?InfinityQ,b_},Stretch->L_],z_]:=-MapToInterval[Line[{b,a},Stretch->L],z];
MapToIntervalD[Line[{a_?InfinityQ,b_},Stretch->L_],z_]:=-MapToIntervalD[Line[{b,a},Stretch->L],z];
MapFromInterval[Line[{a_?InfinityQ,b_},Stretch->L_],x_]:=MapFromInterval[Line[{b,a},Stretch->L],-x];
MapFromIntervalD[Line[{a_?InfinityQ,b_},Stretch->L_],x_]:=-MapFromIntervalD[Line[{b,a},Stretch->L],-x];



MapToInterval[Line[{a_,b_?InfinityQ}],z_]:=MapToInterval[Line[{a,b},Stretch->1.],z];
MapToIntervalD[Line[{a_,b_?InfinityQ}],z_]:=MapToIntervalD[Line[{a,b},Stretch->1.],z];
MapFromInterval[Line[{a_,b_?InfinityQ}],x_]:=MapFromInterval[Line[{a,b},Stretch->1.],x];
MapFromIntervalD[Line[{a_,b_?InfinityQ}],1.]:=0.;
MapFromIntervalD[Line[{a_,b_?InfinityQ}],x_]:=MapFromIntervalD[Line[{a,b},Stretch->1.],x];

MapToInterval[Line[{a_?InfinityQ,b_}],z_]:=MapToInterval[Line[{a,b},Stretch->1.],z];
MapToIntervalD[Line[{a_?InfinityQ,b_}],z_]:=MapToIntervalD[Line[{a,b},Stretch->1.],z];
MapFromInterval[Line[{a_?InfinityQ,b_}],x_]:=MapFromInterval[Line[{a,b},Stretch->1.],x];
MapFromIntervalD[Line[{a_?InfinityQ,b_}],-1.]:=0.;
MapFromIntervalD[Line[{a_?InfinityQ,b_}],x_]:=MapFromIntervalD[Line[{a,b},Stretch->1.],x];


MapToInterval[Line[{a_,b_}],z_]:=(a+b-2z)/(a-b);
MapFromInterval[Line[{a_,b_}],x_]:=(b+a)/2+(b-a)/2 x;
MapToIntervalD[Line[{a_,b_}],x_]:=-(2/(a-b)) ;
MapFromIntervalD[Line[{a_,b_}],x_]:=(b-a)/2  ;

MapToIntervalSeriesAtInfinity[Line[{a_,b_}],1]:=2/(b-a);

MapToInterval[z_]:=z;
MapFromInterval[z_]:=z;



UnitInterval=Line[{-1,1}];
UnitIntervalFunQ[f_IFun]:=N[Domain[f]]===N[UnitInterval];
UnitIntervalFunQ[_]:=False;
IntervalFunQ[f_IFun]:=MatchQ[Domain[f],Line[{_?FiniteQ,_?FiniteQ}]];
IntervalFunQ[_]:=False;




MapToInterval[Line[{a_,b_},Stretch->_?(1.~NEqual~#&)],_?InfinityQ]:=\[Infinity];
MapFromInterval[Line[{a_,b_},Stretch->_?(1.~NEqual~#&)],_?InfinityQ]:=\[Infinity];
MapFromInterval[Line[{a_,b_},Stretch->L_],_?InfinityQ]:=(-a+b L)/(-1+L) ;
MapToInterval[Line[{a_,b_},Stretch->L_],_?InfinityQ]:=(1+L)/(1-L) ;
MapFromInterval[Line[{a_,b_},Stretch->L_],_?InfinityQ]:=(-a+b L)/(-1+L) ;
MapToInterval[Line[{a_,b_},Stretch->L_],z_]:=(- a- b L+(1 +L) z)/(- a+b L+ z-L z) ;
MapFromInterval[Line[{a_,b_},Stretch->L_],z_]:=(b L (-1- z)+a (-1+ z))/(-1- L+z- L z) ;
MapToIntervalD[Line[{a_,b_},Stretch->L_],z_]:=-((2 (a L-b L))/(a-b L-z+L z)^2) ;
MapFromIntervalD[Line[{a_,b_},Stretch->L_],z_]:=(2 (-a L+b L))/(1+L-z+L z)^2 ;


MapFromInterval[Arc[z0_,r_,{t0_,t1_}],x_]:=(-E^(1/2 I (t0+2 t1)) r (1+x)-E^((I t0)/2) (1+x) z0+E^((I t1)/2) (-1+x) (E^(I t0) r+z0))/(E^((I t1)/2) (-1+x)-E^((I t0)/2) (1+x));
MapFromInterval[Arc[z0_,r_,{t0_,t1_}],_?InfinityQ]:=-E^(1/2 I (t0+t1)) r+z0;
MapToInterval[Arc[z0_,r_,{t0_,t1_}],z_]/;((-E^(1/2 I (t0+t1)) r+z0)~NEqual~z):=\[Infinity];
MapToInterval[Arc[z0_,r_,{t0_,t1_}],z_]:=-(((E^((I t0)/2)+E^((I t1)/2)) (E^((I t0)/2+(I t1)/2) r-z+z0))/((-E^(((I t0)/2))+E^((I t1)/2)) (E^((I t0)/2+(I t1)/2) r+z-z0)));
MapToInterval[Arc[z0_,r_,{t0_,t1_}],_?InfinityQ]:=-((E^((I t0)/2)+E^((I t1)/2))/(E^((I t0)/2)-E^((I t1)/2)));

MapFromIntervalD[Arc[z0_,r_,{t0_,t1_}],x_]:=(2 E^(1/2 I (t0+t1)) (-E^(I t0)+E^(I t1)) r)/(E^((I t1)/2) (-1+x)-E^((I t0)/2) (1+x))^2;
MapToIntervalD[Arc[z0_,r_,{t0_,t1_}],z_]:=(2 E^((I t0)/2) (E^(I t1)+E^(1/2 I (t0+t1))) r)/((-E^(((I t0)/2))+E^((I t1)/2)) (E^(1/2 I (t0+t1)) r+z-z0)^2);


LeftEndpoint[Arc[z0_,r_,{t0_,t1_}]]:=z0+r Exp[I t0]//N;
RightEndpoint[Arc[z0_,r_,{t0_,t1_}]]:=z0+r Exp[I t1]//N;

ReverseOrientation[Arc[z0_,r_,{t0_,t1_}]]:=Arc[z0,r,{t1,t0}];

LeftContourArg[Arc[z0_,r_,{t0_,t1_}]]:=t0+Sign[t1-t0] \[Pi]/2//N;
RightContourArg[Arc[z0_,r_,{t0_,t1_}]]:=t1-Sign[t1-t0] \[Pi]/2//N;

IntervalDomainQ[_Arc]:=True;


Arc[{a_,b_,c_}]:=(Det[({
 {Abs[a]^2, Im[a], 1},
 {Abs[b]^2, Im[b], 1},
 {Abs[c]^2, Im[c], 1}
})]-I Det[({
 {Abs[a]^2, Re[a], 1},
 {Abs[b]^2, Re[b], 1},
 {Abs[c]^2, Re[c], 1}
})])/(2 Det[({
 {Re[a], Im[a], 1},
 {Re[b], Im[b], 1},
 {Re[c], Im[c], 1}
})])//Function[x0,Arc[x0,Abs[a-x0],{Arg[a-x0],Arg[c-x0]}]];



LeftEndpoint[comp_]:=MapFromInterval[comp,-1];
RightEndpoint[comp_]:=MapFromInterval[comp,1];
LeftContourArg[comp_]:=-Arg[MapToIntervalD[comp,LeftEndpoint[comp]]];
RightContourArg[comp_]:=-Arg[-MapToIntervalD[comp,RightEndpoint[comp]]];
End[];


MapToIntervalDomainD;
MapToIntervalDDomainD;
MapFromIntervalDomainD;
BaryDomainD;
MapToIntervalSeriesAtInfinityD;
PointsD;
ValuesDomainD;
ToValueListD;



Begin["Private`"];



MapToIntervalDomainD[1,0][Line[{a_,b_}],z_]:=-((2 (b-z))/(a-b)^2);
MapToIntervalDomainD[0,1][Line[{a_,b_}],z_]:=(2 (a-z))/(a-b)^2;
MapToIntervalDDomainD[1,0][Line[{a_,b_}],z_]:=2/(a-b)^2;
MapToIntervalDDomainD[0,1][Line[{a_,b_}],z_]:=-(2/(a-b)^2);

MapFromIntervalDomainD[1,0][Line[{a_,b_}],z_]:=1/2-z/2;
MapFromIntervalDomainD[0,1][Line[{a_,b_}],z_]:=1/2+z/2;
MapToIntervalDomainD[spc__][d_IFun,z_]:=MapToIntervalDomainD[spc][d//Domain,z];
MapToIntervalDDomainD[spc__][d_IFun,z_]:=MapToIntervalDDomainD[spc][d//Domain,z];
MapFromIntervalDomainD[spc__][d_IFun,z_]:=MapFromIntervalDomainD[spc][d//Domain,z];



BaryDomainD[spc__][f_,z_]:=MapToIntervalDomainD[spc][f,z] ToUnitInterval[f]'[MapToInterval[f,z]];

MapToIntervalSeriesAtInfinityD[1,0][Line[{a_,b_}],1]:=2/(-a+b)^2;
MapToIntervalSeriesAtInfinityD[0,1][Line[{a_,b_}],1]:=-(2/(-a+b)^2);
MapToIntervalSeriesAtInfinityD[spc__][f_IFun,s_]:=MapToIntervalSeriesAtInfinityD[spc][f//Domain,s];


PointsD[spc__][d_,n_]:=MapFromIntervalDomainD[spc][d,Points[UnitInterval,n]];
PointsD[spc__][d_IFun]:=PointsD[spc][d//Domain,d//Length];


ValuesDomainD[spc__][f_IFun]:=Values[f']MapFromIntervalDomainD[spc][f,Points[UnitInterval,f//Length]];

ToValueListD[spca__][f_List]:=Join@@(If[#[[2]]=={0,0},ZeroVector[Length[#[[1]]]],ValuesDomainD[Sequence@@#[[2]]][#[[1]]]]&/@Thread[{f,{spca}}]);
End[];







FinitePoints;

Begin["Private`"];

Points[d_?IntervalDomainQ,n_]:=MapFromInterval[d,NChebyshevLobattoPoints[n]];

End[];



DomainPlot[Line[{a_,b_?InfinityQ},___],opts___]:=Graphics[{Thick,Blue,Arrowheads[Large],PointSize[Large],Point[a//{Re[#],Im[#]}&],Arrow[{a//{Re[#],Im[#]}&,a+2 Exp[I Arg[b]]//{Re[#],Im[#]}&}]},opts,Axes->True];
DomainPlot[Line[{a_?InfinityQ,b_},___],opts___]:=Graphics[{Thick,Blue,Arrowheads[Large],PointSize[Large],Point[b//{Re[#],Im[#]}&],Arrow[{b+2 Exp[I Arg[a]]//{Re[#],Im[#]}&,b//{Re[#],Im[#]}&}]},opts,Axes->True];
DomainPlot[Line[{a_,b_},___],opts___]:=Graphics[{Thick,Blue,PointSize[Large],Arrowheads[Medium],Point[a//{Re[#],Im[#]}&],
Point[b//{Re[#],Im[#]}&],Arrow[{a//{Re[#],Im[#]}&,b//{Re[#],Im[#]}&}]},opts,Axes->True];
DomainPlot[Arc[z0_,r_,{t0_,t1_}],opts___]:=
Graphics[{Thick,Blue,PointSize[Large],Arrowheads[Medium],
Point[z0 + r Exp[I t0]//{Re[#],Im[#]}& ],
Point[z0 + r Exp[I t1]//{Re[#],Im[#]}& ],
Arrow[{z0+r Exp[I (t1-0.0001 Sign[t1-t0])]//{Re[#],Im[#]}&,z0 + r Exp[I t1]//{Re[#],Im[#]}&}],
Circle[{Re[#],Im[#]}&[z0],r,{t0,t1}//N//Sort]},opts,Axes->True];




MeanZero::usage="Subtracts out the mean of an IFun.";
ZeroAtZero;
ZeroAtRight;
ZeroAtLeft;

FiniteValues;
FiniteLength;

FastPlus;
FastTimes;
SetLength;

Begin["Private`"];

IFun[l_List,d_][z_]:=ChebyshevLobattoBarycentricInterpolation[l,MapToInterval[d,z]];
IFun[f_?NotListOrPatternQ,d_,n_Integer]:=IFun[f/@Points[d,n],d];


FunQ[_IFun]:=True;

Values[IFun[l_List,_]]:=l;
Domain[IFun[_List,d_]]:=d;

Length[if_IFun]^:=if//Values//Length;
Points[if_IFun]:=Points[if//Domain,if//Length];


DCT[f_IFun?ArrayFunQ]:=ArrayMap[DCT,f]//ToListOfArrays;
DCT[if_IFun]:=if//Values//DCT;

IFun/:Map[f_,g_IFun]:=IFun[f/@Values[g],Domain[g]];

FastPlus[f__IFun]:=IFun[Plus@@(Values/@{f}),Domain[{f}[[1]]]];
FastTimes[f__IFun]:=IFun[Times@@(Values/@{f}),Domain[{f}[[1]]]];



f_IFun+g_IFun^:=f~FastPlus~g;
Times[f_IFun,g_IFun]^:=f~FastTimes~g;
IFun/:f_?ConstantQ+g_IFun:=IFun[f+Values[g],g//Domain];
IFun/:g_IFun+f_?ConstantQ:=IFun[Values[g]+f,g//Domain];
IFun/:Times[f_?ConstantQ,g_IFun]:=IFun[f Values[g],g//Domain];
IFun/:Times[g_IFun,f_?ConstantQ]:=IFun[Values[g]f,g//Domain];
IFun/:f_IFun^c_?ConstantQ:=IFun[Values[f]^c,f//Domain];
IFun/:c_?ConstantQ^f_IFun:=IFun[c^Values[f],f//Domain];
Dot[f_IFun?ArrayFunQ,g_IFun?ArrayFunQ]^:=ToArrayFun[ToArrayOfFuns[f].ToArrayOfFuns[g]];

IFun/:Dot[f_List?(!ArrayFunQ[#]&),g_IFun?ArrayFunQ]:=ToArrayFun[f.ToArrayOfFuns[g]];

MapToValues[op_]:=(op[if_IFun]^:=IFun[op[Values[if]],if//Domain]);
MapToValues/@{Abs,Arg,Re,Im,Conjugate,Exp,Tan,ArcSin,Sec,Sin,Cos,Log,ArcTanh};
Inverse[if_IFun]^:=Inverse/@if;
Transpose[f_IFun]^:=Transpose/@f;
Max[f_IFun]^:=f//Values//Max;
Min[f_IFun]^:=f//Values//Min;
Norm[f_IFun]^:=f//Values//Flatten//Norm;
Mean[f_IFun]^:=DCT[f][[1]];

NEqual[f_IFun,g_IFun]:=Norm[f-g]<$MachineTolerance;


MapToInterval[f_IFun,z_]:=MapToInterval[f//Domain,z];
MapToIntervalD[f_IFun,z_]:=MapToIntervalD[f//Domain,z];
MapFromInterval[f_IFun,z_]:=MapFromInterval[f//Domain,z];
MapFromIntervalD[f_IFun,z_]:=MapFromIntervalD[f//Domain,z];

ComplexMapToInterval[f_IFun,z_]:=ComplexMapToInterval[f//Domain,z];
ComplexMapToIntervalD[f_IFun,z_]:=ComplexMapToIntervalD[f//Domain,z];

MapToIntervalSeriesAtInfinity[f_IFun,s_]:=MapToIntervalSeriesAtInfinity[f//Domain,s];


RightEndpointInfinityQ[f_IFun]:=RightEndpointInfinityQ[Domain[f]];
LeftEndpointInfinityQ[f_IFun]:=LeftEndpointInfinityQ[Domain[f]];
LeftEndpoint[f_IFun]:=f//Domain//LeftEndpoint;
RightEndpoint[f_IFun]:=f//Domain//RightEndpoint;
LeftContourArg[f_IFun]:=f//Domain//LeftContourArg;
RightContourArg[f_IFun]:=f//Domain//RightContourArg;


First[f_IFun]^:=f//Values//First;
Last[f_IFun]^:=f//Values//Last;
Dimensions[f_IFun]^:=f//First//Dimensions;
ReverseOrientation[f_IFun]:=IFun[Reverse[Values[f]],ReverseOrientation[Domain[f]]];
IFun/:f_IFun?MatrixFunQ[[i_,j_]]:=(f//ToMatrixOfFuns)[[i,j]]//ToArrayFun;
IFun/:f_IFun?ListFunQ[[i_]]:=(f//ToArrayOfFuns)[[i]]//ToArrayFun;


MeanZero[f_IFun]:=f-First[DCT[f]];
ZeroAtZero[f_IFun]/;OddQ[Length[f]]:=f-f[0.];
ZeroAtRight[f_]:=f-Last[f];
ZeroAtLeft[f_]:=f-First[f];



FinitePoints[if_IFun?(LeftEndpointInfinityQ[#]&&RightEndpointInfinityQ[#]&)]:=if//Points//Most//Rest;
FiniteValues[if_IFun?(LeftEndpointInfinityQ[#]&&RightEndpointInfinityQ[#]&)]:=if//Values//Most//Rest;
FinitePoints[if_IFun?LeftEndpointInfinityQ]:=if//Points//Rest;
FiniteValues[if_IFun?LeftEndpointInfinityQ]:=if//Values//Rest;
FinitePoints[if_IFun?RightEndpointInfinityQ]:=if//Points//Most;
FiniteValues[if_IFun?RightEndpointInfinityQ]:=if//Values//Most;

FiniteValues[if_IFun]:=Values[if];
FinitePoints[if_IFun]:=Points[if];

FiniteRealPoints[if_IFun]/;Re[LeftEndpoint[if]]~NEqual~Re[RightEndpoint[if]]:=if//FinitePoints//Im;

Format[f:IFun[l_List,_]]:=ReImLinePlot[f,Sequence@@$FunFormat];



End[];



ChebyshevD::usage="Maps a list of Chebyshev coefficients to those of its derivative.";
ChebyshevLobattoDerivative::usage="Maps a list of function values at Chebyshev points to those of its derivative.";
ChebyshevI::usage="Maps a list of Chebyshev coefficients to those of its indefinite integral.";
ChebyshevLobattoIntegrate::usage="Maps a list of function values at Chebyshev points to those of its indefinite integral.";



Begin["Private`"];



ChebyshevD[c_List]:=Module[{d,n},
n=Length[c];
If[n==1,{0 c[[1]]},
d[n]=d[n-1]=0 c[[1]];
Do[
d[k]=d[k+2]+2 (k+1) c[[k+2]],
{k,n-2,1,-1}];
d[0]=d[2]/2 + c[[2]];
Array[d[#-1]&,{n}]]];

ChebyshevLobattoDerivative[c_List]:=
InverseDCT[ChebyshevD[DCT[c]]];


IFun/:Derivative[k_?Positive][f_IFun?ArrayFunQ]:=ArrayMap[Derivative[k],f//ToArrayOfFuns]//ToArrayFun;
IFun/:Derivative[0][if_IFun]:=if;
IFun/:Derivative[1][if_IFun]:=IFun[ChebyshevLobattoDerivative[Values[if]] ,if//Domain] IFun[MapToIntervalD[if,#]&,if//Domain,if//Length];
IFun/:Derivative[k_?Positive][if_IFun]:=Derivative[1][Derivative[k-1][if]];
IFun/:Derivative[if_IFun]:=if';


ReduceDimension[f_IFun]:=
IFun[f//DCT//Most//InverseDCT,Domain[f]];
ReduceDimensionIntegrate[f_IFun]:=ReduceDimension[Integrate[f]];

ChebyshevI[c_List]:=Module[{c2},
c2=ZeroVector[Length[c]+1];
c2[[2]]=c[[1]];
c2[[1]]=c[[1]];
If[Length[c]>1,
c2[[3]]=c[[2]]/4;
c2[[1]]=c2[[1]]-c[[2]]/4;
Table[c2[[k+1]]=c2[[k+1]]+c[[k]]/(2k);
	c2[[k-1]]=c2[[k-1]]-c[[k]]/(2(k-2));
	c2[[1]]=c2[[1]]+(-1)^k c[[k]](1/(2(k-2))-1/(2k));
,{k,3,Length[c]}]];
c2
];

ChebyshevLobattoIntegrate[c_List]:=
InverseDCT[ChebyshevI[DCT[c]]];

Integrate[if_IFun]^:=IFun[(Values[if] Values[IFun[MapFromIntervalD[if,#]&,UnitInterval,if//Length]])//ChebyshevLobattoIntegrate,if//Domain] ;

DomainIntegrate[if_IFun]:=if//Integrate//Last;
DomainIntegrate[{if__IFun}]:=Plus@@(DomainIntegrate/@{if});

End[];




Begin["Private`"];


ToUnitInterval[lf_IFun]:=SetDomain[lf,UnitInterval];



ChopDrop[cf_IFun,prec_]:=IFun[InverseDCT[ChopDrop[cf//DCT,prec]//Which[Length[#]==0,{0,0},Length[#]==1,{#[[1]],0 #[[1]]},True,#]&],Domain[cf]];
ChopDrop[cf_IFun]:=IFun[InverseDCT[ChopDrop[cf//DCT]//Which[Length[#]==0,{0,0},Length[#]==1,{#[[1]],0 #[[1]]},True,#]&],Domain[cf]];



Options[AdaptiveIFun]:={InterpolationPrecision->$MachineTolerance};
AdaptiveIFun[m_Integer,f_,d_,pars:OptionsPattern[]]:=Module[{prec},
prec=InterpolationPrecision/.{pars}/.Options[AdaptiveIFun];
IFun[f,d,m]//If[(DCT[#][[-2;;-1]]//Flatten//Abs//Max)<prec (Max[$MachineTolerance,Norm/@Values[#]]),#,AdaptiveIFun[2m,f,d,pars]]&//ChopDrop[#,prec (Max[$MachineTolerance,Norm/@Values[#]])]&//IFun[f,d,Length[#]]&
];


IFun[f_?NotListOrPatternQ,d_,opts:OptionsPattern[]]:=AdaptiveIFun[4,f,d,opts];



End[];


FromValueList;


Begin["Private`"];


ZeroAtInfinityIFun[ls_List,d_?LeftEndpointInfinityQ]:=IFun[Join[{0 ls[[1]]},ls],d];
ZeroAtInfinityIFun[ls_List,d_?RightEndpointInfinityQ]:=IFun[Join[ls,{0 ls[[1]]}],d];
ZeroAtInfinityIFun[ls_List,d_]:=IFun[ls,d];



FromValueList[f_IFun?ScalarFunQ,ls_]:=ZeroAtInfinityIFun[ls,Domain[f]];
FromValueList[f_IFun?VectorFunQ,ls_]:=ZeroAtInfinityIFun[#,Domain[f]]&/@Partition[ls,FiniteLength[f]]//ToArrayFun;
FromValueList[f_IFun?MatrixFunQ,ls_]:=MatrixMap[ZeroAtInfinityIFun[#,Domain[f]]&,PartitionList[Partition[ls,FiniteLength[f]],f//Dimensions]]//ToArrayFun;

End[];




TransformMatrix;
DerivativeMatrix;
IntegrateMatrix;
FiniteTransformMatrix;
BoundedIntegrateMatrix;

ReduceDimensionMatrix;


Begin["Private`"];

DCTTransformMatrix[n_Integer]:=ColumnMap[DCT,IdentityMatrix[n]];


TransformMatrix[_?IntervalDomainQ,n_Integer]:=DCTTransformMatrix[n];


FiniteTransformMatrix[d_?LeftEndpointInfinityQ,n_Integer]:=TransformMatrix[d,n][[All,2;;]];
FiniteTransformMatrix[d_?RightEndpointInfinityQ,n_Integer]:=TransformMatrix[d,n][[All,;;-2]];


DerivativeMatrix[1][d_?IntervalDomainQ,n_Integer]:=MapToIntervalD[d,Points[d,n]]ColumnMap[ChebyshevLobattoDerivative,IdentityMatrix[n]];


ReduceDimensionMatrix[d_,n_]:=Inverse[TransformMatrix[d,n-1]].IdentityMatrix[n][[;;-2,All]].TransformMatrix[n];
ReduceDimensionMatrix[f_]:=ReduceDimensionMatrix[f//Domain,f//Length];

ReduceDimensionIntegrateMatrix[d_?DomainQ,n_Integer]:=ReduceDimensionMatrix[d,n+1].IntegrateMatrix[d,n];
ReduceDimensionIntegrateMatrix[f_]:=ReduceDimensionIntegrateMatrix[f//Domain,f//Length];


IntegrateMatrix[d_?IntervalDomainQ,n_Integer]:=ColumnMap[ChebyshevLobattoIntegrate,IdentityMatrix[n]].DiagonalMatrix[MapFromIntervalD[d,Points[UnitInterval,n]]];


DiagonalMatrix[f_IFun]^:=f//Values//DiagonalMatrix;
IdentityMatrix[f_IFun]^:=f//Length//IdentityMatrix;



ComplexRoots[cf_IFun?UnitIntervalFunQ]:=Module[{dct},
dct=Chop[cf//DCT,$MachineTolerance]//RemoveZeros;
Which[Length[dct]<=1,
{},
Length[dct]==2,
{-dct[[1]]/dct[[2]]},
True,
-PadLeft[{(dct//Most)/(2 (dct//Last))},{Length[dct]-1,Length[dct]-1}]+(SparseArray[{{1,2}->1,{i_,j_}/;j==i+1->0.5,{i_,j_}/;j==i-1->0.5},{Length[dct]-1,Length[dct]-1}])//Eigenvalues]];


ComplexRoots[cf_IFun]:=MapFromInterval[cf,cf//ToUnitInterval//ComplexRoots];

Roots[cf_IFun]^:=
MapFromInterval[cf,Select[cf//ToUnitInterval//ComplexRoots,(Abs[Im[#]]<100$MachineTolerance)&&(-1.<=Re[#]<=1.)&]//Re//Sort];



Minimize[cf_IFun]^:=Join[cf'//Roots,Endpoints[cf]]//Thread[{cf/@#//Abs,#}]&//Sort//First//Second;


Fun[f_?NotListOrPatternQ,l:Line[{_,_},___],opts___]:=IFun[f,l,opts];

Fun[f_?NotListOrPatternQ,Line[l:{_,_,___},Lopts___],n_List]:=IFun[f,Line[#[[1]],Sequence@@If[Or@@(InfinityQ/@#[[1]]),{Lopts},{}]],#[[2]]]&/@Thread[{Partition[l,2,1],n}]//If[Length[#]==1,#[[1]],#]&;
Fun[f_?NotListOrPatternQ,Line[l:{_,_,___},Lopts___],n_Integer]:=Fun[f,Line[l,Lopts],n OneVector[Length[l]-1]];
Fun[f_?NotListOrPatternQ,Line[l:{_,_,___},Lopts___],opts:OptionsPattern[]]:=IFun[f,Line[#,Sequence@@If[Or@@(InfinityQ/@#),{Lopts},{}]],opts]&/@Partition[l,2,1]//If[Length[#]==1,#[[1]],#]&;
Fun[f_,d_?IntervalDomainQ,opts___]:=IFun[f,d,opts];


ZeroAtInfinityFun[f_List,d_?IntervalDomainQ]:=ZeroAtInfinityIFun[f,d];


ZeroAtInfinityIFun[f_?NotListOrPatternQ,d_,opts___]:=IFun[If[InfinityQ[#],0 f[0.],f[#]/.Underflow[]->0]&,d,opts];

IdentityAtInfinityIFun[G_?NotListOrPatternQ,pars___]:=IFun[If[InfinityQ[#],If[G[0.]//MatrixQ,IdentityMatrix[Length[G[0.]]],1],G[#]/.Underflow[]->0]&,pars];


DCTPlot[f_IFun,opts:OptionsPattern[]]:=ListLineLogPlot[Norm/@(f//DCT),opts];


IntervalDomainQ[Curve[_IFun]]:=True;






MapFromInterval[Curve[cr_IFun],z_]:=MapDot[ChebyshevT[#-1,z]&,DCT[cr]];
MapFromIntervalD[Curve[cr_IFun],z_]:=MapDot[ChebyshevT[#-1,z]&,DCT[cr']];
MapToInterval[Curve[cr_],z_?InfinityQ]:=z;
MapFromInterval[Curve[cr_],z_?InfinityQ]:=z;
MapToInterval[Curve[cr_],z_]/;z~NEqual~First[cr]:=-1.;
MapToInterval[Curve[cr_],z_]/;z~NEqual~Last[cr]:=1.;
MapToInterval[Curve[cr_],z_]:=cr-z//Roots//If[#=={},{},First[#]]&;
MapToIntervalD[cr:Curve[_],z_]:=1/MapFromIntervalD[cr,MapToInterval[cr,z]];
ComplexMapToInterval[Curve[cr_],z_]:=cr-z//ComplexRoots;
ComplexMapToIntervalD[cr:Curve[_],z_]:=1/MapFromIntervalD[cr,ComplexMapToInterval[cr,z]];






MapFromInterval[Curve[cr_IFun,Stretch->L_],z_]:=MapFromInterval[Curve[cr],MapFromInterval[Line[{-1,1},Stretch->L],z]];
MapFromIntervalD[Curve[cr_IFun,Stretch->L_],z_]:=MapFromIntervalD[Line[{-1,1},Stretch->L],z] MapFromIntervalD[Curve[cr],MapFromInterval[Line[{-1,1},Stretch->L],z]];
MapToInterval[Curve[cr_IFun,Stretch->L_],z_]:=MapToInterval[Line[{-1,1},Stretch->L],MapToInterval[cr//Curve,z]];
MapToIntervalD[Curve[cr_IFun,Stretch->L_],z_]:=MapToIntervalD[Line[{-1,1},Stretch->L],MapToInterval[cr//Curve,z]]/MapFromIntervalD[cr//Curve,MapToInterval[cr//Curve,z]];
ComplexMapToInterval[Curve[cr_IFun,Stretch->L_],z_]:=MapToInterval[Line[{-1,1},Stretch->L],ComplexMapToInterval[cr//Curve,z]];
ComplexMapToIntervalD[Curve[cr_IFun,Stretch->L_],z_]:=MapToIntervalD[Line[{-1,1},Stretch->L],ComplexMapToInterval[cr//Curve,z]]/MapFromIntervalD[cr//Curve,ComplexMapToInterval[cr//Curve,z]];



ComplexPlot[cf_IFun,opts:OptionsPattern[]]:=ListLinePlot[{Re[#],Im[#]}&/@cf//Values,opts];


Conjugate[Arc[x0_,r_,{t0_,t1_}]]^:=Arc[Conjugate[x0],r,{-t0,-t1}];
Conjugate[Curve[cr_IFun,opts___]]^:=Curve[Conjugate[cr],opts];
ReverseOrientation[Curve[cr_IFun,opts___]]:=Curve[Head[cr][Reverse[Values[cr]],Domain[cr]],opts];
Curve/:c_?NumberQ+Curve[cr_IFun,opts___]:=Curve[c+cr,opts];
Curve/:c_?NumberQ Curve[cr_IFun,opts___]:=Curve[c cr,opts];
Arc/:c_?NumberQ Arc[x0_,r_,{t0_,t1_}]:=Arc[c x0,r,{\[Pi]-t0,\[Pi]-t1}];


Line[ls_,___]~NEqual~Line[ls2_,___]:=NZeroQ[(If[#[[1]]==#[[2]],0,#[[1]]-#[[2]]]&/@Thread[{Sort[ls],Sort[ls2]}])//Flatten//Abs//Max];
Arc[x0_,r_,t0_]~NEqual~Arc[x2_,r2_,t2_]:=NZeroQ[{x0-x2,r-r2,Exp[I t0]-Exp[I t2]}//Flatten//Abs//Max];


SelectWithPoint[GG_,Gg_]:=Sort[Select[GG,Or[LeftEndpoint[#]~NEqual~(Gg),RightEndpoint[#]~NEqual~(Gg)]&],
If[LeftEndpoint[#1]~NEqual~(Gg),
LeftContourArg[#1],
RightContourArg[#1]]<If[LeftEndpoint[#2]~NEqual~(Gg),
LeftContourArg[#2],
RightContourArg[#2]]&];
SelectWithPoint[GG_,_?InfinityQ]:=Sort[Select[GG,Or[InfinityQ[LeftEndpoint[#]],InfinityQ[RightEndpoint[#]]]&],
If[LeftEndpoint[#1]//InfinityQ,
LeftContourArg[#1],
RightContourArg[#1]]<If[LeftEndpoint[#2]//InfinityQ,
LeftContourArg[#2],
RightContourArg[#2]]&];
Endpoints[GG_IFun]:={GG//LeftEndpoint//N//Chop,GG//RightEndpoint//N//Chop}/._?InfinityQ->\[Infinity];
Endpoints[GG_List]:=Union@@({LeftEndpoint/@GG//N//Chop,RightEndpoint/@GG//N//Chop}/._?InfinityQ->\[Infinity]);
FiniteEndpoints[GG_]:=Select[Endpoints[GG],!InfinityQ[#]&];


LegendreTransform[if_,m_]:=Module[{p,w,cf,ifv,j,n},
{p,w}=Thread[GaussianQuadratureWeights[m,-1,1]];
ifv=if/@p;
cf[0]=w LegendreP[0,p];
cf[1]=w LegendreP[1,p];
cf[n_]:=cf[n]=((2n-1) p cf[n-1] - (n-1) cf[n-2])/n;
Table[(2 j+1)/2 (cf[j].ifv),{j,0,m-1}]];

LegendreTransform[if_IFun]:=LegendreTransform[if//ToUnitInterval,if//Length];
LegendreTransform[if_List]:=LegendreTransform[IFun[if,UnitInterval],if//Length];

EvaluateMatrix[f_IFun,x_List]:=EvaluateMatrix[f//ToUnitInterval,MapToInterval[f,x]];
EvaluateMatrix[f_IFun?UnitIntervalFunQ,x_List]:=Module[{w,j,p,k,n},
k=Length[f];
n=NChebyshevLobattoPoints[k];
w[1]=1/2;
w[j_]=(-1)^(j+1);
w[k]=(-1)^(k+1) 1/2;
If[MemberQ[n,N[Chop[#,$MachineEpsilon]]],BasisVector[k][Position[n,N[Chop[#,$MachineEpsilon]]][[1,1]]],
Table[w[j]/(#-n[[j]]),{j,1,k}]/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(k\)]\(w[j]/\((# - n[[j]])\)\)\)]&/@x
];
EvaluateMatrix[f_IFun,x_]:=EvaluateMatrix[f,{x}]//First;
LegendreTransformMatrix[if_IFun]:=Module[{p,w,cf,ifv,j,n,m},
m=Length[if];
{p,w}=Thread[GaussianQuadratureWeights[m,-1,1]];
cf[0]=w LegendreP[0,p];
cf[1]=w LegendreP[1,p];
cf[n_]:=cf[n]=((2n-1) p cf[n-1] - (n-1) cf[n-2])/n;
Table[(2 j+1)/2 (cf[j]),{j,0,m-1}].EvaluateMatrix[if//ToUnitInterval,p]];

FiniteLegendreTransformMatrix[if_?LeftEndpointInfinityQ]:=LegendreTransformMatrix[if][[All,2;;]];
FiniteLegendreTransformMatrix[if_?RightEndpointInfinityQ]:=LegendreTransformMatrix[if][[All,;;-2]];

FiniteInverseLegendreTransformMatrix[if_?LeftEndpointInfinityQ]:=Inverse[LegendreTransformMatrix[if]][[2;;,All]];
FiniteInverseLegendreTransformMatrix[if_?RightEndpointInfinityQ]:=Inverse[LegendreTransformMatrix[if]][[;;-2,All]];


OneFun[lf_?FunQ]:=Head[lf][1&,lf//Domain,lf//Length];


End[];
EndPackage[];
