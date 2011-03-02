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

LFun::usage="LFun[\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\),d,n] constructs an \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\)-th order Laurent approximation of \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\) over the domain d, which is either a Circle or the real line Line[{-\[Infinity],\[Infinity]}]\!\(\*
StyleBox[\".\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"  \",\nFontSlant->\"Italic\"]\)If f is a constructed LFun, then\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)[x] evaluates the approximation at the point x. Operations that can be applied to an LFun include standard mathematical functions (Abs,Sin,Exp,etc.) and operators Integrate (returning the indefinite integral) and Derivative[k].";

Options[LFun]:={InterpolationPrecision->$MachineTolerance};

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
LaurentCoefficients::usage="Gives the Laurent coefficients of an LFun over a circle.";

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

ComplexMapToCircle::usage=
"MapToInterval[d,x] maps the point x via the conformal map that maps the domain d to the unit interval. If d is an IFun, then it is equivalent to MapToInterval[Domain[d],x].";

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
UnitCircle::usage=
"The unit circle Circle[0.,1.].";

SetDomain::usage="SetDomain[f,d] changes the domain of the Fun (or List of Funs) f to d";

ToUnitInterval::usage=
"ToUnitInterval[ifun] maps ifun to an IFun defined over the unit interval.";
ToUnitCircle::usage=
"ToUnitCircle[lfun] maps lfun to an LFun defined over the unit circle.";

DomainQ::usage=
"Test whether something is a domain (Line, Arc, Circle, Curve, etc).";

IntervalDomainQ::usage=
"Test whether something is a domain mapped from the unit interval.";

CircleDomainQ::usage=
"Test whether something is a domain mapped from the unit circle.";

BoundedDomainQ::usage="Test whether a domain is bounded.";

ZeroAtInfinityFun::usage="Constructs a Fun with the default value of zero at infinity";
IdentityAtInfinityFun::usage="Constructs a Fun with the default value of the identity matrix at infinity";

ZeroAtInfinityIFun::usage="Constructs a Fun with the default value of zero at infinity";
IdentityAtInfinityIFun::usage="Constructs a Fun with the default value of the identity matrix at infinity";

ZeroAtInfinityLFun::usage="Constructs a LFun with the default value of zero at infinity";

FunListDot::usage="FunListDot[{f1,f2,...,fn},{g1,g2,...,gn}] returns {f1.g1,f2.g2,...,fn.gn}";

DCTPlot::usage="LogPlots the norm of the Chebyshev coefficients of an IFun";
FFTPlot::usage="LogPlots the norm of the Fourier coefficients of an LFun";

ComplexRoots::usage="Returns all roots";


Curve::usage="Turns an IFun into a domain";


SelectWithPoint::usage="Select from a list of Funs the Fun which contains a point";
Endpoints::usage="Returns all endpoints of a list of funs";
FiniteEndpoints::usage="Returns all finite endpoints of a list of funs";

LegendreTransform::usage="Computes Legendre Coefficients";
EvaluateMatrix::usage="Matrix for evaluating at a point";



FunQ::usage="Tests if an object is in IFun or LFun.";
ListFunQ::usage="Tests if an object is in IFun whose values are a list.";
VectorFunQ::usage="Tests if an object is in IFun whose values are a vector.";
MatrixFunQ::usage="Tests if an object is in IFun whose values are a matrix.";
ArrayFunQ::usage="Tests if an object is in IFun whose values are an array.";

BoundedIntegrate::usage="BoundedIntegrate[lf] integrates an LFun with its -1 coefficient removed";

OneFun;

AddIdentityMatrix::usage=
"Adds IdentityMatrix[2] to a list of ifuns.";



LeftEvenPoints::usage="Evenly spaced points on the unit interval.";
NLeftEvenPoints;
FFT::usage=
"Returns a ShiftList of Laurent coefficients of a LFun or its value list.";
InverseFFT::usage=
"Returns values that a ShiftList of Laurent coefficients would take on the unit circle.";

Begin["Private`"];


LeftEvenPoints[n_]:=Range[-1,(n-1)/n,2/n];
LeftEvenPoints[n_,a_,b_]:=a+(b-a)(1+ LeftEvenPoints[n])/2;
NLeftEvenPoints[v__]:=N[LeftEvenPoints[v]];
FFT[f:{__?ScalarQ}]:=Module[{c,cc,n,scale,k},
n=Length[f];
scale=First[Fourier[(1&)/@LeftEvenPoints[n]]];
cc=-Reverse[AlternatingVector[n]]/scale (f//Fourier);
c[0]=(-1)^n cc[[1]];
c[k_]/;k<0:=(-1)^n cc[[1-k]];
c[k_]/;k>0:=cc[[-k]];

If[EvenQ[n],
	ShiftList[Table[c[k],{k,-n/2,-1}],Table[c[k],{k,0,n/2-1}]],
	ShiftList[Table[c[k],{k,-(n-1)/2,-1}],Table[c[k],{k,0,(n-1)/2}]]]
];
InverseFFT[c:ShiftList[{__?ScalarQ},_]]:=Module[{scale,cc,cf,j,n},
n=Length[c];
scale=First[Fourier[(1&)/@LeftEvenPoints[n]]];

-Reverse[AlternatingVector[n]]scale If[EvenQ[n],
	RotateLeft[c//ToList//Reverse,(n-2)/2],
	RotateLeft[ShiftList[(-1)^n OneVector[(n-1)/2],Join[{(-1)^n},OneVector[(n-1)/2]]]c//ToList//Reverse,(n-1)/2]
]
//InverseFourier];

FFT[f:{__?ArrayQ}]:=Map[FFT,ToArrayOfLists[f],{-2}]//ToShiftListOfArrays;
InverseFFT[sl:ShiftList[{__?ArrayQ},_]]:=ArrayMap[InverseFFT,ToArrayOfShiftLists[sl]]//ToListOfArrays;
End[];





CircleToPeriodicInterval;
RealLineToPeriodicInterval;
PeriodicIntervalToRealLine;

InfinityInDomainQ::usage="InfinityInDomainQ[d] tests if a domain is unbounded.";
RightEndpointInfinityQ::usage="RightEndpointInfinityQ[d] tests if a domain's right endpoint is unbounded.";
LeftEndpointInfinityQ::usage="LeftEndpointInfinityQ[d] tests if a domain's left endpoint is unbounded.";
ReverseOrientation::usage="ReverseOrientation[ifun] gives ifun with the orientation reversed.";


DomainMemberQ::usage="DomainMemberQ[d,z] tests whether z is numerically on the domain d.";

UnitCircleFunQ::usage="Tests whether an object is an LFun whose domain is the unit circle.";

MapToCircle::usage="MapToCircle[d,z] maps z via the conformal map from the domain d to the unit circle.";
MapFromCircle::usage="MapToCircle[d,z] maps z via the conformal map from the unit circle to the domain d.";
MapToCircleD::usage="MapToCircleD[d,z] is the derivative of MapToCircle[d,z].";
MapFromCircleD::usage="MapFromCircleD[d,z] is the derivative of MapFromCircle[d,z].";



RealLine::usage="The real line Line[{-\[Infinity],\[Infinity]}]";
Orientation;

Begin["Private`"];


CircleToPeriodicInterval[z_]:=-I Log[z];
RealLineToPeriodicInterval[x_]:=CircleToPeriodicInterval[RealLineToCircle[x]];
PeriodicIntervalToRealLine[t_]:=CircleToRealLine[Exp[I t]];
IntervalToPositivePeriodicInterval[x_]:=ArcCos[x];
IntervalToNegativePeriodicInterval[x_]:=-ArcCos[x];

DomainQ[d_]:=IntervalDomainQ[d]~Or~CircleDomainQ[d];

CircleDomainQ[_]:=False;
DomainMemberQ[f_?FunQ,x_]:=DomainMemberQ[f//Domain,x];
DomainMemberQ[d_?CircleDomainQ,x_]:=Abs[Abs[MapToCircle[d,x]]-1]<=10 $MachineTolerance;


UnitCircle=Circle[0,1];
UnitCircleFunQ[f_LFun]:=N[Domain[f]]==N[UnitCircle];
UnitCircleFunQ[_]:=False;

CircleDomainQ[_Circle]:=True;


SetAttributes[ComplexMapToCircle,Listable];
SetAttributes[MapToCircle,Listable];
SetAttributes[MapFromCircle,Listable];

MapToCircle[Circle[a_,r_],z_]:=(z-a)/r;
MapFromCircle[Circle[a_,r_],z_]:=r z+a;
MapToCircleD[Circle[a_,r_],z_]:=1/r;
MapFromCircleD[Circle[a_,r_],z_]:=r ;

MapToCircle[Circle[a_,r_,Orientation->1],z_]:=(z-a)/r;
MapFromCircle[Circle[a_,r_,Orientation->1],z_]:=r z+a;
MapToCircleD[Circle[a_,r_,Orientation->1],z_]:=1/r;
MapFromCircleD[Circle[a_,r_,Orientation->1],z_]:=r ;

MapToCircle[Circle[a_,r_,Orientation->-1],z_]:=r/(z-a);
MapFromCircle[Circle[a_,r_,Orientation->-1],z_]:=r/ z+a;
MapToCircleD[Circle[a_,r_,Orientation->-1],z_]:=-r/(z-a)^2;
MapFromCircleD[Circle[a_,r_,Orientation->-1],z_]:=-r /z^2;



RealLine=Line[{-\[Infinity],\[Infinity]}];

CircleDomainQ[RealLine]:=True;
IntervalDomainQ[RealLine]:=False;

MapFromCircle[RealLine,_?(#~NEqual~-1.&)]:=\[Infinity];
MapFromCircle[RealLine,z_]:=(-I z +I)/(z +1);
MapToCircle[RealLine,_?InfinityQ]:=-1.;
MapToCircle[RealLine,t_]:=(I-t)/(I+t);

MapFromCircleD[RealLine,z_]:=-((2 I)/(1+z)^2);
MapToCircleD[RealLine,t_]:=-((2 I)/(I+t)^2);


MapFromCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_],_?(#~NEqual~-1.&)]:=\[Infinity];
MapFromCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_],z_]:=-((I (-1+z))/(L (1+z)));
MapToCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_],_?InfinityQ]:=-1.;
MapToCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_],t_]:=( I-L t)/( I+L t);


MapFromCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_,Centre->a_],_?(#~NEqual~-1.&)]:=\[Infinity];
MapFromCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_,Centre->a_],z_]:=a-(I (-1+z))/(L (1+z));
MapToCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_,Centre->a_],_?InfinityQ]:=-1.;
MapToCircle[Line[{-\[Infinity],\[Infinity]},Stretch->L_,Centre->a_],t_]:=( I-L (t-a))/( I+L (t-a));

MapFromCircle[Line[{-\[Infinity] I,\[Infinity] I},Stretch->L_,Centre->a_],_?(#~NEqual~-1.&)]:=\[Infinity];
MapFromCircle[Line[{-\[Infinity] I,\[Infinity] I},Stretch->L_,Centre->a_],z_]:=a-I (I (-1+z))/(L (1+z));
MapToCircle[Line[{-\[Infinity] I,\[Infinity] I},Stretch->L_,Centre->a_],_?InfinityQ]:=-1.;
MapToCircle[Line[{-\[Infinity] I,\[Infinity] I},Stretch->L_,Centre->a_],t_]:=( I+I L (t-a))/( I- I L (t-a));


PeriodicInterval=Line[{-\[Pi],\[Pi]}];
MapFromCircle[PeriodicInterval,z_]:=CircleToPeriodicInterval[z];
MapToCircle[PeriodicInterval,z_]:=Exp[I z];
MapFromCircleD[PeriodicInterval,z_]:=CircleToPeriodicInterval'[z];
MapToCircleD[PeriodicInterval,z_]:=I Exp[I z];

MapFromCircle[l_Line,z_]:=MapFromInterval[l,MapFromCircle[PeriodicInterval,z]/\[Pi]];
MapToCircle[l_Line,z_]:=MapToCircle[PeriodicInterval,MapToInterval[l,z] \[Pi] ];
MapFromCircleD[l_Line,z_]:=MapFromIntervalD[l,MapFromCircle[PeriodicInterval,z]/\[Pi]] MapFromCircleD[PeriodicInterval,z]/\[Pi];
MapToCircleD[l_Line,z_]:=MapToCircleD[PeriodicInterval,MapToInterval[l,z] \[Pi] ] MapToIntervalD[l,z] \[Pi];


Ellipse[r_]:=Ellipse[{-1,1},r];

MapFromCircle[Ellipse[{a_,b_},r_],z_]:=MapFromInterval[Line[{a,b}],CircleToInterval[MapFromCircle[Circle[0,r],z]]];
MapToCircle[Ellipse[{a_,b_},r_],z_]:=MapToCircle[Circle[0,r],IntervalToInnerCircle[MapToInterval[Line[{a,b}],z]]];
MapFromCircleD[Ellipse[{a_,b_},r_],z_]:=MapFromIntervalD[Line[{a,b}],CircleToInterval[MapFromCircle[Circle[0,r],z]]]CircleToInterval'[MapFromCircle[Circle[0,r],z]]MapFromCircleD[Circle[0,r],z];
MapToCircleD[Ellipse[{a_,b_},r_],z_]:=MapToCircleD[Circle[0,r],IntervalToInnerCircle[MapToInterval[Line[{a,b}],z]]]IntervalToInnerCircle'[MapToInterval[Line[{a,b}],z]]MapToIntervalD[Line[{a,b}],z];

End[];






FinitePoints;

Begin["Private`"];



CirclePoints[n_]:=Exp[I \[Pi] LeftEvenPoints[n]];
NCirclePoints[n_]:=CirclePoints[n]//N;

Points[d_?CircleDomainQ,n_]:=MapFromCircle[d,NCirclePoints[n]];

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

MeanZero::usage="Subtracts out the mean of an IFun.";
ZeroAtZero;
ZeroAtRight;
ZeroAtLeft;

FiniteValues;
FiniteLength;

FastPlus;
FastTimes;

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


DomainPlot[Circle[z0_,r_],opts___]:=
Graphics[{Thick,Blue,PointSize[Large],Arrowheads[Medium],
Arrow[{z0+r Exp[I 0.2] //{Re[#],Im[#]}&,z0 + r Exp[I 0.2001]//{Re[#],Im[#]}&}],
Circle[{Re[#],Im[#]}&[z0],r]},opts,Axes->True];
DomainPlot[Circle[z0_,r_,Orientation->1],opts___]:=
DomainPlot[Circle[z0,r],opts];
DomainPlot[Circle[z0_,r_,Orientation->-1],opts___]:=
Graphics[{Thick,Blue,PointSize[Large],Arrowheads[Medium],
Arrow[{z0 + r Exp[I 0.2001]//{Re[#],Im[#]}&,z0+r Exp[I 0.2] //{Re[#],Im[#]}&}],
Circle[{Re[#],Im[#]}&[z0],r]},opts,Axes->True];


DomainPlot[ell_Ellipse,opts___]:=Module[{t},
Show[ComplexPlot[MapFromCircle[ell,Exp[I t]],{t,-\[Pi],\[Pi]},PlotStyle->Thick],Graphics[{Thick,Blue,PointSize[Large],Arrowheads[Medium],
Arrow[{MapFromCircle[ell,Exp[I 0.2]]//{Re[#],Im[#]}&,MapFromCircle[ell,Exp[I 0.2001]]//{Re[#],Im[#]}&}]},opts,Axes->True]]];

DomainPlot[if_?FunQ,opts___]:=DomainPlot[if//Domain,opts];

DomainPlot[l_List,opts___]:=Show[DomainPlot[#,opts]&/@l,PlotRange->All];



LFun[l_List,d_][z_]:=MapDot[MapToCircle[d,z]^#&,FFT[l]];
LFun[f_,d_,n_]:=LFun[f/@MapFromCircle[d,Points[UnitCircle,n]],d];

FunQ[_LFun]:=True;

Values[LFun[l_List,_]]:=l;
Domain[LFun[_,d_]]:=d;

Length[if_LFun]^:=if//Values//Length;
Points[if_LFun]:=MapFromCircle[if//Domain,Points[UnitCircle,if//Length]];


FFT[if_LFun]:=if//Values//FFT;

LaurentCoefficients[lf:LFun[_,_Circle]]:=MapOuter[MapToCircle[lf,1]^#&,FFT[lf]];

LFun/:Map[f_,g_LFun]:=LFun[f/@Values[g],Domain[g]];
FastPlus[f__LFun]:=LFun[Plus@@(Values/@{f}),Domain[{f}[[1]]]];
FastTimes[f__LFun]:=LFun[Times@@(Values/@{f}),Domain[{f}[[1]]]];
f_LFun+g_LFun^:=f~FastPlus~g;
Times[f_LFun,g_LFun]^:=f~FastTimes~g;
LFun/:f_?ConstantQ+g_LFun:=LFun[f+Values[g],g//Domain];
LFun/:g_LFun+f_?ConstantQ:=LFun[Values[g]+f,g//Domain];
LFun/:Times[f_?ConstantQ,g_LFun]:=LFun[f Values[g],g//Domain];
LFun/:Times[g_LFun,f_?ConstantQ]:=LFun[Values[g]f,g//Domain];
LFun/:f_LFun^c_?ConstantQ:=LFun[Values[f]^c,f//Domain];
LFun/:c_?ConstantQ^f_LFun:=LFun[c^Values[f],f//Domain];

Dot[f_LFun?ArrayFunQ,g_LFun?ArrayFunQ]^:=ToArrayFun[ToArrayOfFuns[f].ToArrayOfFuns[g]];

LFun/:Dot[f_List?(!ArrayFunQ[#]&),g_LFun?ArrayFunQ]:=ToArrayFun[f.ToArrayOfFuns[g]];


LMapToValues[op_]:=(op[if_LFun]^:=LFun[op[Values[if]],if//Domain]);
LMapToValues/@{Abs,Arg,Re,Im,Conjugate,Exp,Tan,ArcSin,Sec,Sin,Cos,Log,ArcTanh};

Max[f_LFun]^:=f//Values//Max;
Min[f_LFun]^:=f//Values//Min;
Norm[f_LFun]^:=f//Values//Flatten//Norm;
Mean[f_LFun]^:=FFT[f][[0]];


NEqual[f_LFun,g_LFun]:=Norm[f-g]<$MachineTolerance;
NEqual[f:{__?FunQ},g:{__?FunQ}]:=Norm[Norm/@(f-g)]<$MachineTolerance;

MeanZero[f_LFun]:=f-Mean[f];
MeanZero[sl_ShiftList]:=sl-sl[[0]] BasisShiftList[sl,0];

LFun/:f_LFun?MatrixFunQ[[i_,j_]]:=(f//ToMatrixOfFuns)[[i,j]]//ToArrayFun;
LFun/:f_LFun?ListFunQ[[i_]]:=(f//ToArrayOfFuns)[[i]]//ToArrayFun;

ComplexMapToCircle[f_?FunQ,z_]:=ComplexMapToCircle[f//Domain,z];

MapToCircle[f_LFun,z_]:=MapToCircle[f//Domain,z];
MapToCircleD[f_LFun,z_]:=MapToCircleD[f//Domain,z];
MapFromCircle[f_LFun,z_]:=MapFromCircle[f//Domain,z];
MapFromCircleD[f_LFun,z_]:=MapFromCircleD[f//Domain,z];


FinitePoints[if:LFun[_,RealLine]]:=if//Points//Rest;
FinitePoints[if_LFun]:=if//Points;
FiniteValues[if_LFun]:=Last/@Select[Thread[{Points[if],Values[if]}],!InfinityQ[First[#]]&];
FiniteLength[f_]:=f//FinitePoints//Length;
FiniteRealPoints[if_LFun]:=if//FinitePoints//Re;


$FunFormat={ImageSize->Small};
Format[f:LFun[l_List,_]]:=ReImLinePlot[f,Sequence@@$FunFormat];



LFun/:Derivative[0][if_LFun]:=if;
LFun/:Derivative[1][f_LFun]:=
	LFun[PadRight[ShiftLeft[MapOuter[#&,FFT[f]]],Length[f]+2]//InverseFFT,Domain[f]]//# LFun[MapToCircleD[f,#]&,f//Domain,#//Length]&;
LFun/:Derivative[k_?Positive][if_LFun]:=Derivative[1][Derivative[k-1][if]];
LFun/:Derivative[if_LFun]:=if';


DomainIntegrate[if_LFun?(Domain[#]==RealLine&)]:=((Values[if] Values[LFun[If[#==-1.,0,MapFromCircleD[if,#]]&,UnitCircle,if//Length]])//FFT)[[-1]] (2 \[Pi]\[NonBreakingSpace]I);
DomainIntegrate[if_LFun]:=((Values[if] Values[LFun[MapFromCircleD[if,#]&,UnitCircle,if//Length]])//FFT)[[-1]] (2 \[Pi]\[NonBreakingSpace]I);


MakeFFTIndexRange[sl_ShiftList]:=SetIndexRange[sl,{-1,1}(IndexRange[sl]//Abs//Max)];

BoundedIntegrate[lf_LFun?UnitCircleFunQ]:=LFun[MapOuter[If[ZeroQ[#],0,1/#]&,lf//FFT//ShiftRight]//MakeFFTIndexRange//InverseFFT,lf//Domain];

BoundedIntegrate[lf_LFun]:=SetDomain[BoundedIntegrate[ToUnitCircle[lf] LFun[MapFromCircleD[lf,#]&,UnitCircle,lf//Length]],lf//Domain];

End[];


NegativePart;
NonNegativePart;
PositivePart;
NonPositivePart;

NonNegativeEvaluate;
NegativeEvaluate;
PositiveEvaluate;
NonPositiveEvaluate;

Begin["Private`"];

NegativePart[lf_LFun]:=LFun[lf//FFT//NegativeShiftList//InverseFFT,lf//Domain];
NonNegativePart[lf_LFun]:=LFun[lf//FFT//NonNegativeShiftList//InverseFFT,lf//Domain];
PositivePart[lf_LFun]:=LFun[lf//FFT//PositiveShiftList//InverseFFT,lf//Domain];
NonPositivePart[lf_LFun]:=LFun[lf//FFT//NonPositiveShiftList//InverseFFT,lf//Domain];

NonNegativeEvaluate[f_LFun,z_]/;NZeroQ[MapToCircle[f,z]]:=(f//FFT)[[0]];
NonNegativeEvaluate[f_LFun,z_]:=MapDot[MapToCircle[f,z]^#&,f//FFT//NonNegativeShiftList];
NegativeEvaluate[f_LFun,z_]/;InfinityQ[MapToCircle[f,z]]:=0 First[Values[f]];
NegativeEvaluate[f_LFun,z_]:=MapDot[MapToCircle[f,z]^#&,f//FFT//NegativeShiftList];
PositiveEvaluate[f_LFun,z_]:=MapDot[MapToCircle[f,z]^#&,f//FFT//PositiveShiftList];
PositiveEvaluate[f_LFun,z_]/;NZeroQ[MapToCircle[f,z]]:=0 First[Values[f]];
NonPositiveEvaluate[f_LFun,z_]:=MapDot[MapToCircle[f,z]^#&,f//FFT//NonPositiveShiftList];
NonPositiveEvaluate[f_LFun,z_]/;InfinityQ[MapToCircle[f,z]]:=(f//FFT)[[0]];
End[];


SetLength;


Begin["Private`"];


SetLength[if_LFun,n_?OddQ]:=LFun[SetIndexRange[if//FFT,{(1-n)/2,(n-1)/2}]//InverseFFT,if//Domain];
SetLength[if_LFun,n_?EvenQ]:=LFun[SetIndexRange[if//FFT,{-(n/2),n/2-1}]//InverseFFT,if//Domain];




LFun[f_IFun]:=LFun[Join[Values[f],Reverse[Values[f]][[2;;-2]]],UnitCircle];


SetDomain[f_?FunQ,d_]:=Head[f][Values[f],d];

SetDomain[f_List,d_List]:=SetDomain@@#&/@Thread[{f,d}];

ToUnitCircle[lf_LFun]:=SetDomain[lf,UnitCircle];




ChopDrop[cf_LFun,prec_]:=LFun[ChopDrop[cf//FFT,prec]//Which[Length[#]==0,ShiftList[{0,0,0},2],IndexRange[#]=={0,0}, ShiftList[{0 #[[0]],#[[0]],0 #[[0]]},2],True,#]&//SetIndexRange[#,{-Max[Abs[IndexRange[#]]],Max[Abs[IndexRange[#]]]}]&//InverseFFT,cf//Domain];
ChopDrop[cf_LFun]:=LFun[ChopDrop[cf//FFT]//Which[Length[#]==0,ShiftList[{0,0,0},2],IndexRange[#]=={0,0}, ShiftList[{0 #[[0]],#[[0]],0 #[[0]]},2],True,#]&//SetIndexRange[#,{-Max[Abs[IndexRange[#]]],Max[Abs[IndexRange[#]]]}]&//InverseFFT,cf//Domain];




Options[AdaptiveLFun]:={InterpolationPrecision->$MachineTolerance};
AdaptiveLFun[m_Integer,f_,d_,pars:OptionsPattern[]]:=Module[{prec},
prec=InterpolationPrecision/.{pars}/.Options[AdaptiveLFun];
LFun[f,
d,Length[ChopDrop[LFun[f,d,m]//If[(ToList[FFT[#]][[{1,2,-1,-2}]]//Flatten//Abs//Max)<prec,#,AdaptiveLFun[2m,f,d,pars]]&,prec]]]
];


LFun[f_?NotListOrPatternQ,d_,opts:OptionsPattern[]]:=AdaptiveLFun[8,f,d,opts];



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


FromValueList[f_LFun?ScalarFunQ,ls_]:=ZeroAtInfinityLFun[ls,Domain[f]];
FromValueList[f_LFun?MatrixFunQ,ls_]:=MatrixMap[ZeroAtInfinityLFun[#,Domain[f]]&,PartitionList[Partition[ls,FiniteLength[f]],f//Dimensions]]//ToArrayFun;
FromValueList[f_LFun?VectorFunQ,ls_]:=ZeroAtInfinityLFun[#,Domain[f]]&/@Partition[ls,FiniteLength[f]]//ToArrayFun;


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


FFTTransformMatrix[n_Integer]:=ColumnMap[ToList[FFT[#]]&,IdentityMatrix[n]];

TransformMatrix[_?CircleDomainQ,n_Integer]:=FFTTransformMatrix[n];

TransformMatrix[f_?FunQ]:=TransformMatrix[f//Domain,f//Length];
FiniteTransformMatrix[RealLine,n_Integer]:=TransformMatrix[RealLine,n][[All,2;;]];

FiniteTransformMatrix[d_?DomainQ,n_Integer]:=TransformMatrix[d,n];
FiniteTransformMatrix[f_]:=FiniteTransformMatrix[f//Domain,f//Length];

DerivativeMatrix[1][f_?FunQ]:=DerivativeMatrix[1][f//Domain,f//Length];
DerivativeMatrix[k_Integer][pars__]:=MatrixPower[DerivativeMatrix[1][pars],k];
DerivativeMatrix[d_?DomainQ,n_Integer]:=DerivativeMatrix[1][d,n];
DerivativeMatrix[f_?FunQ]:=DerivativeMatrix[1][f];




IntegrateMatrix[f_]:=IntegrateMatrix[f//Domain,f//Length];


DiagonalMatrix[f_LFun]^:=f//Values//DiagonalMatrix;
IdentityMatrix[f_LFun]^:=f//Length//IdentityMatrix;


BoundedIntegrateMatrix[lf_LFun?UnitCircleFunQ]:=Module[{T,IM,i,n},
n=lf//Length;
T=TransformMatrix[lf];
IM=SparseArray[{i_,j_}/;j==i-1->If[FirstIndex[FFT[lf]]==1-i,0,1/(i-1+FirstIndex[FFT[lf]])],{n,n}];
Inverse[T].IM.T];
BoundedIntegrateMatrix[lf_LFun]:=BoundedIntegrateMatrix[lf//ToUnitCircle].DiagonalMatrix[LFun[MapFromCircleD[lf,#]&,UnitCircle,lf//Length]];


ComplexRoots[lf_LFun]:=ComplexRoots[lf//Domain,Chop[lf//FFT,$MachineTolerance]//RemoveZeros];

ComplexRoots[d_,fft_ShiftList]:=Module[{dct},
dct=fft//ToList;
MapFromCircle[d,
Join[Transpose[SparseArray[{i_,j_}/;j==i-1->1,{Length[dct]-1, Length[dct]-2}]],-{Most[dct]}/Last[dct]]//Transpose//Normal//N//Eigenvalues]];

Roots[lf_LFun]^:=Module[{dct},
dct=Chop[lf//FFT,$MachineTolerance]//RemoveZeros//ToList;
MapFromCircle[lf,
Select[Join[Transpose[SparseArray[{i_,j_}/;j==i-1->1,{Length[dct]-1, Length[dct]-2}]],-{Most[dct]}/Last[dct]]//Transpose//Eigenvalues,
Abs[Abs[#]-1]<10.^(-6)&]]];





Fun[f_,d_?CircleDomainQ,opts___]:=LFun[f,d,opts];

Fun[f_,l_List,n_List]:=Flatten[Fun[f,#[[1]],#[[2]]]&/@Thread[{l,n}]];

Fun[f_,l_List,opts___]:=Flatten[Fun[f,#,opts]&/@l];



ZeroAtInfinityFun[f_?NotListOrPatternQ,d_,opts___]:=Fun[If[InfinityQ[#],0 f[0.],f[#]/.Underflow[]->0]&,d,opts];


ZeroAtInfinityFun[f_List,d_?CircleDomainQ]:=ZeroAtInfinityLFun[f,d];

IdentityAtInfinityFun[G_?NotListOrPatternQ,pars___]:=Fun[If[InfinityQ[#],If[G[0.]//MatrixQ,IdentityMatrix[Length[G[0.]]],1],G[#]/.Underflow[]->0]&,pars];


ZeroAtInfinityLFun[f_?NotListOrPatternQ,d_,opts___]:=LFun[If[InfinityQ[#],0 f[0.],f[#]/.Underflow[]->0]&,d,opts];
ZeroAtInfinityLFun[f_List,RealLine]:=LFun[Join[{0},f],RealLine];
ZeroAtInfinityLFun[f_List,d_]:=LFun[f,d];



FunListDot[f_,g_]:=#[[1]].#[[2]]&/@Thread[{f,g}];


FFTPlot[f_LFun,opts:OptionsPattern[]]:=ListLineLogPlot[Norm/@(f//FFT),opts];



CircleDomainQ[Curve[_LFun]]:=True;





MapFromCircle[Curve[cr_LFun],z_]:=MapDot[z^#&,FFT[cr]];
MapFromCircleD[Curve[cr_LFun],z_]:=MapDot[z^#&,FFT[cr']];
MapToCircle[Curve[cr_],z_?InfinityQ]:=z;
MapFromCircle[Curve[cr_],z_?InfinityQ]:=z;
MapFromCircle[Curve[cr_],z_?ZeroQ]:=0;
MapToCircle[Curve[cr_],z_]:=cr-z//Roots//First;
MapToCircleD[cr:Curve[_],z_]:=1/MapFromCircleD[cr,MapToCircle[cr,z]];
ComplexMapToCircle[Curve[cr_],z_]:=ComplexRoots[UnitCircle,IncreaseIndexRange[RemoveNZeros[FFT[cr]],{0,0}]//#-z  BasisShiftList[#,0]&];




ComplexPlot[cf_LFun,opts:OptionsPattern[]]:=ListLinePlot[{Re[#],Im[#]}&/@cf//Values,opts];
DomainPlot[Curve[cf_,___],opts:OptionsPattern[]]:=Show[ComplexPlot[cf,opts,PlotStyle->{Thick,Blue}],
{Arrowheads[Medium],Blue,Arrow[{cf//Values//Last//{Re[#],Im[#]}&,(cf//Values//Last)+0.01Exp[I Arg[((cf//Values//Last)-(cf//Values)[[-2]])]]//{Re[#],Im[#]}&}]}//Graphics];



Curve[f_,opts___]~NEqual~Curve[g_,opts___]:=If[Length[f]==Length[g],(f-g//Values//Norm)<$MachineTolerance,False];


OneFun[lf_?FunQ]:=Head[lf][1&,lf//Domain,lf//Length];


End[];
EndPackage[];
