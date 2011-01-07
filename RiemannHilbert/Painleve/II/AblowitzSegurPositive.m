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
Begin["`AblowitzSegurPositive`"];


\[Theta][z_]:=I(4/3 z^3 + z);
G//Clear;
s//Clear;
G[_,_][_?InfinityQ]:=IdentityMatrix[2];
G[{x_,s_},1][z_]=({
 {Exp[-x^(3/2)  \[Theta][z]], 0},
 {0, Exp[x^(3/2)  \[Theta][z]]}
}).({
 {1, 0},
 {I s, 1}
}).({
 {Exp[x^(3/2)  \[Theta][z]], 0},
 {0, Exp[-x^(3/2)  \[Theta][z]]}
});
G[{x_,s_},6][z_]=({
 {Exp[-x^(3/2)  \[Theta][z]], 0},
 {0, Exp[x^(3/2)  \[Theta][z]]}
}).({
 {1, I s},
 {0, 1}
}).({
 {Exp[x^(3/2)  \[Theta][z]], 0},
 {0, Exp[-x^(3/2)  \[Theta][z]]}
})//Inverse;


Cdefs[n_]:={Function[x,({
 {.5 I, -.5 I},
 {x, -x}
})],({
 {Line[{-4.,4.}], n}
})};
Gl[{x_,s_}]:=({
 {G[{x,s},1], G[{x,s},6]}
})


slvr//Clear;slvr:=slvr=ScaledRHSolver[Cdefs[40]];


AblowitzSegurPositivePainleveII[s_,x_]:=-(1/\[Pi])Sqrt[x]Total[DomainIntegrate/@slvr[x,Gl[{x,-s}]]][[1,2]];


End[]
