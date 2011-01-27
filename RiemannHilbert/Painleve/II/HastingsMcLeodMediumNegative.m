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



`Private`PainleveIINegative["HMMed"] =`HastingsMcLeodMediumNegative`PainleveIIHMMedNeg;
Begin["`HastingsMcLeodMediumNegative`"];


\[Beta][y_,\[Lambda]_]:=((\[Lambda]-y)/(\[Lambda]+y))^(1/4);
\[Beta]p[y_,\[Lambda]_]:=PowerBranch[(\[Lambda]-y)/(\[Lambda]+y),1/4,0];
\[Beta]m[y_,\[Lambda]_]:=PowerBranch[(\[Lambda]-y)/(\[Lambda]+y),1/4,-2\[Pi]];
Y[{\[Sigma]_,y_},\[Lambda]_]:=1/2({
 {\[Beta][y,\[Lambda]]+\[Beta][y,\[Lambda]]^(-1), -\[Sigma] (\[Beta][y,\[Lambda]]-\[Beta][y,\[Lambda]]^(-1))},
 {-\[Sigma] (\[Beta][y,\[Lambda]]-\[Beta][y,\[Lambda]]^(-1)), \[Beta][y,\[Lambda]]+\[Beta][y,\[Lambda]]^(-1)}
});
Yp[{\[Sigma]_,y_},\[Lambda]_]:=1/2({
 {\[Beta]p[y,\[Lambda]]+\[Beta]p[y,\[Lambda]]^(-1), -\[Sigma] (\[Beta]p[y,\[Lambda]]-\[Beta]p[y,\[Lambda]]^(-1))},
 {-\[Sigma] (\[Beta]p[y,\[Lambda]]-\[Beta]p[y,\[Lambda]]^(-1)), \[Beta]p[y,\[Lambda]]+\[Beta]p[y,\[Lambda]]^(-1)}
});
Ym[{\[Sigma]_,y_},\[Lambda]_]:=1/2({
 {\[Beta]m[y,\[Lambda]]+\[Beta]m[y,\[Lambda]]^(-1), -\[Sigma] (\[Beta]m[y,\[Lambda]]-\[Beta]m[y,\[Lambda]]^(-1))},
 {-\[Sigma] (\[Beta]m[y,\[Lambda]]-\[Beta]m[y,\[Lambda]]^(-1)), \[Beta]m[y,\[Lambda]]+\[Beta]m[y,\[Lambda]]^(-1)}
});
\[Sigma]3=({
 {1, 0},
 {0, -1}
});
\[CapitalSigma]=({
 {0, I},
 {I, 0}
});

g[_,_?InfinityQ]:=\[Infinity];
g[y_,z_]:=I 4/3 (z+y)^(3/2)(z-y)^(3/2);
gp[y_,z_]:=I 4/3 (z+y)^(3/2)PowerBranch[z-y,3/2,0];
gm[y_,z_]:=I 4/3 (z+y)^(3/2)PowerBranch[z-y,3/2,-2\[Pi]];
\[Theta][x_,z_]:=4 I/3z^3+I x z;
\[CapitalPhi][{\[Sigma]_,y_},z_]:=Y[{\[Sigma],y},z].MatrixExp[(-g[y,z]) \[Sigma]3];
\[CapitalPhi]p[{\[Sigma]_,y_},z_]:=Yp[{\[Sigma],y},z].MatrixExp[-(gp[y,z]) \[Sigma]3];
\[CapitalPhi]m[{\[Sigma]_,y_},z_]:=Ym[{\[Sigma],y},z].MatrixExp[-(gm[y,z]) \[Sigma]3];



Clear[GG\[CapitalSigma],GG\[CapitalSigma]in,GG,S];
S[s_,k_?EvenQ]:=({
 {1, s[k]},
 {0, 1}
});
S[s_,k_?OddQ]:=({
 {1, 0},
 {s[k], 1}
});
GG\[CapitalSigma][_][_,_?InfinityQ]:=IdentityMatrix[2];
GG\[CapitalSigma][{s_,y_}][z_]=\[CapitalPhi]m[{s[1]I,y},z].Inverse[S[s,1]].Inverse[\[CapitalPhi]m[{s[1]I,y},z]]//Simplify;
GG\[CapitalSigma]in[{s_,y_}][z_]=GG\[CapitalSigma][y][z]//Inverse;
GG[_,_][_?InfinityQ]:=IdentityMatrix[2];
GG[{s_,y_},6][z_]=\[CapitalPhi][{s[1]I,y},z].S[s,6].Inverse[\[CapitalPhi][{s[1]I,y},z]]//Simplify;
GG[{s_,y_},1][z_]=\[CapitalPhi][{s[1]I,y},z].S[s,1].Inverse[\[CapitalPhi][{s[1]I,y},z]]//Simplify;
GG[{s_,y_},3][z_]=\[CapitalPhi][{s[1]I,y},z].S[s,3].Inverse[\[CapitalPhi][{s[1]I,y},z]]//Simplify;
GG[{s_,y_},4][z_]=\[CapitalPhi][{s[1]I,y},z].S[s,4].Inverse[\[CapitalPhi][{s[1]I,y},z]]//Simplify;


GLC[{s_,y_},1][z_]=Inverse[\[CapitalPhi]m[{s[1]I,y},z]];
GLC[{s_,y_},2][z_]=S[s,6].S[s,1].S[s,3].Inverse[\[CapitalPhi][{s[1]I,y},z]];
GLC[{s_,y_},3][z_]=S[s,6].S[s,1].Inverse[\[CapitalPhi]p[{s[1]I,y},z]];


GRC[{s_,y_},1][z_]=S[s,6].S[s,1].Inverse[\[CapitalPhi]p[{s[1]I,y},z]];
GRC[{s_,y_},2][z_]=S[s,6].Inverse[\[CapitalPhi][{s[1]I,y},z]];
GRC[{s_,y_},3][z_]=Inverse[\[CapitalPhi]m[{s[1]I,y},z]];


\[CapitalPsi]pin[{\[Sigma]_,y_},z_]=Inverse[\[CapitalPhi]p[{\[Sigma],y},z]];
GMid[{s_,y_}][z_]=\[CapitalPhi]p[{s[1]I,y},z].S[s,2].\[CapitalPsi]pin[{s[1]I,y},z];


rngg={.5,2.3};

Cdefs[n_]:={{Function[y,({
 {-y, y},
 {y, -y}
})],({
 {Line[Exp[2 I \[Pi]/3]rngg], n},
 {Line[Exp[-2I \[Pi]/3]rngg], n},
 {Line[{1 ,Exp[-2I \[Pi]/3] }rngg[[1]]], n},
 {Line[{Exp[-2I \[Pi]/3] ,Exp[2I \[Pi]/3] }rngg[[1]]], n},
 {Line[{Exp[2I \[Pi]/3] ,1 }rngg[[1]]], n}
})},
{Function[y,({
 {0},
 {Sqrt[y]}
})],({
 {Line[2 {-I,0}], n},
 {Line[2 {0,I}], n}
})}};
Gl[y_]:={({
 {GG[y,3], GG[y,6]},
 {GG[y,4], GG[y,1]},
 {GLC[y,1], GRC[y,1]},
 {GLC[y,2], GRC[y,2]},
 {GLC[y,3], GRC[y,3]}
}),({
 {GMid[y]},
 {GMid[y]}
})};


\[CapitalPhi]\[Theta]Series[x_]:=-(Sqrt[-x/2]/2) ;

PainleveIIHMMedNeg[sin_,x_]:=Module[{s,y,n},
y=Sqrt[-x/2];
n=25;
{s[1],s[2],s[3]}=sin;
{s[4],s[5],s[6]}=-Array[s,3];
2 \[CapitalPhi]\[Theta]Series[x]-1/(\[Pi] I) (Join[ConstructCurve[Cdefs[n],Gl[{s,y}],y],
Fun[GG\[CapitalSigma][{s,y}],Line[{-y+rngg[[1]]/y,0,y-rngg[[1]]/y}],15]]//RHSolveTop//DomainIntegrate)[[2]]
]


End[]
