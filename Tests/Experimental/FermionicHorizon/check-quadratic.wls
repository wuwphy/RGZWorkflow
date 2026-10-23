(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Independent exact color/block checks; no production files are loaded. *)
Module[{x,m,h,r,s,w,cf=4/3,a,qq,lam,up,down,tt,bb,cc,kk,dd,checks,zero},
 tt={{{0,1,0},{1,0,0},{0,0,0}},{{0,-I,0},{I,0,0},{0,0,0}},
 {{1,0,0},{0,-1,0},{0,0,0}},{{0,0,1},{0,0,0},{1,0,0}},
 {{0,0,-I},{0,0,0},{I,0,0}},{{0,0,0},{0,0,1},{0,1,0}},
 {{0,0,0},{0,0,-I},{0,I,0}},DiagonalMatrix[{1,1,-2}]/Sqrt[3]}/2;
 bb=ArrayFlatten[{tt}];cc=ConjugateTranspose[bb];
 a=m-h^2 cf/r;qq=I/(x-a) IdentityMatrix[3];
 up=h qq . bb/r;down=h cc . qq/r;
 lam=-I IdentityMatrix[24]/r+h^2 cc . qq . bb/r^2;
 kk=ArrayFlatten[{{(x-m) IdentityMatrix[3],h bb},{h cc,-r IdentityMatrix[24]}}];
 dd=ArrayFlatten[{{qq,up},{down,lam}}];
 zero=Map[Cancel[Together[#]]&,kk . dd-I IdentityMatrix[27],{2}];
 checks=<|"FundamentalCasimir"->(Simplify[bb . cc]===cf IdentityMatrix[3]),
 "FullUnprojected27ColorBlockInverse"->(zero===ConstantArray[0,{27,27}]),
 "DiracResolvent"->(Cancel[(x+a)/(x^2-a^2)-1/(x-a)]===0),
 "EuclideanMassFunction"->(Cancel[(m-h^2 cf/(s-w^2)/.s->-s)-(m+h^2 cf/(s+w^2))]===0),
 "ZeroHorizonQuark"->(Cancel[(I/(x-a)/.h->0)-I/(x-m)]===0),
 "ZeroHorizonMixed"->((up/.h->0)===ConstantArray[0,{3,24}]),
 "ZeroHorizonAuxiliary"->((lam/.h->0)===-I IdentityMatrix[24]/r),
 "EqualSpinColorMultiplicity"->(4*8*3===96)|>;
 Print[checks];Exit[If[And@@Values[checks],0,1]]];
