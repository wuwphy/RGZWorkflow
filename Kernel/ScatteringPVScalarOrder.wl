(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Scalar loop integrals are invariant under every propagator relabelling,
   including moving the zero chord. Tensor coefficients must not use this
   operation: a zero-chord shift mixes their tensor structures. *)
qqPVScalarCanonical[m_] := Module[{v=qqPVParse[m],ind,inv,mass,n,edges,mat,perms,candidates,best},
 If[v===$Failed,Return[m]];{ind,inv,mass}=v;n=Length[mass];
 If[ind=!={0},Return[m]];
 If[n===1&&inv==={},Return[FeynCalc`A0[First[mass]]]];
 If[n===2&&Length[inv]===1,Return[FeynCalc`B0[First[inv],Sequence@@Sort[mass]]]];
 If[!MemberQ[{3,4,5},n]||Length[inv]=!=n(n-1)/2,Return[m]];
 edges=qqPVEdges[n];mat=ConstantArray[0,{n,n}];
 Do[mat[[edges[[k,1]]+1,edges[[k,2]]+1]]=inv[[k]];
  mat[[edges[[k,2]]+1,edges[[k,1]]+1]]=inv[[k]],{k,Length[edges]}];
 perms=Select[Permutations[Range[n]],mass[[#]]===Sort[mass]&];
 candidates=Table[{mass[[per]],(mat[[per[[#[[1]]+1]],per[[#[[2]]+1]]]]& /@ edges)},{per,perms}];
 best=First[Sort[candidates]];
 Switch[n,3,FeynCalc`C0[Sequence@@Join[best[[2]],best[[1]]]],
 4,FeynCalc`D0[Sequence@@Join[best[[2]],best[[1]]]],
 5,FeynCalc`PaVe[0,best[[2]],best[[1]],FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False]]];
