(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* These Clifford/Dirac identities hold at symbolic D, without a Fierz reduction. *)
qqBoxWardLineChecks[] := Module[{k,l,p,mu,ubar,u,g,p2,exprs},
 ubar=FeynCalc`SpinorUBarD[k,0];u=FeynCalc`SpinorUD[l,0];g=FeynCalc`GAD[mu];p2=FeynCalc`SPD[p,p];
 exprs={ubar.FeynCalc`GSD[p-k].FeynCalc`GSD[p].g.u-p2 ubar.g.u,
 ubar.g.FeynCalc`GSD[p].FeynCalc`GSD[p-l].u-p2 ubar.g.u,
 ubar.FeynCalc`GSD[p-k].FeynCalc`GSD[p].FeynCalc`GSD[p-l].u-p2 ubar.FeynCalc`GSD[p].u};
 FeynCalc`DiracSimplify[#,FeynCalc`DiracEquation->True,FeynCalc`SirlinSimplify->False]===0& /@ exprs];
qqBoxProjectorWard[input_,name_] := Module[{norm,metric,chains,replace,pref,mu,j,jq,prime,indices,props,pol1,pol2,tensor,diff,
 q=RGZWorkflow`Engine`q,k1=RGZWorkflow`Engine`qqK1,k2=RGZWorkflow`Engine`qqK2,k3=RGZWorkflow`Engine`qqK3,k4=RGZWorkflow`Engine`qqK4,a,b,mom1,mom2,verts,matches},
 norm=FeynCalc`MomentumCombine[FeynCalc`DiracGammaCombine[FeynCalc`MomentumCombine[input]]];
 metric=norm/.HoldPattern[FeynCalc`Pair[_FeynCalc`Momentum,_FeynCalc`LorentzIndex]|FeynCalc`Pair[_FeynCalc`LorentzIndex,_FeynCalc`Momentum]]->0;
 chains=DeleteDuplicates[Cases[metric,_Dot,Infinity]];
 If[Length[chains]=!=2||!AllTrue[chains,Length[#]===5&&MatchQ[#[[1]],_FeynCalc`Spinor]&&MatchQ[#[[3]],FeynCalc`DiracGamma[_FeynCalc`Momentum,D]]&&MatchQ[#[[5]],_FeynCalc`Spinor]&],Return[$Failed]];
 If[!AllTrue[chains,#[[1,2]]===0&&#[[5,2]]===0&&#[[1,3]]===1&&#[[5,3]]===1&],Return[$Failed]];
 chains=SortBy[chains,FirstPosition[{FeynCalc`Momentum[k3,D],FeynCalc`Momentum[k4,D]},#[[1,1]]]&];
 If[chains[[1,1,1]]=!=FeynCalc`Momentum[k3,D]||chains[[1,5,1]]=!=FeynCalc`Momentum[k1,D]||chains[[2,1,1]]=!=FeynCalc`Momentum[k4,D]||chains[[2,5,1]]=!=FeynCalc`Momentum[k2,D],Return[$Failed]];
 {mom1,mom2}=If[name==="Box1",{k1-q,q+k2},{q,q+k2-k3}];
 If[!And@@MapThread[TrueQ[FeynCalc`MomentumExpand[#1[[3,1]]-FeynCalc`Momentum[#2,D]]===0]&,{chains,{mom1,mom2}}],Return[$Failed]];
 verts=Flatten[(List@@#& /@ chains)[[All,{2,4}]],1];
 If[!AllTrue[verts,Length[Cases[#,_FeynCalc`DiracGamma,Infinity]]===1&&MatchQ[First[Cases[#,_FeynCalc`DiracGamma,Infinity]],FeynCalc`DiracGamma[_FeynCalc`LorentzIndex,D]]&],Return[$Failed]];
 indices=First[Cases[#,_FeynCalc`DiracGamma,Infinity]][[1]]& /@ verts;
 props=Cases[metric,_FeynCalc`PropagatorDenominator,Infinity];If[Length[props]=!=2||!AllTrue[props,#[[2]]===0&],Return[$Failed]];
 matches=Table[TrueQ[FeynCalc`MomentumExpand[props[[i,1]]-FeynCalc`Momentum[{mom1,mom2}[[j]],D]]===0]||TrueQ[FeynCalc`MomentumExpand[props[[i,1]]+FeynCalc`Momentum[{mom1,mom2}[[j]],D]]===0],{i,2},{j,2}];
 If[!TrueQ[(matches[[1,1]]&&matches[[2,2]])||(matches[[1,2]]&&matches[[2,1]])],Return[$Failed]];
 replace=Thread[chains->Map[(#[[2]]/._FeynCalc`DiracGamma->1)(#[[4]]/._FeynCalc`DiracGamma->1)&,chains]];
 pref=((metric/.replace)/._FeynCalc`FeynAmpDenominator->1)/.HoldPattern[FeynCalc`Pair[_FeynCalc`LorentzIndex,_FeynCalc`LorentzIndex]]->1;
 If[!FreeQ[pref,_FeynCalc`DiracGamma|_FeynCalc`LorentzIndex|_FeynCalc`Spinor|_Dot],Return[$Failed]];
 pol1=If[name==="Box1",{q,indices[[2]],indices[[4]]},{q-k3,indices[[1]],indices[[4]]}];
 pol2=If[name==="Box1",{q-k1+k3,indices[[1]],indices[[3]]},{q-k1,indices[[2]],indices[[3]]}];
 tensor[route_,li_,lj_]:=FeynCalc`Pair[li,lj]-FeynCalc`Pair[li,FeynCalc`Momentum[route,D]] FeynCalc`Pair[lj,FeynCalc`Momentum[route,D]]/FeynCalc`SPD[route,route];
 diff=Cancel[Together[FeynCalc`ExpandScalarProduct[(norm/.replace)-pref (Times@@DeleteDuplicates[Cases[norm,_FeynCalc`FeynAmpDenominator,Infinity]]) (tensor@@pol1)(tensor@@pol2)]]];
 If[diff=!=0,Return[$Failed]];
 j=RGZWorkflow`Engine`rgzQQCurrent[mu,k3,k1] RGZWorkflow`Engine`rgzQQCurrent[mu,k4,k2];
 jq=FeynCalc`SpinorUBarD[k3,0].FeynCalc`GSD[q].FeynCalc`SpinorUD[k1,0];
 If[name==="Box1",a=FeynCalc`SPD[q,q];b=FeynCalc`SPD[q-k1+k3,q-k1+k3];prime=FeynCalc`SpinorUBarD[k4,0].FeynCalc`GSD[q].FeynCalc`SpinorUD[k2,0];metric+pref (j (1/a+1/b)-jq prime/(a b)),
 a=FeynCalc`SPD[q-k3,q-k3];b=FeynCalc`SPD[q-k1,q-k1];prime=FeynCalc`SpinorUBarD[k4,0].FeynCalc`GSD[q-k3].FeynCalc`SpinorUD[k2,0];metric+pref (-j (1/a+1/b)+jq prime/(a b))]];
