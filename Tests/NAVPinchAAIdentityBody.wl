(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
aaa=First[Select[RGZWorkflow`RGZResult["NAV"]["Connections"],#["Fields"]==={"A","A","A"}&]]["TransverseTensor"];
split=rgzNAVWardSplit[aaa];Print[If[AssociationQ[split],split["ExactPolynomialReconstruction"],split]];
If[FailureQ[split],Exit[1]];
fromFull=Contract[FCI[I SMP["g_s"] SUNN (split["PinchTensor"]/.q->-q)] FCI[RGZLoopPT[p,\[Mu],rgzNAVPinchMu]]]/.rgzNAVNu->\[Nu];
old=RGZBuildPinches["Representation"->"Rational"]["nav全部"]/.CA->SUNN;
scalar=FCE[Contract[FCI[RGZLoopPT[p,\[Mu],\[Nu]]] (fromFull-old)]]/(D-1);
scalar=RGZLoopNormalize[ExpandScalarProduct[scalar]];
sample=Cancel[Together[scalar/.{Pair[Momentum[p,___],Momentum[p,___]]->19,Pair[Momentum[q,___],Momentum[q,___]]->7/4,Pair[Momentum[p,___],Momentum[q,___]]->21/2,Pair[Momentum[q,___],Momentum[p,___]]->21/2,m->1,\[Mu]->2,mG->3,M->1,\[Alpha]->2,SUNN->3,D->4,SMP["g_s"]->1}]];
Print["AA old-vs-new sample = ",sample];
Do[qc=Cancel[Together[scalar/.{Pair[Momentum[p,___],Momentum[p,___]]->19,Pair[Momentum[q,___],Momentum[q,___]]->7/4,Pair[Momentum[p,___],Momentum[q,___]]->21/2,Pair[Momentum[q,___],Momentum[p,___]]->21/2,m->0,\[Mu]->0,mG->0,M->0,\[Alpha]->av,SUNN->3,D->4,SMP["g_s"]->1}]];Print["QCD alpha ",av," difference = ",qc],{av,{0,1,2}}];
qs=Factor[Cancel[Together[scalar/.{m->0,\[Mu]->0,mG->0,M->0,SUNN->1,SMP["g_s"]->1}/.{Pair[Momentum[p,___],Momentum[p,___]]->ss,Pair[Momentum[q,___],Momentum[q,___]]->uu,Pair[Momentum[p,___],Momentum[q,___]]->(ss+uu-vv)/2,Pair[Momentum[q,___],Momentum[p,___]]->(ss+uu-vv)/2}]]];
Print["QCD invariant difference = ",qs];
Print["QCD apart difference = ",Apart[Apart[qs,uu],vv]];
rs=scalar/.{Pair[Momentum[p,___],Momentum[p,___]]->ss,Pair[Momentum[q,___],Momentum[q,___]]->uu,Pair[Momentum[p,___],Momentum[q,___]]->(ss+uu-vv)/2,Pair[Momentum[q,___],Momentum[p,___]]->(ss+uu-vv)/2};
symDiff=TimeConstrained[Cancel[Together[rs+(rs/.{uu->vv,vv->uu})]],120,$TimedOut];
Print["Full RGZ symmetrized AA difference = ",symDiff];
Export["/tmp/nav-pinch-AA-difference.wxf",scalar,"WXF"];
Exit[If[TrueQ[symDiff===0]&&TrueQ[split["ExactPolynomialReconstruction"]],0,1]];
