(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ============================================================*)
(*RGZ\:ff1a\:68c0\:67e5\:5168\:90e8\:4e09\:70b9\:3001\:56db\:70b9\:9876\:70b9\:7684\:4e0d\:540c\:5916\:817f\:987a\:5e8f*)
(*\:524d\:63d0\:ff1a\:5df2\:7ecf\:8fd0\:884c\:539f\:68c0\:67e5\:4ee3\:7801\:7684\:7b2c 0\:ff5e5 \:6bb5*)
(* ============================================================*)ClearAll[rgzOrderRootName,rgzOrderResiduals,rgzOrderOne,rgzOrderResults,rgzOrderFailures,rgzOrderZeros,rgzOrderSummary,rgzOrderFailureTable,rgzOrderShow,rgzOrderTotal];


(*------------------------------------------------------------*)
(*1. \:68c0\:67e5\:662f\:5426\:6709\:5c1a\:672a\:89e3\:6790\:7684\:8026\:5408
\:6216\:5f20\:91cf*)
(*\:540c\:65f6\:8bc6\:522b C[...][...][...] \:8fd9\:79cd\:591a\:5c42\:7ed3\:6784*)
(*------------------------------------------------------------*)

rgzOrderRootName[x_]:=Module[{h=Head[x]},While[Head[h]=!=Symbol,h=Head[h]];
SymbolName[h]];

rgzOrderResiduals[expr_]:=DeleteDuplicates@Cases[expr,x_/;MemberQ[{"C","G","TheC","AnalyticalCoupling","AnalyticalPropagator","FCFAConvert","FCPrepareFAAmp","FASUNF","FAFourVector","FAMetricTensor","FAScalarProduct","IndexSum"},rgzOrderRootName[x]],{0,Infinity}];


(*------------------------------------------------------------*)
(*2. \:68c0\:67e5\:67d0\:4e2a\:9876\:70b9\:7684\:4e00\:79cd\:5916\:817f\:987a
\:5e8f*)
(*------------------------------------------------------------*)

rgzOrderOne[id_Integer,names_List]:=Module[{n,nIn,inFields,outFields,inserted,fa=Missing["\:672a\:751f\:6210"],amp=Missing["\:672a\:8f6c\:6362"],lorentzPositions,lorentzNames,nInserted=0,nAmplitudes=0,messages={},leftovers={},finish},n=Length[names];
nIn=If[n==3,1,2];
finish[state_,success_:False]:=<|"\:7f16\:53f7"->id,"\:5916\:817f\:6570"->n,"\:5916\:817f"->names,"\:5165\:5c04"->Take[names,nIn],"\:51fa\:5c04"->Drop[names,nIn],"\:63d2\:5165\:56fe\:6570"->nInserted,"\:632f\:5e45\:6570"->nAmplitudes,"\:72b6\:6001"->state,"\:53ef\:751f\:6210"->success,"\:96f6\:632f\:5e45"->TrueQ[amp===0],"\:6d88\:606f"->messages,"\:672a\:89e3\:6790\:9879"->leftovers,"\:539f\:59cb\:632f\:5e45"->fa,"FA"->amp|>;
(*\:51fa\:5c04\:590d\:573a\:4f7f\:7528\:5bf9\:5e94\:5171\:8f6d\:7c92
\:5b50\:7c7b\:3002*)inFields=rgzFAField[#,False]&/@Take[names,nIn];
outFields=rgzFAField[#,True]&/@Drop[names,nIn];
If[!FreeQ[{inFields,outFields},$Failed],Return[finish["\:672a\:8bc6\:522b\:7684\:573a\:540d"]]];
(*\:63d2\:5165\:56fe\:3002\:4fdd\:7559\:6d88\:606f\:ff0c\:907f\:514d\:5927\:91cf\:62a5\:9519\:5237\:5c4f\:3002*)Block[{$MessageList={}},inserted=Quiet[Check[InsertFields[If[n==3,rgzTop3,rgzTop4],inFields->outFields,InsertionLevel->{Particles},Model->rgzModel,GenericModel->rgzModel],$Failed]];
messages=Join[messages,$MessageList];];
If[inserted===$Failed,Return[finish["InsertFields \:62a5\:9519"]]];
(*\:7edf\:8ba1\:7c92\:5b50\:5c42\:7684\:5b9e\:9645\:63d2\:5165\:56fe\:3002*)nInserted=Total@Cases[inserted,HoldPattern[Insertions[Particles][graphs___]]:>Length[{graphs}],Infinity];
If[nInserted==0,Return[finish["\:65e0\:7c92\:5b50\:5c42\:63d2\:5165\:56fe"]]];
(*\:751f\:6210 FeynArts \:632f\:5e45\:3002*)Block[{$MessageList={}},fa=Quiet[Check[CreateFeynAmp[inserted,Truncated->True,GaugeRules->{},PreFactor->1],$Failed,{CreateFeynAmp::nocoupl,CreateFeynAmp::noprop}]];
messages=Join[messages,$MessageList];];
If[fa===$Failed,Return[finish["\:8026\:5408\:6216\:4f20\:64ad\:5b50\:672a\:89e3\:6790"]]];
(*\:517c\:5bb9\:539f\:59cb FeynArts \:548c FeynCalc \:6539\:540d\:540e\:7684\:632f\:5e45\:5217\:8868\:3002*)If[!MemberQ[{"FeynAmpList","FAFeynAmpList"},rgzOrderRootName[fa]],Return[finish["\:672a\:5f97\:5230\:632f\:5e45\:5217\:8868"]]];
nAmplitudes=Length[fa];
If[nAmplitudes==0,Return[finish["\:632f\:5e45\:5217\:8868\:4e3a\:7a7a"]]];
(*V\:3001U \:7684 Lorentz \:6307\:6807\:6309\:5f53\:524d\:5916\:817f\:987a\:5e8f\:6392\:5217
\:3002*)lorentzPositions=Flatten@Position[names,x_/;MemberQ[{"eta","etabar","A","\:03d5","\:03d5bar","\:03b2","\:03b2bar","c","cbar","\:03c9","\:03c9bar","\:03b6","\:03b6bar"},x],{1}];
lorentzNames=Join[Table[Symbol["RGZWorkflow`Engine`\:03bc"<>ToString[j]],{j,lorentzPositions}],Table[Symbol["RGZWorkflow`Engine`rgzExtraLor"<>ToString[j]],{j,1,8}]];
(*\:8f6c\:6362\:540e\:7ee7\:7eed\:68c0\:67e5\:ff0c\:9632\:6b62\:628a\:672a\:89e3\:6790\:7684\:5f62\:5f0f\:5f53\:6210
\:751f\:6210\:6210\:529f\:3002*)Block[{$MessageList={}},amp=Quiet[Check[FCFAConvert[rgzPrepareFA[fa],IncomingMomenta->Take[rgzMom,nIn],OutgoingMomenta->Take[rgzMom,{nIn+1,n}],LorentzIndexNames->lorentzNames,ChangeDimension->D,List->False,DropSumOver->True,FinalSubstitutions->{Global`FAGS->SMP["g_s"],FCGV["MU"]->SMP["m_q"]}],$Failed]];
messages=Join[messages,$MessageList];];
amp=rgzVertexAmplitudeNormalize[amp,names];
If[amp===$Failed,Return[finish["FeynCalc \:8f6c\:6362\:62a5\:9519"]]];
leftovers=rgzOrderResiduals[amp];
If[leftovers=!={},Return[finish["\:4ecd\:6709\:672a\:89e3\:6790\:9879"]]];
If[TrueQ[amp===0],finish["\:751f\:6210\:6210\:529f\:ff1b\:632f\:5e45\:4e3a\:96f6",True],finish["\:751f\:6210\:6210\:529f",True]]];


(*------------------------------------------------------------*)
(*3. \:904d\:5386\:6bcf\:4e2a\:9876\:70b9\:7684\:5168\:90e8\:4e0d\:540c\:573a
\:987a\:5e8f*)
(*Permutations \:81ea\:52a8\:53bb\:6389\:76f8\:540c\:573a\:9020\:6210\:7684\:91cd\:590d
\:6392\:5217*)
(*------------------------------------------------------------*)

