(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ============================================================*)


(* ============================================================*)ClearAll[rgzOrderRootName,rgzOrderResiduals,rgzOrderOne,rgzOrderResults,rgzOrderFailures,rgzOrderZeros,rgzOrderSummary,rgzOrderFailureTable,rgzOrderShow,rgzOrderTotal];


(*------------------------------------------------------------*)


(*------------------------------------------------------------*)

rgzOrderRootName[x_]:=Module[{h=Head[x]},While[Head[h]=!=Symbol,h=Head[h]];
SymbolName[h]];

rgzOrderResiduals[expr_]:=DeleteDuplicates@Cases[expr,x_/;MemberQ[{"C","G","TheC","AnalyticalCoupling","AnalyticalPropagator","FCFAConvert","FCPrepareFAAmp","FASUNF","FAFourVector","FAMetricTensor","FAScalarProduct","IndexSum"},rgzOrderRootName[x]],{0,Infinity}];


(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

rgzOrderOne[id_Integer,names_List]:=Module[{n,nIn,inFields,outFields,inserted,fa=Missing["NotGenerated"],amp=Missing["NotConverted"],lorentzPositions,lorentzNames,nInserted=0,nAmplitudes=0,messages={},leftovers={},finish},n=Length[names];
nIn=If[n==3,1,2];
finish[state_,success_:False]:=<|"ID"->id,"ExternalLegsCount"->n,"ExternalLegs"->names,"Incoming"->Take[names,nIn],"Outgoing"->Drop[names,nIn],"InsertionDiagramCount"->nInserted,"AmplitudeCount"->nAmplitudes,"Status"->state,"Generatable"->success,"ZeroAmplitude"->TrueQ[amp===0],"Messages"->messages,"UnresolvedTerm"->leftovers,"OriginalAmplitude"->fa,"FA"->amp|>;
inFields=rgzFAField[#,False]&/@Take[names,nIn];
outFields=rgzFAField[#,True]&/@Drop[names,nIn];
If[!FreeQ[{inFields,outFields},$Failed],Return[finish["UnknownFieldName"]]];
Block[{$MessageList={}},inserted=Quiet[Check[InsertFields[If[n==3,rgzTop3,rgzTop4],inFields->outFields,InsertionLevel->{Particles},Model->rgzModel,GenericModel->rgzModel],$Failed]];
messages=Join[messages,$MessageList];];
If[inserted===$Failed,Return[finish["InsertFields failed"]]];
nInserted=Total@Cases[inserted,HoldPattern[Insertions[Particles][graphs___]]:>Length[{graphs}],Infinity];
If[nInserted==0,Return[finish["no particle-level InsertionDiagram"]]];
Block[{$MessageList={}},fa=Quiet[Check[CreateFeynAmp[inserted,Truncated->True,GaugeRules->{},PreFactor->1],$Failed,{CreateFeynAmp::nocoupl,CreateFeynAmp::noprop}]];
messages=Join[messages,$MessageList];];
If[fa===$Failed,Return[finish["couplingOrPropagatorUnresolved"]]];
If[!MemberQ[{"FeynAmpList","FAFeynAmpList"},rgzOrderRootName[fa]],Return[finish["Notobtain AmplitudeList"]]];
nAmplitudes=Length[fa];
If[nAmplitudes==0,Return[finish["The amplitude list is empty"]]];
lorentzPositions=Flatten@Position[names,x_/;MemberQ[{"eta","etabar","A","ϕ","ϕbar","β","βbar","c","cbar","ω","ωbar","ζ","ζbar"},x],{1}];
lorentzNames=Join[Table[Symbol["RGZWorkflow`Engine`μ"<>ToString[j]],{j,lorentzPositions}],Table[Symbol["RGZWorkflow`Engine`rgzExtraLor"<>ToString[j]],{j,1,8}]];
Block[{$MessageList={}},amp=Quiet[Check[FCFAConvert[rgzPrepareFA[fa],IncomingMomenta->Take[rgzMom,nIn],OutgoingMomenta->Take[rgzMom,{nIn+1,n}],LorentzIndexNames->lorentzNames,ChangeDimension->D,List->False,DropSumOver->True,FinalSubstitutions->{Global`FAGS->SMP["g_s"],FCGV["MU"]->SMP["m_q"]}],$Failed]];
messages=Join[messages,$MessageList];];
amp=rgzVertexAmplitudeNormalize[amp,names];
If[amp===$Failed,Return[finish["FeynCalc conversion failed"]]];
leftovers=rgzOrderResiduals[amp];
If[leftovers=!={},Return[finish["RemainingUnresolvedTerm"]]];
If[TrueQ[amp===0],finish["Generatesuccessful; AmplitudeIsZero",True],finish["Generatesuccessful",True]]];


(*------------------------------------------------------------*)


(*------------------------------------------------------------*)
