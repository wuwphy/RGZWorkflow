(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(*============================================================*)


(*============================================================*)ClearAll[rgzPermCompare,rgzPermResults,rgzPermSummary,rgzPermFailures,rgzPermShow,rgzPermFormat];

If[!ListQ[rgzOrderResults]||!AssociationQ[rgzFRFC],Print["Run the original FA/FR checks and all ordering-generation checks first."];
Abort[]];


rgzPermTimeLimit=60;

rgzPermCompare[id_Integer,permutation_List]:=Module[{original,names,n,nIn,oldToNew,ghostLegs,ghostSign,item,rules,fa,fr,difference,conservationRule,finish},original=rgzFieldName/@rgzRaw[[id,1,All,1]];
n=Length[original];
nIn=If[n==3,1,2];
names=original[[permutation]];
oldToNew=Ordering[permutation];
ghostLegs=Select[Range[n],MemberQ[{"eta","etabar","Q1","Q1bar","Q2","Q2bar","c","cbar","ω","ωbar","ζ","ζbar"},original[[#]]]&];
ghostSign=If[Length[ghostLegs]<2,1,Signature[oldToNew[[ghostLegs]]]];
item=SelectFirst[rgzOrderResults,#["ID"]===id&&#["ExternalLegs"]===names&,Missing["No generated result found"]];
fa=Missing["NotGenerated"];
fr=Missing["NotConverted"];
finish[state_,diff_:Missing["NotCompared"]]:=<|"ID"->id,"LegPermutation"->permutation,"ExternalLegs"->names,"GhostExchangeSymbol"->ghostSign,"FA"->fa,"FR"->fr,"FA-FR"->diff,"Status"->state|>;
If[Head[item]===Missing,Return[finish["NotCompared"]]];
If[!TrueQ[Lookup[item,"Generatable",False]],Return[finish["NotCompared"]]];
If[Lookup[rgzFRFC,id,$Failed]===$Failed,Return[finish["NotCompared"]]];
fa=item["FA"];
rules=Flatten[Table[With[{j=oldToNew[[i]]},{rgzP[i]->If[j<=nIn,rgzMom[[j]],-rgzMom[[j]]],Symbol["RGZWorkflow`Engine`μ"<>ToString[i]]->Symbol["RGZWorkflow`Engine`μ"<>ToString[j]],Symbol["RGZWorkflow`Engine`Glu"<>ToString[i]]->Symbol["RGZWorkflow`Engine`Glu"<>ToString[j]],Symbol["RGZWorkflow`Engine`Gri"<>ToString[i]]->Symbol["RGZWorkflow`Engine`Gri"<>ToString[j]],Symbol["RGZWorkflow`Engine`Col"<>ToString[i]]->Symbol["RGZWorkflow`Engine`Col"<>ToString[j]]}],{i,n}],1];
fr=ghostSign (rgzFRFC[id]/. rules);
conservationRule=rgzMom[[n]]->Total[Take[rgzMom,nIn]]-Total[Take[rgzMom,{nIn+1,n-1}]];
difference=TimeConstrained[Quiet[Check[Module[{z},z=rgzReduce[fa-fr];
If[!TrueQ[z===0],z=rgzReduce[z/. conservationRule]];
z],$Failed]],rgzPermTimeLimit,$Failed];
Which[difference===$Failed,finish["ComputeFailedOrTimeout",$Failed],TrueQ[difference===0],finish["Passed",0],True,finish["DifferenceNonzero",difference]]];

(*============================================================*)

(*============================================================*)
