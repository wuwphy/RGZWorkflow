(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(*============================================================*)
(*\:5168\:90e8\:5916\:817f\:7f6e\:6362\:7684\:8868\:8fbe\:5f0f\:68c0\:67e5\:ff1a\:590d\:7528 rgzOrderResults\:ff0c\:4e0d\:91cd\:65b0\:751f\:6210\:56fe*)
(*\:540c\:65f6\:7f6e\:6362\:52a8\:91cf\:3001Lorentz\:3001Gluon\:3001Gribov \:6307\:6807*)
(*============================================================*)ClearAll[rgzPermCompare,rgzPermResults,rgzPermSummary,rgzPermFailures,rgzPermShow,rgzPermFormat];

If[!ListQ[rgzOrderResults]||!AssociationQ[rgzFRFC],Print["\:8bf7\:5148\:8fd0\:884c\:539f\:59cb FA/FR \:68c0\:67e5\:548c
\:5168\:90e8\:6392\:5e8f\:751f\:6210\:68c0\:67e5\:3002"];
Abort[]];

(*\:6bcf\:6b21\:8868\:8fbe\:5f0f\:6bd4\:8f83\:7684\:6700\:957f\:65f6\:95f4
\:ff0c\:53ef\:81ea\:884c\:589e\:5927*)
rgzPermTimeLimit=60;

rgzPermCompare[id_Integer,permutation_List]:=Module[{original,names,n,nIn,oldToNew,ghostLegs,ghostSign,item,rules,fa,fr,difference,conservationRule,finish},original=rgzFieldName/@rgzRaw[[id,1,All,1]];
n=Length[original];
nIn=If[n==3,1,2];
(*permutation[[j]]\:ff1a\:5f53\:524d\:7b2c j \:6761\:817f\:6765\:81ea\:539f\:59cb\:7b2c\:51e0\:6761\:817f*)names=original[[permutation]];
(*oldToNew[[i]]\:ff1a\:539f\:59cb\:7b2c i \:6761\:817f\:73b0\:5728\:5904\:4e8e\:7b2c\:51e0\:4f4d*)oldToNew=Ordering[permutation];
ghostLegs=Select[Range[n],MemberQ[{"eta","etabar","Q1","Q1bar","Q2","Q2bar","c","cbar","\:03c9","\:03c9bar","\:03b6","\:03b6bar"},original[[#]]]&];
(*\:53ea\:8ba1\:7b97 Grassmann \:573a\:4e4b\:95f4\:7684\:4ea4\:6362\:7b26\:53f7*)ghostSign=If[Length[ghostLegs]<2,1,Signature[oldToNew[[ghostLegs]]]];
item=SelectFirst[rgzOrderResults,#["\:7f16\:53f7"]===id&&#["\:5916\:817f"]===names&,Missing["\:672a\:627e\:5230\:751f\:6210\:7ed3\:679c"]];
fa=Missing["\:672a\:751f\:6210"];
fr=Missing["\:672a\:8f6c\:6362"];
finish[state_,diff_:Missing["\:672a\:6bd4\:8f83"]]:=<|"\:7f16\:53f7"->id,"\:817f\:6392\:5217"->permutation,"\:5916\:817f"->names,"\:9b3c\:573a\:4ea4\:6362\:7b26\:53f7"->ghostSign,"FA"->fa,"FR"->fr,"FA-FR"->diff,"\:72b6\:6001"->state|>;
If[Head[item]===Missing,Return[finish["\:672a\:6bd4\:8f83"]]];
If[!TrueQ[Lookup[item,"\:53ef\:751f\:6210",False]],Return[finish["\:672a\:6bd4\:8f83"]]];
If[Lookup[rgzFRFC,id,$Failed]===$Failed,Return[finish["\:672a\:6bd4\:8f83"]]];
fa=item["FA"];
(*\:4e00\:6b21\:6027\:66ff\:6362\:ff0c\:907f\:514d\:6307\:6807\:4ea4\:6362\:65f6\:53d1\:751f\:8fde\:7eed
\:8986\:76d6*)rules=Flatten[Table[With[{j=oldToNew[[i]]},{rgzP[i]->If[j<=nIn,rgzMom[[j]],-rgzMom[[j]]],Symbol["RGZWorkflow`Engine`\:03bc"<>ToString[i]]->Symbol["RGZWorkflow`Engine`\:03bc"<>ToString[j]],Symbol["RGZWorkflow`Engine`Glu"<>ToString[i]]->Symbol["RGZWorkflow`Engine`Glu"<>ToString[j]],Symbol["RGZWorkflow`Engine`Gri"<>ToString[i]]->Symbol["RGZWorkflow`Engine`Gri"<>ToString[j]],Symbol["RGZWorkflow`Engine`Col"<>ToString[i]]->Symbol["RGZWorkflow`Engine`Col"<>ToString[j]]}],{i,n}],1];
fr=ghostSign (rgzFRFC[id]/. rules);
(*\:6700\:540e\:4e00\:6761\:51fa\:5c04\:817f\:7531\:52a8\:91cf
\:5b88\:6052\:6d88\:53bb*)conservationRule=rgzMom[[n]]->Total[Take[rgzMom,nIn]]-Total[Take[rgzMom,{nIn+1,n-1}]];
difference=TimeConstrained[Quiet[Check[Module[{z},z=rgzReduce[fa-fr];
If[!TrueQ[z===0],z=rgzReduce[z/. conservationRule]];
z],$Failed]],rgzPermTimeLimit,$Failed];
Which[difference===$Failed,finish["\:8ba1\:7b97\:5931\:8d25\:6216\:8d85\:65f6",$Failed],TrueQ[difference===0],finish["\:901a\:8fc7",0],True,finish["\:5dee\:503c\:672a\:5f52\:96f6",difference]]];

(*============================================================*)
(*\:4e09\:70b9\:9876\:70b9\:68c0\:67e5 3!\:ff0c\:56db\:70b9\:9876\:70b9\:68c0\:67e5 4!\:ff1b\:5171 630 \:9879*)
(*============================================================*)

