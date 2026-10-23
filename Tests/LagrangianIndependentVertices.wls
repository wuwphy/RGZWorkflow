(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
testWork=Environment["RGZ_TEST_WORK"];
If[!StringQ[testWork]||testWork==="",testWork=FileNameJoin[{$TemporaryDirectory,"RGZWorkflow-integration-0.3.0"}]];
RGZInitialize[testWork];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
vertexLoad=RGZWorkflow`Private`loadVertexEngine[60];If[FailureQ[vertexLoad],Print[vertexLoad];Exit[2]];
RGZWorkflow`Private`engineCode["
(* Historical formula labels are resolved by ordered fields after new exports. *)\nindReferenceFields=<|1->{\"cbar\",\"c\",\"A\"},2->{\"A\",\"A\",\"A\"},3->{\"A\",\"A\",\"A\",\"A\"},\n4->{\"A\",\"\\[Beta]\",\"\\[Beta]bar\"},31->{\"A\",\"\\:03d5\",\"\\:03d5bar\"},\n6->{\"\\[Zeta]bar\",\"\\[Zeta]\",\"A\"},39->{\"\\[Omega]bar\",\"\\[Omega]\",\"A\"},\n18->{\"A\",\"\\[Beta]\",\"\\[Xi]\"},19->{\"A\",\"\\[Beta]bar\",\"\\[Xi]\"},28->{\"A\",\"\\[Xi]\",\"\\:03d5\"},36->{\"A\",\"\\[Xi]\",\"\\:03d5bar\"},\n5->{\"\\[Beta]\",\"\\[Beta]bar\",\"\\[Xi]\"},32->{\"\\[Xi]\",\"\\:03d5\",\"\\:03d5bar\"},15->{\"A\",\"\\[Xi]\",\"\\[Tau]\"},21->{\"A\",\"\\[Xi]\",\"\\[Xi]\",\"\\[Tau]\"}|>;\nindCurrentID[id_]:=SelectFirst[rgzIDs,(rgzFieldName/@rgzRaw[[#,1,All,1]])===indReferenceFields[id]&,Missing[\"ReferenceFieldsAbsent\"]];\nindChecks=<||>;
indCheck[id_,expr_]:=Module[{difference},difference=rgzReduce[(rgzFRFC[indCurrentID[id]]/.HoldPattern[System`N]->SUNN)-expr];AssociateTo[indChecks,id->TrueQ[difference===0]];Print[\"Independent vertex \",id,\": \",indChecks[id]]];
gCoupling=SMP[\"g_s\"];
(* Ordered Grassmann legs are bar,c; the left derivative fixes their sign. *)
indCheck[1,-gCoupling SUNF[Glu1,Glu2,Glu3] FVD[p1,\\:03bc3]];
indCheck[2,gCoupling SUNF[Glu1,Glu2,Glu3] (MTD[\\:03bc1,\\:03bc2] FVD[p1-p2,\\:03bc3]+MTD[\\:03bc1,\\:03bc3] FVD[p3-p1,\\:03bc2]+MTD[\\:03bc2,\\:03bc3] FVD[p2-p3,\\:03bc1])];
indCheck[3,-I gCoupling^2 (SUNF[Glu1,Glu2,indQuarticDummy] SUNF[Glu3,Glu4,indQuarticDummy] (MTD[\\:03bc1,\\:03bc3] MTD[\\:03bc2,\\:03bc4]-MTD[\\:03bc1,\\:03bc4] MTD[\\:03bc2,\\:03bc3])+SUNF[Glu1,Glu3,indQuarticDummy] SUNF[Glu2,Glu4,indQuarticDummy] (MTD[\\:03bc1,\\:03bc2] MTD[\\:03bc3,\\:03bc4]-MTD[\\:03bc1,\\:03bc4] MTD[\\:03bc2,\\:03bc3])+SUNF[Glu1,Glu4,indQuarticDummy] SUNF[Glu2,Glu3,indQuarticDummy] (MTD[\\:03bc1,\\:03bc2] MTD[\\:03bc3,\\:03bc4]-MTD[\\:03bc1,\\:03bc3] MTD[\\:03bc2,\\:03bc4]))];
Do[indCheck[id,gCoupling SUNF[Glu1,Glu2,Glu3] FVD[p3,\\:03bc1] MTD[\\:03bc2,\\:03bc3] SUNDelta[Gri2,Gri3]],{id,{4,31}}];
(* Lomega/Lzeta carry the opposite kinetic sign; differentiating the
   two ordered odd fields supplies the additional Grassmann sign. *)
Do[indCheck[id,gCoupling SUNF[Glu1,Glu2,Glu3] FVD[p1,\\:03bc3] MTD[\\:03bc1,\\:03bc2] SUNDelta[Gri1,Gri2]],{id,{6,39}}];
Do[auxLeg=If[id<20,2,3];xiLeg=5-auxLeg;familySign=If[id<20,-1,1];
 auxGlu=Symbol[\"RGZWorkflow`Engine`Glu\"<>ToString[auxLeg]];
 auxGri=Symbol[\"RGZWorkflow`Engine`Gri\"<>ToString[auxLeg]];
 xiGlu=Symbol[\"RGZWorkflow`Engine`Glu\"<>ToString[xiLeg]];
 auxLor=Symbol[\"RGZWorkflow`Engine`\\:03bc\"<>ToString[auxLeg]];
 indCheck[id,-I familySign gCoupling mG^2/(2 Sqrt[SUNN]) SUNF[indHorizonDummy,auxGlu,auxGri] SUNF[indHorizonDummy,Glu1,xiGlu] MTD[\\:03bc1,auxLor]],{id,{18,19,28,36}}];
(* del -> -I p and Ah_linear=A-del[xi] fix the relative kinetic signs. *)
indCheck[5,I gCoupling SUNF[Glu3,Glu1,Glu2] SPD[p3,p2] MTD[\\:03bc1,\\:03bc2] SUNDelta[Gri1,Gri2]];
indCheck[32,I gCoupling SUNF[Glu1,Glu2,Glu3] SPD[p1,p3] MTD[\\:03bc2,\\:03bc3] SUNDelta[Gri2,Gri3]];
Do[indCheck[id,rgzHand[indCurrentID[id]]],{id,{15,21}}];
Print[<|\"Passed\"->Count[Values[indChecks],True],\"Failed\"->Count[Values[indChecks],False]|>];
Exit[If[And@@Values[indChecks],0,1]];"];
