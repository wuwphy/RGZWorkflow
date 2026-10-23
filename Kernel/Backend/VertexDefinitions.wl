(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ============================================================*)
(*RGZ\:ff1a\:68c0\:67e5\:5168\:90e8\:4e09\:70b9\:3001\:56db\:70b9\:63a5\:89e6\:9876\:70b9*)
(*\:03c9\:3001\:03b6 \:4f7f\:7528 U[2]\:3001U[3]*)
(*\:5bf9\:5e94\:5f53\:524d gen\:ff1aKinematicIndices[U]={Lorentz}*)
(*\:5728\:5df2\:52a0\:8f7d FeynCalc/FeynArts \:7684\:5185\:6838\:8fd0\:884c*)(* ============================================================*)ClearAll[rgzBase,rgzModel,rgzRawFile,rgzFindFile,rgzRaw,rgzIDs,rgzFRFC,rgzName,rgzLabel,rgzType,rgzP,rgzColor,rgzGribov,rgzLorentz,rgzDelta,rgzFRToFC,rgzFieldName,rgzDisplayField,rgzFAField,rgzDropIndexSums,rgzPrepareFA,rgzNormalizeFA,rgzNormalizeGribov,rgzReduce,rgzCheckOne,rgzShow,rgzTop3,rgzTop4,rgzMom,rgzHand,rgzResults,rgzSummary,rgzNotCompared,rgzMismatch];

Clear[p,k,l,r,p1,p2,p3,p4,rgzHandColor];


(*------------------------------------------------------------*)
(*0. \:6587\:4ef6\:8def\:5f84*)
(*------------------------------------------------------------*)

rgzBase=RGZWorkflow`Private`dataDirectory[];

rgzModel=RGZWorkflow`Private`modelBase[];

rgzFindFile[names_List]:=SelectFirst[FileNameJoin[{rgzBase,#}]&/@names,FileExistsQ,$Failed];

rgzRawFile=rgzFindFile[{"RGZ_all_raw.wxf","RGZ_all_raw(2).wxf","RGZ_all_raw(1).wxf"}];

If[rgzRawFile===$Failed||!AllTrue[{rgzModel<>".gen",rgzModel<>".mod"},FileExistsQ],Print["\:7f3a\:5c11 RGZ_W2.5.gen\:3001RGZ_W2.5.mod \:6216 
RGZ_all_raw.wxf\:3002"];
Abort[]];


(*------------------------------------------------------------*)
(*1. \:8bfb\:53d6 WXF\:ff0c\:53ea\:68c0\:67e5\:4e09\:70b9\:548c\:56db\:70b9\:9876\:70b9*)
(*------------------------------------------------------------*)

rgzRaw=Import[rgzRawFile,"WXF"];

If[!ListQ[rgzRaw],Print["WXF \:8bfb\:53d6\:5931\:8d25\:3002"];
Abort[]];

rgzIDs=Select[Range[Length[rgzRaw]],MemberQ[{3,4},Length[rgzRaw[[#,1]]]]&];

Print["\:539f\:59cb\:9876\:70b9\:603b\:6570\:ff1a",Length[rgzRaw]];

Print["\:5f85\:68c0\:67e5\:4e09\:70b9\:3001\:56db\:70b9\:6570\:91cf\:ff1a
",Tally[Length[rgzRaw[[#,1]]]&/@rgzIDs]];


(*------------------------------------------------------------*)
(*2. FeynRules WXF \:8868\:8fbe\:5f0f->FeynCalc*)
(*------------------------------------------------------------*)

rgzName[h_]:=If[Head[Unevaluated[h]]===Symbol,SymbolName[Unevaluated[h]],""];

rgzLabel[x_]:=Module[{y=x,name},If[rgzName[Head[y]]==="Index"&&Length[y]==2,y=y[[2]]];
If[rgzName[Head[y]]==="Ext"&&Length[y]>=1,y=y[[1]]];
If[Head[y]===Symbol,name=SymbolName[y];
If[StringMatchQ[name,"a"~~DigitCharacter..],y=ToExpression[StringDrop[name,1]]]];
y];

rgzType[x_]:=If[rgzName[Head[x]]==="Index",rgzName[x[[1]]],""];

rgzP[n_Integer]:=Symbol["RGZWorkflow`Engine`p"<>ToString[n]];

rgzColor[x_]:=Module[{y=rgzLabel[x]},If[IntegerQ[y],Symbol["RGZWorkflow`Engine`Glu"<>ToString[y]],y]];

rgzGribov[x_]:=Module[{y=rgzLabel[x]},If[IntegerQ[y],Symbol["RGZWorkflow`Engine`Gri"<>ToString[y]],y]];

rgzFundamental[x_]:=Module[{y=rgzLabel[x]},If[IntegerQ[y],Symbol["RGZWorkflow`Engine`Col"<>ToString[y]],y]];

rgzLorentz[x_]:=Module[{y=rgzLabel[x]},If[IntegerQ[y],Symbol["RGZWorkflow`Engine`\:03bc"<>ToString[y]],y]];

(*Gluon \:548c Gribov \:6307\:6807\:91c7\:7528\:4e0d\:540c\:6807\:7b7e\:3002*)

rgzDelta[x_,y_]:=Switch[{rgzType[x],rgzType[y]},{"Gluon","Gluon"},SUNDelta[rgzColor[x],rgzColor[y]],{"Gribov","Gribov"},SUNDelta[rgzGribov[x],rgzGribov[y]],{"Colour","Colour"},SUNFDelta[rgzFundamental[x],rgzFundamental[y]],_,$Failed];

rgzFRToFC[expr_]:=Module[{z,leftovers},z=expr/. {HoldPattern[h_[mu_,sp1_,sp2_]]/;rgzName[h]==="Ga"&&AllTrue[{sp1,sp2},IntegerQ[rgzLabel[#]]&&1<=rgzLabel[#]<=4&]:>GAD[rgzLorentz[mu]],HoldPattern[h_[a_,i_,j_]]/;rgzName[h]==="T":>SUNTF[{rgzColor[a]},rgzFundamental[i],rgzFundamental[j]],HoldPattern[h_[i_Integer,j_Integer]]/;rgzName[h]==="SP":>SPD[rgzP[i],rgzP[j]],HoldPattern[h_[i_Integer,mu_]]/;rgzName[h]==="FV":>FVD[rgzP[i],rgzLorentz[mu]],HoldPattern[h_[mu_,nu_]]/;rgzName[h]==="ME":>MTD[rgzLorentz[mu],rgzLorentz[nu]],HoldPattern[h_[x_,y_]]/;rgzName[h]==="IndexDelta":>rgzDelta[x,y],HoldPattern[h_[x_,y_,w_]]/;MemberQ[{"f","SUNF"},rgzName[h]]:>SUNF[rgzColor[x],rgzColor[y],rgzColor[w]]};
(*\:5bf9\:9f50\:53c2\:6570\:540d\:79f0\:3002*)z=z/. {s_Symbol/;rgzName[s]==="gs":>SMP["g_s"],s_Symbol/;rgzName[s]==="mg2":>Symbol["RGZWorkflow`Engine`mG"]^2,s_Symbol/;rgzName[s]==="m02":>Symbol["RGZWorkflow`Engine`m"]^2,s_Symbol/;rgzName[s]==="Nc":>N};
leftovers=DeleteDuplicates@Cases[z,x_/;MemberQ[{"SP","FV","ME","IndexDelta","Index","f","Ga","T"},rgzName[Head[x]]],{0,Infinity}];
If[!FreeQ[z,$Failed]||leftovers=!={},Return[$Failed]];
FCE[FCI[z]]];

rgzFRFC=Association@Table[id->rgzFRToFC[rgzRaw[[id,2]]],{id,rgzIDs}];

Print["FeynRules \:8f6c\:6362\:6210\:529f\:6570\:ff1a",Count[Values[rgzFRFC],Except[$Failed]]];


(*------------------------------------------------------------*)
(*3. \:573a\:540d\:79f0\:4e0e\:7c92\:5b50\:7c7b*)
(*------------------------------------------------------------*)

rgzFieldName[field_]:=StringReplace[rgzName[field],{"\:03c6"->"\:03d5"}];

(*\:5171\:8f6d\:573a\:663e\:793a\:4e3a\:4e0a\:6a2a\:7ebf\:3002*)

rgzDisplayField[name_String]:=Switch[name,"cbar",Overscript[Style["c",Italic],"_"],"\:03d5bar",Overscript[Style["\:03d5",Italic],"_"],"\:03b2bar",Overscript[Style["\:03b2",Italic],"_"],"\:03c9bar",Overscript[Style["\:03c9",Italic],"_"],"\:03b6bar",Overscript[Style["\:03b6",Italic],"_"],_,Style[name,Italic]];

(*outgoing=True \:65f6\:4f7f\:7528\:51fa\:5c04\:7c92\:5b50\:7684\:5171\:8f6d\:7c7b\:3002*)

rgzFAField[name_String,outgoing_]:=Switch[name,"eta",If[outgoing,-U[4],U[4]],"etabar",If[outgoing,U[4],-U[4]],"Q1",If[outgoing,-F[1],F[1]],"Q1bar",If[outgoing,F[1],-F[1]],"Q2",If[outgoing,-F[2],F[2]],"Q2bar",If[outgoing,F[2],-F[2]],"A",V[1],"\:03be",S[1],"\:03c4",S[2],"b",S[3],"c",If[outgoing,-U[1],U[1]],"cbar",If[outgoing,U[1],-U[1]],"\:03d5",If[outgoing,-V[2],V[2]],"\:03d5bar",If[outgoing,V[2],-V[2]],"\:03b2",If[outgoing,-V[3],V[3]],"\:03b2bar",If[outgoing,V[3],-V[3]],"\:03c9",If[outgoing,-U[2],U[2]],"\:03c9bar",If[outgoing,U[2],-U[2]],"\:03b6",If[outgoing,-U[3],U[3]],"\:03b6bar",If[outgoing,U[3],-U[3]],_,$Failed];


(*------------------------------------------------------------*)
(*4. \:72ec\:7acb\:8f93\:5165\:624b\:63a8\:516c\:5f0f*)
(*------------------------------------------------------------*)

(*\:4f7f\:7528 WXF \:539f\:59cb\:5916\:817f\:987a\:5e8f\:4ee5\:53ca\:5168\:6d41\:5165
\:52a8\:91cf\:3002*)
(*\:672a\:5f55\:5165\:7684\:9879\:4fdd\:6301 Missing\:3002*)

(* Match reference vertices by ordered fields, never by export row number. *)
rgzHand=Association@Cases[Table[With[{fields=rgzFieldName/@rgzRaw[[id,1,All,1]]},
 Which[fields==={"A","\:03be","\:03c4"},id->SMP["g_s"] SUNF[Glu1,Glu2,Glu3] FVD[p3,\:03bc1],
 fields==={"A","\:03be","\:03be","\:03c4"},id->-SMP["g_s"]^2/2 FVD[p4,\:03bc1](SUNF[Glu1,Glu3,rgzHandColor] SUNF[Glu2,Glu4,rgzHandColor]+SUNF[Glu1,Glu2,rgzHandColor] SUNF[Glu3,Glu4,rgzHandColor]),True,Nothing]],{id,rgzIDs}],_Rule];


(*------------------------------------------------------------*)
(*5. \:63a5\:89e6\:62d3\:6251\:4e0e FA \:6307\:6807\:8f6c\:6362*)
(*------------------------------------------------------------*)

rgzTop3=CreateTopologies[0,1->2,CTOrder->0,Adjacencies->3];

rgzTop4=CreateTopologies[0,2->2,CTOrder->0,Adjacencies->4];

rgzMom={p,k,l,r};

(*\:663e\:5f0f\:8272\:6c42\:548c\:7684\:7ed1\:5b9a\:6307\:6807\:6539\:4e3a
\:5404\:81ea\:72ec\:7acb\:7684 Einstein \:54d1\:6307\:6807\:3002*)

rgzDropIndexSums[expr_]:=expr//.(HoldPattern[h_[body_,specs__List]]/;rgzName[h]==="IndexSum":>(body/. Thread[(First/@{specs})->Table[Unique["rgzColorDummy"],{Length[{specs}]}]]));

(*\:5728 FCFAConvert \:524d\:5206\:522b\:5904\:7406 Gluon \:548c Gribov \:6307\:6807\:3002*)

rgzPrepareFA[expr_]:=rgzDropIndexSums[expr]/. {HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Colour":>SUNFIndex[rgzFundamental[h[type,i]]],HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Gluon":>SUNIndex[rgzColor[h[type,i]]],HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Gribov":>SUNIndex[rgzGribov[h[type,i]]]};

(*\:5904\:7406\:6b8b\:7559\:7684 Gribov \:5305\:88c5\:3002*)

rgzNormalizeGribov[expr_]:=expr/. {HoldPattern[SUNIndex[h_[type_,i_]]]/;rgzName[h]==="Index"&&rgzName[type]==="Gribov":>SUNIndex[rgzGribov[h[type,i]]],HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Gribov":>SUNIndex[rgzGribov[h[type,i]]]};

rgzNormalizeFA[expr_]:=expr/. (HoldPattern[h_[x_,y_]]/;rgzName[h]==="IndexDelta":>SUNDelta[x,y]);

rgzReduce[expr_]:=Module[{z},z=rgzNormalizeGribov[rgzNormalizeFA[expr]];
z=Expand[ExpandScalarProduct[FCI[z]]];
z=FCCanonicalizeDummyIndices[z,SUNIndexNames->Table[Symbol["RGZWorkflow`Engine`rgzD"<>ToString[j]],{j,1,24}]];
FullSimplify[SUNSimplify[z]]];


(*------------------------------------------------------------*)
(*6. \:5bf9\:6bcf\:4e2a\:9876\:70b9\:751f\:6210\:632f\:5e45\:5e76\:6bd4\:8f83
*)
(*------------------------------------------------------------*)

(* A single open fermion line carries the external-state permutation sign
   in CreateFeynAmp. Remove that sign for comparison to an all-incoming
   ordered functional derivative; the class coupling itself stays +I gs,
   independently confirmed by the native FeynRules FA exporter. *)
rgzVertexAmplitudeNormalize[amp_,names_] := If[AnyTrue[names,MemberQ[{"Q1","Q1bar","Q2","Q2bar"},#]&],
 -DiracSimplify[DotSimplify[DiracSubstitute67[amp]],DiracEquation->False,SirlinSimplify->False],amp];
rgzCheckOne[id_Integer]:=Module[{n,nIn,names,inFields,outFields,lorentzPositions,lorentzNames,inserted,fa,amp,fr,hand,rules,difference,handDifference,conservationRule,status,failure,nInserted=0,nAmplitudes=0,leftovers},n=Length[rgzRaw[[id,1]]];
nIn=If[n==3,1,2];
names=rgzFieldName/@rgzRaw[[id,1,All,1]];
failure[reason_]:=<|"\:7f16\:53f7"->id,"\:9636"->n,"\:5916\:817f"->names,"\:72b6\:6001"->reason,"\:63d2\:5165\:56fe\:6570"->nInserted,"\:632f\:5e45\:6570"->nAmplitudes,"FA"->Missing["\:672a\:751f\:6210"],"FR"->rgzFRFC[id],"FA-FR"->Missing["\:672a\:6bd4\:8f83"],"FA-\:624b\:63a8"->Missing["\:672a\:6bd4\:8f83"]|>;
If[rgzFRFC[id]===$Failed,Return[failure["FR \:683c\:5f0f\:8f6c\:6362\:5931\:8d25"]]];
inFields=rgzFAField[#,False]&/@Take[names,nIn];
outFields=rgzFAField[#,True]&/@Drop[names,nIn];
If[!FreeQ[{inFields,outFields},$Failed],Return[failure["\:672a\:8bc6\:522b\:7684\:573a\:540d"]]];
inserted=InsertFields[If[n==3,rgzTop3,rgzTop4],inFields->outFields,InsertionLevel->{Particles},Model->rgzModel,GenericModel->rgzModel];
nInserted=Length[inserted];
If[nInserted==0,Return[failure["\:65e0\:63d2\:5165\:56fe"]]];
fa=Check[CreateFeynAmp[inserted,Truncated->True,GaugeRules->{},PreFactor->1],$Failed,{CreateFeynAmp::nocoupl,CreateFeynAmp::noprop}];
If[fa===$Failed,Return[failure["\:8026\:5408\:6216\:4f20\:64ad\:5b50\:672a\:89e3\:6790"]]];
nAmplitudes=Length[fa];
If[nAmplitudes==0,Return[failure["\:632f\:5e45\:5217\:8868\:4e3a\:7a7a"]]];
(* ==========================================================*)(*\:5173\:952e\:4fee\:6539\:ff1aV \:548c U \:90fd\:5177\:6709 Lorentz \:8fd0\:52a8\:5b66\:6307\:6807\:3002*)(*\:5fc5\:987b\:6309\:539f\:59cb\:5916\:817f\:987a\:5e8f\:5206\:914d
\:5bf9\:5e94\:7684 \:03bcj\:3002*)(*c\:3001cbar \:867d\:7136\:662f\:6807\:91cf\:9b3c\:573a\:ff0c\:4e5f\:8981\:4fdd\:7559 U \:7c7b\:5206\:914d\:7684\:6307\:6807\:4f4d\:7f6e\:3002*)(* ==========================================================*)lorentzPositions=Flatten@Position[names,x_/;MemberQ[{"eta","etabar","A","\:03d5","\:03d5bar","\:03b2","\:03b2bar","c","cbar","\:03c9","\:03c9bar","\:03b6","\:03b6bar"},x],{1}];
lorentzNames=Join[Table[Symbol["RGZWorkflow`Engine`\:03bc"<>ToString[j]],{j,lorentzPositions}],Table[Symbol["RGZWorkflow`Engine`rgzExtraLor"<>ToString[j]],{j,1,8}]];
amp=FCFAConvert[rgzPrepareFA[fa],IncomingMomenta->Take[rgzMom,nIn],OutgoingMomenta->Take[rgzMom,{nIn+1,n}],LorentzIndexNames->lorentzNames,ChangeDimension->D,List->False,DropSumOver->True,FinalSubstitutions->{Global`FAGS->SMP["g_s"],FCGV["MU"]->SMP["m_q"]}];
amp=rgzVertexAmplitudeNormalize[amp,names];
leftovers=DeleteDuplicates@Cases[amp,x_/;MemberQ[{"FASUNF","FAFourVector","FAMetricTensor","FAScalarProduct","IndexSum","AnalyticalCoupling","AnalyticalPropagator"},rgzName[Head[x]]],{0,Infinity}];
If[Head[amp]===FCFAConvert||leftovers=!={},Return[failure["FA \:5f20\:91cf\:5c1a\:672a\:8f6c\:6362\:5b8c\:6210"]]];
(*FR \:9876\:70b9\:91c7\:7528\:5168\:6d41\:5165\:52a8\:91cf\:ff0c\:51fa\:5c04\:817f\:52a8\:91cf\:53d6\:8d1f\:3002*)rules=Table[rgzP[j]->If[j<=nIn,rgzMom[[j]],-rgzMom[[j]]],{j,n}];
fr=rgzFRFC[id]/. rules;
difference=rgzReduce[amp-fr];
conservationRule=If[n==3,l->p-k,r->p+k-l];
If[!TrueQ[difference===0],difference=rgzReduce[difference/. conservationRule]];
If[KeyExistsQ[rgzHand,id],hand=rgzHand[id]/. rules;
handDifference=rgzReduce[amp-hand];
If[!TrueQ[handDifference===0],handDifference=rgzReduce[handDifference/. conservationRule]],handDifference=Missing["\:672a\:5f55\:5165\:624b\:63a8"]];
status=Which[TrueQ[difference===0]&&TrueQ[handDifference===0],"\:4e09\:65b9\:4e00\:81f4",TrueQ[difference===0]&&Head[handDifference]===Missing,"FA=FR\:ff1b\:672a\:5f55\:5165\:624b\:63a8",TrueQ[difference===0],"FA=FR\:ff1b\:624b\:63a8\:4e0d\:4e00\:81f4",True,"FA \:4e0e FR \:4e0d\:4e00\:81f4"];
<|"\:7f16\:53f7"->id,"\:9636"->n,"\:5916\:817f"->names,"\:72b6\:6001"->status,"\:63d2\:5165\:56fe\:6570"->nInserted,"\:632f\:5e45\:6570"->nAmplitudes,"FA"->amp,"FR"->fr,"FA-FR"->difference,"FA-\:624b\:63a8"->handDifference|>];


(*------------------------------------------------------------*)
(*7. \:91cd\:65b0\:8fd0\:884c\:5168\:90e8\:4e09\:70b9\:3001\:56db\:70b9\:68c0\:67e5*)
(*------------------------------------------------------------*)

rgzShow[id_Integer]:=Module[{item},item=SelectFirst[rgzResults,#["\:7f16\:53f7"]==id&,Missing["\:6ca1\:6709\:8fd9\:4e2a\:7f16\:53f7"]];
If[Head[item]===Missing,Print[item];
Return[$Failed]];
Print["\:7f16\:53f7\:ff1a",id,"\:ff1b\:5916\:817f\:ff1a",Row[rgzDisplayField/@item["\:5916\:817f"],", "]];
Print["\:72b6\:6001\:ff1a",item["\:72b6\:6001"]];
Print["FeynArts\:ff1a",TraditionalForm[item["FA"]]];
Print["FeynRules\:ff1a",TraditionalForm[item["FR"]]];
Print["FA-FR\:ff1a",TraditionalForm[item["FA-FR"]]];
Print["FA-\:624b\:63a8\:ff1a",TraditionalForm[item["FA-\:624b\:63a8"]]];];

(*\:81ea\:52a8\:663e\:793a\:4ecd\:6709\:5dee\:5f02\:7684\:9879\:3002*)

