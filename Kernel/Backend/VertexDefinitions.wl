(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ============================================================*)



(* ============================================================*)ClearAll[rgzBase,rgzModel,rgzRawFile,rgzFindFile,rgzRaw,rgzIDs,rgzFRFC,rgzName,rgzLabel,rgzType,rgzP,rgzColor,rgzGribov,rgzLorentz,rgzDelta,rgzFRToFC,rgzFieldName,rgzDisplayField,rgzFAField,rgzDropIndexSums,rgzPrepareFA,rgzNormalizeFA,rgzNormalizeGribov,rgzReduce,rgzCheckOne,rgzShow,rgzTop3,rgzTop4,rgzMom,rgzHand,rgzResults,rgzSummary,rgzNotCompared,rgzMismatch];

Clear[p,k,l,r,p1,p2,p3,p4,rgzHandColor];


(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

rgzBase=RGZWorkflow`Private`dataDirectory[];

rgzModel=RGZWorkflow`Private`modelBase[];

rgzFindFile[names_List]:=SelectFirst[FileNameJoin[{rgzBase,#}]&/@names,FileExistsQ,$Failed];

rgzRawFile=rgzFindFile[{"RGZ_all_raw.wxf","RGZ_all_raw(2).wxf","RGZ_all_raw(1).wxf"}];

If[rgzRawFile===$Failed||!AllTrue[{rgzModel<>".gen",rgzModel<>".mod"},FileExistsQ],Print["Missing RGZ_W2.5.gen, RGZ_W2.5.mod Or
RGZ_all_raw.wxf. "];
Abort[]];


(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

rgzRaw=Import[rgzRawFile,"WXF"];

If[!ListQ[rgzRaw],Print["WXF ReadFailed. "];
Abort[]];

rgzIDs=Select[Range[Length[rgzRaw]],MemberQ[{3,4},Length[rgzRaw[[#,1]]]]&];

Print["OriginalVertexTotalCount: ",Length[rgzRaw]];

Print["pending CheckthreePoint, fourPointCountvalue:
",Tally[Length[rgzRaw[[#,1]]]&/@rgzIDs]];


(*------------------------------------------------------------*)

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

rgzLorentz[x_]:=Module[{y=rgzLabel[x]},If[IntegerQ[y],Symbol["RGZWorkflow`Engine`μ"<>ToString[y]],y]];



rgzDelta[x_,y_]:=Switch[{rgzType[x],rgzType[y]},{"Gluon","Gluon"},SUNDelta[rgzColor[x],rgzColor[y]],{"Gribov","Gribov"},SUNDelta[rgzGribov[x],rgzGribov[y]],{"Colour","Colour"},SUNFDelta[rgzFundamental[x],rgzFundamental[y]],_,$Failed];

rgzFRToFC[expr_]:=Module[{z,leftovers},z=expr/. {HoldPattern[h_[mu_,sp1_,sp2_]]/;rgzName[h]==="Ga"&&AllTrue[{sp1,sp2},IntegerQ[rgzLabel[#]]&&1<=rgzLabel[#]<=4&]:>GAD[rgzLorentz[mu]],HoldPattern[h_[a_,i_,j_]]/;rgzName[h]==="T":>SUNTF[{rgzColor[a]},rgzFundamental[i],rgzFundamental[j]],HoldPattern[h_[i_Integer,j_Integer]]/;rgzName[h]==="SP":>SPD[rgzP[i],rgzP[j]],HoldPattern[h_[i_Integer,mu_]]/;rgzName[h]==="FV":>FVD[rgzP[i],rgzLorentz[mu]],HoldPattern[h_[mu_,nu_]]/;rgzName[h]==="ME":>MTD[rgzLorentz[mu],rgzLorentz[nu]],HoldPattern[h_[x_,y_]]/;rgzName[h]==="IndexDelta":>rgzDelta[x,y],HoldPattern[h_[x_,y_,w_]]/;MemberQ[{"f","SUNF"},rgzName[h]]:>SUNF[rgzColor[x],rgzColor[y],rgzColor[w]]};
z=z/. {s_Symbol/;rgzName[s]==="gs":>SMP["g_s"],s_Symbol/;rgzName[s]==="mg2":>Symbol["RGZWorkflow`Engine`mG"]^2,s_Symbol/;rgzName[s]==="m02":>Symbol["RGZWorkflow`Engine`m"]^2,s_Symbol/;rgzName[s]==="Nc":>N};
leftovers=DeleteDuplicates@Cases[z,x_/;MemberQ[{"SP","FV","ME","IndexDelta","Index","f","Ga","T"},rgzName[Head[x]]],{0,Infinity}];
If[!FreeQ[z,$Failed]||leftovers=!={},Return[$Failed]];
FCE[FCI[z]]];

rgzFRFC=Association@Table[id->rgzFRToFC[rgzRaw[[id,2]]],{id,rgzIDs}];

Print["FeynRules ConvertsuccessfulCount: ",Count[Values[rgzFRFC],Except[$Failed]]];


(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

rgzFieldName[field_]:=StringReplace[rgzName[field],{"φ"->"ϕ"}];



rgzDisplayField[name_String]:=Switch[name,"cbar",Overscript[Style["c",Italic],"_"],"ϕbar",Overscript[Style["ϕ",Italic],"_"],"βbar",Overscript[Style["β",Italic],"_"],"ωbar",Overscript[Style["ω",Italic],"_"],"ζbar",Overscript[Style["ζ",Italic],"_"],_,Style[name,Italic]];



rgzFAField[name_String,outgoing_]:=Switch[name,"eta",If[outgoing,-U[4],U[4]],"etabar",If[outgoing,U[4],-U[4]],"Q1",If[outgoing,-F[1],F[1]],"Q1bar",If[outgoing,F[1],-F[1]],"Q2",If[outgoing,-F[2],F[2]],"Q2bar",If[outgoing,F[2],-F[2]],"A",V[1],"ξ",S[1],"τ",S[2],"b",S[3],"c",If[outgoing,-U[1],U[1]],"cbar",If[outgoing,U[1],-U[1]],"ϕ",If[outgoing,-V[2],V[2]],"ϕbar",If[outgoing,V[2],-V[2]],"β",If[outgoing,-V[3],V[3]],"βbar",If[outgoing,V[3],-V[3]],"ω",If[outgoing,-U[2],U[2]],"ωbar",If[outgoing,U[2],-U[2]],"ζ",If[outgoing,-U[3],U[3]],"ζbar",If[outgoing,U[3],-U[3]],_,$Failed];


(*------------------------------------------------------------*)

(*------------------------------------------------------------*)




(* Match reference vertices by ordered fields, never by export row number. *)
rgzHand=Association@Cases[Table[With[{fields=rgzFieldName/@rgzRaw[[id,1,All,1]]},
 Which[fields==={"A","ξ","τ"},id->SMP["g_s"] SUNF[Glu1,Glu2,Glu3] FVD[p3,μ1],
 fields==={"A","ξ","ξ","τ"},id->-SMP["g_s"]^2/2 FVD[p4,μ1](SUNF[Glu1,Glu3,rgzHandColor] SUNF[Glu2,Glu4,rgzHandColor]+SUNF[Glu1,Glu2,rgzHandColor] SUNF[Glu3,Glu4,rgzHandColor]),True,Nothing]],{id,rgzIDs}],_Rule];


(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

rgzTop3=CreateTopologies[0,1->2,CTOrder->0,Adjacencies->3];

rgzTop4=CreateTopologies[0,2->2,CTOrder->0,Adjacencies->4];

rgzMom={p,k,l,r};



rgzDropIndexSums[expr_]:=expr//.(HoldPattern[h_[body_,specs__List]]/;rgzName[h]==="IndexSum":>(body/. Thread[(First/@{specs})->Table[Unique["rgzColorDummy"],{Length[{specs}]}]]));



rgzPrepareFA[expr_]:=rgzDropIndexSums[expr]/. {HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Colour":>SUNFIndex[rgzFundamental[h[type,i]]],HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Gluon":>SUNIndex[rgzColor[h[type,i]]],HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Gribov":>SUNIndex[rgzGribov[h[type,i]]]};



rgzNormalizeGribov[expr_]:=expr/. {HoldPattern[SUNIndex[h_[type_,i_]]]/;rgzName[h]==="Index"&&rgzName[type]==="Gribov":>SUNIndex[rgzGribov[h[type,i]]],HoldPattern[h_[type_,i_]]/;rgzName[h]==="Index"&&rgzName[type]==="Gribov":>SUNIndex[rgzGribov[h[type,i]]]};

rgzNormalizeFA[expr_]:=expr/. (HoldPattern[h_[x_,y_]]/;rgzName[h]==="IndexDelta":>SUNDelta[x,y]);

rgzReduce[expr_]:=Module[{z},z=rgzNormalizeGribov[rgzNormalizeFA[expr]];
z=Expand[ExpandScalarProduct[FCI[z]]];
z=FCCanonicalizeDummyIndices[z,SUNIndexNames->Table[Symbol["RGZWorkflow`Engine`rgzD"<>ToString[j]],{j,1,24}]];
FullSimplify[SUNSimplify[z]]];


(*------------------------------------------------------------*)

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
failure[reason_]:=<|"ID"->id,"Order"->n,"ExternalLegs"->names,"Status"->reason,"InsertionDiagramCount"->nInserted,"AmplitudeCount"->nAmplitudes,"FA"->Missing["NotGenerated"],"FR"->rgzFRFC[id],"FA-FR"->Missing["NotCompared"],"FA-Hand"->Missing["NotCompared"]|>;
If[rgzFRFC[id]===$Failed,Return[failure["FR format conversion failed"]]];
inFields=rgzFAField[#,False]&/@Take[names,nIn];
outFields=rgzFAField[#,True]&/@Drop[names,nIn];
If[!FreeQ[{inFields,outFields},$Failed],Return[failure["UnknownFieldName"]]];
inserted=InsertFields[If[n==3,rgzTop3,rgzTop4],inFields->outFields,InsertionLevel->{Particles},Model->rgzModel,GenericModel->rgzModel];
nInserted=Length[inserted];
If[nInserted==0,Return[failure["without InsertionDiagram"]]];
fa=Check[CreateFeynAmp[inserted,Truncated->True,GaugeRules->{},PreFactor->1],$Failed,{CreateFeynAmp::nocoupl,CreateFeynAmp::noprop}];
If[fa===$Failed,Return[failure["couplingOrPropagatorUnresolved"]]];
nAmplitudes=Length[fa];
If[nAmplitudes==0,Return[failure["The amplitude list is empty"]]];
(* ==========================================================*)(* ==========================================================*)lorentzPositions=Flatten@Position[names,x_/;MemberQ[{"eta","etabar","A","ϕ","ϕbar","β","βbar","c","cbar","ω","ωbar","ζ","ζbar"},x],{1}];
lorentzNames=Join[Table[Symbol["RGZWorkflow`Engine`μ"<>ToString[j]],{j,lorentzPositions}],Table[Symbol["RGZWorkflow`Engine`rgzExtraLor"<>ToString[j]],{j,1,8}]];
amp=FCFAConvert[rgzPrepareFA[fa],IncomingMomenta->Take[rgzMom,nIn],OutgoingMomenta->Take[rgzMom,{nIn+1,n}],LorentzIndexNames->lorentzNames,ChangeDimension->D,List->False,DropSumOver->True,FinalSubstitutions->{Global`FAGS->SMP["g_s"],FCGV["MU"]->SMP["m_q"]}];
amp=rgzVertexAmplitudeNormalize[amp,names];
leftovers=DeleteDuplicates@Cases[amp,x_/;MemberQ[{"FASUNF","FAFourVector","FAMetricTensor","FAScalarProduct","IndexSum","AnalyticalCoupling","AnalyticalPropagator"},rgzName[Head[x]]],{0,Infinity}];
If[Head[amp]===FCFAConvert||leftovers=!={},Return[failure["FA tensor conversion is incomplete"]]];
rules=Table[rgzP[j]->If[j<=nIn,rgzMom[[j]],-rgzMom[[j]]],{j,n}];
fr=rgzFRFC[id]/. rules;
difference=rgzReduce[amp-fr];
conservationRule=If[n==3,l->p-k,r->p+k-l];
If[!TrueQ[difference===0],difference=rgzReduce[difference/. conservationRule]];
If[KeyExistsQ[rgzHand,id],hand=rgzHand[id]/. rules;
handDifference=rgzReduce[amp-hand];
If[!TrueQ[handDifference===0],handDifference=rgzReduce[handDifference/. conservationRule]],handDifference=Missing["Hand calculation not supplied"]];
status=Which[TrueQ[difference===0]&&TrueQ[handDifference===0],"Three-way agreement",TrueQ[difference===0]&&Head[handDifference]===Missing,"FA=FR; hand calculation not supplied",TrueQ[difference===0],"FA=FR; hand calculation mismatch",True,"FA and FR disagree"];
<|"ID"->id,"Order"->n,"ExternalLegs"->names,"Status"->status,"InsertionDiagramCount"->nInserted,"AmplitudeCount"->nAmplitudes,"FA"->amp,"FR"->fr,"FA-FR"->difference,"FA-Hand"->handDifference|>];


(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

rgzShow[id_Integer]:=Module[{item},item=SelectFirst[rgzResults,#["ID"]==id&,Missing["no such ItemsID"]];
If[Head[item]===Missing,Print[item];
Return[$Failed]];
Print["ID: ",id,"; ExternalLegs: ",Row[rgzDisplayField/@item["ExternalLegs"],", "]];
Print["Status: ",item["Status"]];
Print["FeynArts：",TraditionalForm[item["FA"]]];
Print["FeynRules：",TraditionalForm[item["FR"]]];
Print["FA-FR：",TraditionalForm[item["FA-FR"]]];
Print["FA-Hand: ",TraditionalForm[item["FA-Hand"]]];];
