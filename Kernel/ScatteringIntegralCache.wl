(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Share TID's own canonical tensor integrals across open spin chains.
   No new tensor decomposition is used. The hook is scoped to this call;
   FeynCalc definitions and the original outer checkpoint keys are preserved. *)
qqIntegralRename[x_,rules_] := x/.FeynCalc`LorentzIndex[label_,dim___]:>
 FeynCalc`LorentzIndex[label/.rules,dim];
qqIntegralTemplate[x_] := Module[{labels,names,forward,back,core},
 labels=DeleteDuplicates[Cases[x,FeynCalc`LorentzIndex[label_,___]:>label,Infinity]];
 names=Table[Symbol["RGZWorkflow`IntegralCache`li"<>ToString[i]],{i,Length[labels]}];
 (* Avoid a simultaneous rename with overlapping source and target names. *)
 If[Intersection[labels,names]=!={},Return[$Failed]];
 forward=Thread[labels->names];back=Thread[names->labels];
 core=qqIntegralRename[x,forward];
 If[qqIntegralRename[core,back]=!=x,Return[$Failed]];
 <|"Core"->core,"Back"->back|>];
qqIntegralValueQ[x_] := FreeQ[x,_Failure|_Missing|_TerminatedEvaluation|
 $Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity|_HoldForm|
 FeynCalc`TID`Private`tidSingleIntegral];

(* Validate dependencies without expanding the represented expressions. *)
qqIntegralAcyclicQ[values_List] := Module[{n=Length[values],dependencies,parents,remaining,queue,cursor=1,node,parent,i,dependency},
 dependencies=DeleteDuplicates[Cases[#,qqIntegralAbbreviation[j_Integer]:>j,{0,Infinity}]]&/@values;
 If[!AllTrue[Flatten[dependencies],1<=#<=n&],Return[False]];
 parents=ConstantArray[{},n];remaining=Length/@dependencies;
 Do[Do[AppendTo[parents[[dependency]],i],{dependency,dependencies[[i]]}],{i,n}];
 queue=Flatten[Position[remaining,0,{1},Heads->False]];
 While[cursor<=Length[queue],node=queue[[cursor]];cursor++;
  Do[remaining[[parent]]--;If[remaining[[parent]]===0,AppendTo[queue,parent]],{parent,parents[[node]]}]];
 Length[queue]===n];

(* Freeze the abbreviation graph without FRH-expanding its shared scalar
   factors. Expanding a rank-five result here can multiply its size by orders
   of magnitude and undo the benefit of TID's Isolate representation. *)
qqIntegralFreeze[x_] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{held,values={},more,rules,packet,i=1},
 held=DeleteDuplicates[Cases[x,_HoldForm,{0,Infinity}]];
 While[i<=Length[held],
  more=ReleaseHold[held[[i]]];
  If[more===held[[i]],Throw[$Failed,rgzFunctionReturnTag]];
  AppendTo[values,more];
  held=DeleteDuplicates[Join[held,Cases[more,_HoldForm,{0,Infinity}]]];i++];
 (* TID isolates scalar coefficients only; index-bearing abbreviations must
    not bypass the index-renaming operation outside HoldForm. *)
 If[!FreeQ[values,_FeynCalc`LorentzIndex],Return[$Failed]];
 rules=Dispatch[MapIndexed[Rule[#1,qqIntegralAbbreviation[First[#2]]]&,held]];
 packet=<|"Value"->(x/.rules),"Abbreviations"->(values/.rules)|>;
 If[qqIntegralAcyclicQ[packet["Abbreviations"]],packet,$Failed]],rgzFunctionReturnTag]];
(* Resolve the frozen DAG to ordinary expressions before returning to TID.
   No HoldForm or session-local definition may reach an outer checkpoint. *)
qqIntegralThaw[packet_,head_] := Module[{values=packet["Abbreviations"],resolve,state=<||>,stop=Unique["thawStop"],answer},
 Internal`WithLocalSettings[Null,Catch[
 resolve[i_Integer] := Module[{r},
  If[!TrueQ[1<=i<=Length[values]],Throw[$Failed,stop]];
  If[Lookup[state,i,None]==="Visiting",Throw[$Failed,stop]];
  If[KeyExistsQ[state,i]&&state[i]=!="Visiting",Return[state[i]]];
  AssociateTo[state,i->"Visiting"];
  r=values[[i]]/.HoldPattern[qqIntegralAbbreviation[j_Integer]]:>resolve[j];
  AssociateTo[state,i->r];r];
 answer=packet["Value"]/.HoldPattern[qqIntegralAbbreviation[j_Integer]]:>resolve[j];
 If[qqIntegralValueQ[answer],answer,$Failed],stop],Clear[resolve];state=<||>]];
qqIntegralResolve[x_] := Module[{p},If[FreeQ[x,_HoldForm],Return[x]];
 p=qqIntegralFreeze[x];If[p===$Failed,Return[$Failed]];qqIntegralThaw[p,Null]];
qqIntegralPacketQ[packet_,key_] := AssociationQ[packet]&&Lookup[packet,"Key",None]===key&&
 KeyExistsQ[packet,"Value"]&&ListQ[Lookup[packet,"Abbreviations",None]]&&
 qqIntegralValueQ[{packet["Value"],packet["Abbreviations"]}]&&
 AllTrue[Cases[{packet["Value"],packet["Abbreviations"]},qqIntegralAbbreviation[i_]:>i,Infinity],
  IntegerQ[#]&&1<=#<=Length[packet["Abbreviations"]]&]&&qqIntegralAcyclicQ[packet["Abbreviations"]];

SetAttributes[qqWithTensorIntegralCache,HoldRest];
qqWithTensorIntegralCache[job_,body_] := Module[{original,definitions,signature,dir,memo=<||>,instances=<||>,answer,heads={}},
 definitions=DownValues[FeynCalc`TID`Private`tidSingleIntegral];
 (* A FeynCalc release without this helper uses the unmodified pipeline. *)
 If[definitions==={},Return[body]];
 signature=Hash[{definitions,$Version,Options[FeynCalc`TID],
  DownValues[RGZWorkflow`Engine`rgzQQReduce]},"SHA256"];
 dir=FileNameJoin[{job["Directory"],"tensor-integrals-v2"}];
 If[!DirectoryQ[dir],CreateDirectory[dir]];
 DownValues[original]=definitions/.FeynCalc`TID`Private`tidSingleIntegral->original;
 Internal`WithLocalSettings[Null,
 answer=Internal`InheritedBlock[{FeynCalc`TID`Private`tidSingleIntegral},
 DownValues[FeynCalc`TID`Private`tidSingleIntegral]={};
 FeynCalc`TID`Private`tidSingleIntegral[args___] := Module[
  {a={args},template,canonical,state,key,file,packet,r,saved,rank,operation,head},
  If[Length[a]=!=7,Return[original[args]]];
  template=qqIntegralTemplate[First[a]];
  If[template===$Failed,Return[original[args]]];
  canonical=Prepend[Rest[a],template["Core"]];
  (* Capture the actual scalar-product definitions inside rgzQQReduce's
     inherited kinematic scope, including the scaleless-integral convention. *)
  state=Hash[{DownValues[FeynCalc`Pair],DownValues[FeynCalc`ScalarProduct],
   DownValues[FeynCalc`SP],DownValues[FeynCalc`SPD],DownValues[FeynCalc`SPE],
   DownValues[FeynCalc`SPLR],DownValues[FeynCalc`SPLRD],
   FeynCalc`$KeepLogDivergentScalelessIntegrals,FeynCalc`TID`Private`genpave,
   FeynCalc`TID`Private`paveao,FeynCalc`TID`Private`pavear},"SHA256"];
  key=Hash[{"QQTensorIntegral/2",job["Fingerprint"],signature,state,canonical},"SHA256"];
  file=FileNameJoin[{dir,IntegerString[key,16,64]<>".wxf"}];
  rank=Length[DeleteDuplicates[Cases[First[canonical],_FeynCalc`LorentzIndex,Infinity]]];
  operation="TID integral (rank "<>ToString[rank]<>")";
  qqProgressOperation[operation<>" - lookup"];
  If[KeyExistsQ[memo,key],packet=memo[key],
   If[FileExistsQ[file],
    packet=Quiet[Check[Import[file,"WXF"],$Failed]];
    If[!qqIntegralPacketQ[packet,key],packet=$Failed],packet=$Failed];
   If[packet=!=$Failed,AssociateTo[memo,key->packet]]];
  If[packet=!=$Failed,
   qqProgressUpdate[<|"TensorIntegralReused"->(Lookup[$qqProgress,"TensorIntegralReused",0]+1)|>],
   qqProgressOperation[operation<>" - reduce"];
   r=original@@canonical;
   packet=qqIntegralFreeze[r];
   If[packet===$Failed,Return[qqIntegralRename[r,template["Back"]]]];
   packet=Join[packet,<|"Key"->key|>];
   If[!qqIntegralPacketQ[packet,key],Return[qqIntegralRename[r,template["Back"]]]];
   qqProgressOperation[operation<>" - save"];
   saved=atomicWXF[file,packet];
   If[!FailureQ[saved],AssociateTo[memo,key->packet]];
   qqProgressUpdate[<|"TensorIntegralComputed"->(Lookup[$qqProgress,"TensorIntegralComputed",0]+1)|>]];
  If[KeyExistsQ[instances,key],r=instances[key],
   AppendTo[heads,head];
   r=qqIntegralThaw[packet,head];AssociateTo[instances,key->r]];
  r=qqIntegralRename[r,template["Back"]];
  qqProgressOperation["TID - PV conversion and final contraction"];
  r];
 body],Scan[Function[s,Remove[s],HoldAll],heads]];answer];
