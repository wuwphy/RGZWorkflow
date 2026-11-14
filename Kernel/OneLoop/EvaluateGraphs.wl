(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Evaluate one conventional vertex or box part through the shared algebra and native TID.

   Inputs:
   The checked tree/model data and the graph or task data named by the called function.

   Dependencies:
   Tree/ definitions and Algebra/ tools; Internal/ supplies configuration and persistence.
*)

(* Progress and input-bound reduced-part persistence. *)
qqPartStage[operation_,index_,cache_:"Computing"] := (
 qqProgressUpdate[<|"Stage"->"QQPart:"<>Lookup[$qqProgress,"Part",""],"Index"->index,"Total"->5,"CacheState"->cache|>];
 qqProgressOperation[operation]);
SetAttributes[qqPartTimed,HoldRest];
qqPartTimed[stage_,expression_,seconds_] := Module[{r,held=HoldComplete[expression],input},
 qqPartStage[stage,Which[StringStartsQ[stage,"QQAttach:"]||StringStartsQ[stage,"QQWardPrecondition:"],0,StringStartsQ[stage,"QQPoleSplit:"],1,True,2]];
 input=Replace[held,{HoldPattern[HoldComplete[RGZWorkflow`Engine`rgzQQSplit[x_,_,_]]]:>LeafCount[x],
 HoldPattern[HoldComplete[Check[qqReduceTensorGroups[x_,___],_]]]:>LeafCount[x],_->Missing["NotAvailable"]}];
 qqProgressUpdate[<|"StepLimitSeconds"->seconds,"InputLeaves"->input|>];
 r=timed[stage,expression,seconds];
 qqProgressUpdate[<|"OperationFinished"->AbsoluteTime[]|>];r];
qqRunPartDetailed[name_String,seconds_]:=engineEvaluation@Module[{job=RGZResult["QQ"],r,input,file,saved,cached,raw,identity,proof,cacheState},
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[!And@@(KeyExistsQ[job,#]&/@{"Fingerprint","Gauge","RegulatorMass","Directory","Integrands"})||
 !AssociationQ[job["Integrands"]]||!StringQ[job["Directory"]],Return[Failure["QQPartInputInvalid",<||>]]];
 If[!qqPhysicalLoopMeasureQ[Lookup[job,"LoopMeasureConversion",None]],
 Return[Failure["QQMeasureConventionRequired",<|"Expected"->1/(2 Pi)^4|>]]];
 If[Lookup[job,"Fingerprint",None]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 r=engineGet["OneLoop/QuarkGraphs.wl",False];If[FailureQ[r],Return[r]];
 If[!KeyExistsQ[job["Integrands"],name],Return[Failure["QQPart",<|"Names"->Keys[job["Integrands"]]|>]]];
 If[!And@@(KeyExistsQ[job,#]&/@{"LoopMeasureConversion","Gauge","RegulatorMass"}) ||
 qqFFInvalidQ[job["Integrands"][name]/.Missing["DeferredNAV",1|2]->0],Return[Failure["QQPartInputInvalid",<||>]]];
 identity=qqPartInputIdentity[job,name];
 file=FileNameJoin[{job["Directory"],name<>"-reduced.wxf"}];
 qqProgressUpdate[<|"CheckpointFile"->file|>];
 qqPartStage["Read reduced part checkpoint",0,"Reading"];
 cached=If[FileExistsQ[file],timed["QQPartRead:"<>name,Quiet[Check[Import[file,"WXF"],$Failed]],seconds],Missing["NoCache"]];
 If[FailureQ[cached],Return[cached]];
 If[qqPartCacheQ[cached,job,name],r=cached["Expression"];cacheState="Reused";
 qqProgressUpdate[<|"CacheState"->"Reused","OperationFinished"->AbsoluteTime[]|>],
 qqPartStage["Inspect integrand",0];raw=job["Integrands"][name];
 If[MatchQ[raw,Missing["DeferredNAV",_Integer]],
 raw=qqPartTimed["QQAttach:"<>name,-RGZWorkflow`Engine`rgzQQNAV[job["NAVSource"],raw[[2]]],seconds];If[FailureQ[raw],Return[raw]];
 raw=raw/.{RGZWorkflow`Engine`\[Alpha]->job["Gauge"],RGZWorkflow`Engine`M->job["RegulatorMass"]}];
 If[MemberQ[{"Box1","Box2"},name],raw=qqPartTimed["QQWardPrecondition:"<>name,qqBoxWardPrecondition[raw,name],seconds];If[FailureQ[raw],Return[raw]]];
 If[qqFFInvalidQ[raw],Return[Failure["QQPartInputInvalid",<||>]]];
 raw=qqPartTimed["QQWardPrecondition:Routes:"<>name,qqRestoreAffineSquares[raw],seconds];If[FailureQ[raw],Return[raw]];
 input=qqPartTimed["QQPoleSplit:"<>name,RGZWorkflow`Engine`rgzQQSplit[raw,job["Gauge"],job["RegulatorMass"]],seconds];If[qqFFInvalidQ[input],Return[Failure["QQPoleSplitIncomplete",<|"Part"->name,"Cause"->input|>]]];
 r=qqPartTimed["QQTID:"<>name,Check[qqReduceTensorGroups[input,job,name,seconds],Failure["QQTensorReductionMessages",<|"Part"->name|>]],seconds];If[FailureQ[r],Return[r]];
 If[!FreeQ[r,FeynCalc`TID|FeynCalc`FeynAmpDenominator|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 qqPartStage["Restore pole masses and loop measure",3];
 r=RGZWorkflow`Engine`rgzQQRestorePoleMasses[r,job["Gauge"],job["RegulatorMass"]] job["LoopMeasureConversion"];
 If[!FreeQ[r,_Failure|_Missing|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 qqProgressUpdate[<|"OutputLeaves"->LeafCount[r]|>];qqPartStage["Save reduced part",4,"Saving"];saved=timed["QQPartSave:"<>name,atomicWXF[file,<|"Fingerprint"->job["Fingerprint"],"Part"->name,"Expression"->r,"Provenance"->qqPartProof[job,name,r]|>],seconds];
 If[FailureQ[saved],Return[saved]];
 If[!AssociationQ[saved]||Lookup[saved,"Status",None]=!="Saved",Return[Failure["QQPartSaveFailed",<|"File"->file|>]]];cacheState="Saved";qqProgressUpdate[<|"CacheState"->"Saved","OperationFinished"->AbsoluteTime[]|>]];
 If[!FreeQ[r,_Failure|_Missing|FeynCalc`TID|FeynCalc`FeynAmpDenominator|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 qqPartStage["Save QQ task",5,"Saving"];
 If[identity=!=qqPartInputIdentity[RGZResult["QQ"],name],Return[Failure["QQPartSourceChanged",<||>]]];
 proof=qqPartProof[job,name,r];
 AssociateTo[job,{"Reduced"->Append[job["Reduced"],name->r],
  "ReducedProvenance"->Append[Lookup[job,"ReducedProvenance",<||>],name->proof]}];saved=timed["QQPartSaveTask:"<>name,atomicWXF[FileNameJoin[{job["Directory"],"job.wxf"}],job],seconds];
 If[FailureQ[saved],Return[saved]];
 If[!AssociationQ[saved]||Lookup[saved,"Status",None]=!="Saved",Return[Failure["QQPartSaveFailed",<|"File"->"job.wxf"|>]]];
 invalidateQQDerived[];storeResult["QQ",job];qqProgressUpdate[<|"CacheState"->cacheState|>];
 <|"Status"->"Complete","Name"->name,"File"->file|>
];

RGZQQRunPart[name_String,seconds_:300] := Block[{$IterationLimit=Infinity},Module[{answer,now=AbsoluteTime[]},
 $qqProgress=<|"Part"->name,"Stage"->"QQPart:"<>name,"Index"->0,"Total"->5,
 "Status"->"Computing","RunStarted"->now,"ItemStarted"->now,"OperationStarted"->now,
 "Operation"->"Validate task","RequestedSeconds"->seconds,"StepLimitSeconds"->seconds|>;
 storeResult["QQProgress",$qqProgress];
 answer=If[!NumericQ[seconds]||!TrueQ[seconds>0],Failure["TimeLimit",<||>],CheckAbort[If[TrueQ[$Notebooks],Monitor[qqRunPartDetailed[name,seconds],Dynamic[qqSelfProgressPanel[],UpdateInterval->1,TrackedSymbols:>{$qqProgress}]],qqRunPartDetailed[name,seconds]],Failure["QQPartAborted",<|"Part"->name|>]]];
 If[!FailureQ[answer]&&(!AssociationQ[answer]||Lookup[answer,"Status",None]=!="Complete"||qqFFInvalidQ[answer]),
 answer=Failure["QQPartIncomplete",<|"Part"->name,"Cause"->answer|>]];
 If[FailureQ[answer],qqProgressUpdate[<|"CacheState"->"Stopped"|>]];
 qqProgressUpdate[<|"Status"->If[FailureQ[answer],"Stopped","Complete"],"StoppedAt"->AbsoluteTime[],"OperationFinished"->AbsoluteTime[]|>];
 answer]];
