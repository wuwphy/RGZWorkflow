(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringBoxWard.wl"}]];
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringTensor.wl"}]];
(* Observation layer; no change to physical job fingerprints or reduced-part files. *)
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
qqRunPartDetailed[name_String,seconds_]:=engineEvaluation@Module[{job=RGZResult["QQ"],r,input,file,saved,cached,raw},
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[Lookup[job,"Fingerprint",None]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 If[!KeyExistsQ[job["Integrands"],name],Return[Failure["QQPart",<|"Names"->Keys[job["Integrands"]]|>]]];
 file=FileNameJoin[{job["Directory"],name<>"-reduced.wxf"}];
 qqProgressUpdate[<|"CheckpointFile"->file|>];
 If[FileExistsQ[file],qqPartStage["Read completed part",0,"Reading"];cached=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[cached]||Lookup[cached,"Fingerprint",None]=!=job["Fingerprint"]||Lookup[cached,"Part",None]=!=name,Return[Failure["QQCacheInvalid",<|"File"->file|>]]];r=cached["Expression"];qqProgressUpdate[<|"CacheState"->"Reused","OperationFinished"->AbsoluteTime[]|>],
 qqPartStage["Inspect integrand",0];raw=job["Integrands"][name];
 If[MatchQ[raw,Missing["DeferredNAV",_Integer]],
 raw=qqPartTimed["QQAttach:"<>name,-RGZWorkflow`Engine`rgzQQNAV[job["NAVSource"],raw[[2]]],seconds];If[FailureQ[raw],Return[raw]];
 raw=raw/.{RGZWorkflow`Engine`\[Alpha]->job["Gauge"],RGZWorkflow`Engine`M->job["RegulatorMass"]}];
 If[MemberQ[{"Box1","Box2"},name],raw=qqPartTimed["QQWardPrecondition:"<>name,qqBoxWardPrecondition[raw,name],seconds];If[FailureQ[raw],Return[raw]]];
 raw=qqPartTimed["QQWardPrecondition:Routes:"<>name,qqRestoreAffineSquares[raw],seconds];If[FailureQ[raw],Return[raw]];
 input=qqPartTimed["QQPoleSplit:"<>name,RGZWorkflow`Engine`rgzQQSplit[raw,job["Gauge"],job["RegulatorMass"]],seconds];If[FailureQ[input],Return[input]];
 r=qqPartTimed["QQTID:"<>name,Check[qqReduceTensorGroups[input,job,name,seconds],Failure["QQTensorReductionMessages",<|"Part"->name|>]],seconds];If[FailureQ[r],Return[r]];
 If[!FreeQ[r,FeynCalc`TID|FeynCalc`FeynAmpDenominator|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 qqPartStage["Restore pole masses and loop measure",3];
 r=RGZWorkflow`Engine`rgzQQRestorePoleMasses[r,job["Gauge"],job["RegulatorMass"]] job["LoopMeasureConversion"];
 If[!FreeQ[r,_Failure|_Missing|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 qqProgressUpdate[<|"OutputLeaves"->LeafCount[r]|>];qqPartStage["Save reduced part",4,"Saving"];saved=atomicWXF[file,<|"Fingerprint"->job["Fingerprint"],"Part"->name,"Expression"->r|>];If[FailureQ[saved],Return[saved]];qqProgressUpdate[<|"CacheState"->"Saved","OperationFinished"->AbsoluteTime[]|>]];
 If[!FreeQ[r,_Failure|_Missing|FeynCalc`TID|FeynCalc`FeynAmpDenominator|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 qqPartStage["Save QQ task",5,"Saving"];
 AssociateTo[job,"Reduced"->Append[job["Reduced"],name->r]];saved=atomicWXF[FileNameJoin[{job["Directory"],"job.wxf"}],job];If[FailureQ[saved],Return[saved]];storeResult["QQ",job];
 <|"Status"->"Complete","Name"->name,"File"->file|>
];

RGZQQRunPart[name_String,seconds_:300] := Block[{$IterationLimit=Infinity},Module[{answer,now=AbsoluteTime[]},
 If[!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["TimeLimit",<||>]]];
 $qqProgress=<|"Part"->name,"Stage"->"QQPart:"<>name,"Index"->0,"Total"->5,
 "Status"->"Computing","RunStarted"->now,"ItemStarted"->now,"OperationStarted"->now,
 "Operation"->"Validate task","RequestedSeconds"->seconds,"StepLimitSeconds"->seconds|>;
 storeResult["QQProgress",$qqProgress];
 answer=If[TrueQ[$Notebooks],Monitor[qqRunPartDetailed[name,seconds],Dynamic[qqSelfProgressPanel[],UpdateInterval->1,TrackedSymbols:>{$qqProgress}]],qqRunPartDetailed[name,seconds]];
 If[!FailureQ[answer],qqProgressUpdate[<|"CacheState"->"Saved"|>]];
 qqProgressUpdate[<|"Status"->If[FailureQ[answer],"Stopped","Complete"],"StoppedAt"->AbsoluteTime[],"OperationFinished"->AbsoluteTime[]|>];
 answer]];
