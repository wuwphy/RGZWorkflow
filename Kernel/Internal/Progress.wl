(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Observe QQ progress and store resumable self-preparation checkpoints.

   Inputs:
   The current configuration and the explicit expression, task or result packet supplied by the caller.

   Dependencies:
   The package configuration and explicitly supplied data; no physics task is started merely by loading.
*)

(* Progress and equivalent algebra layer: physical checkpoint identities stay unchanged. *)
qqProgressUpdate[fields_Association] := (
 If[!AssociationQ[$qqProgress],$qqProgress=<||>];
 AssociateTo[$qqProgress,fields];storeResult["QQProgress",$qqProgress]);
qqSelfProgress[stage_,index_,total_,state_:"Computing"] := Module[{now=AbsoluteTime[],fresh},
 fresh=stage==="Matrix"&&index===0&&state==="Computing";
 If[fresh,$qqProgress=<|"RunStarted"->now,"ReusedCheckpoints"->0,"ComputedCheckpoints"->0|>];
 If[Lookup[$qqProgress,"Stage",None]=!=stage||Lookup[$qqProgress,"Index",None]=!=index,
 qqProgressUpdate[<|"ItemStarted"->now,"Operation"->"Preparing stage","OperationStarted"->now,"OperationFinished"->Missing["NotAvailable"],"Master"->Missing["NotAvailable"],"InputLeaves"->Missing["NotAvailable"],"DenominatorGroups"->Missing["NotAvailable"],"OutputLeaves"->Missing["NotAvailable"],"AlgebraGroupIndex"->0,"AlgebraGroupTotal"->0|>]];
 qqProgressUpdate[<|"Stage"->stage,"Index"->index,"Total"->total,"Status"->state|>];
 If[MemberQ[{"Stopped","Complete"},state],qqProgressUpdate[<|"StoppedAt"->now|>]]];
qqProgressOperation[name_] := qqProgressUpdate[<|"Operation"->name,"OperationStarted"->AbsoluteTime[],"OperationFinished"->Missing["NotAvailable"]|>];
(* Inactivate algebra before snapshotting its data arguments: cache lookup must
   not evaluate the expensive stage. Resolve association-valued accessors;
   ordinary value arguments evaluate inside Inactivate without running heads. *)
SetAttributes[qqSelfCheckpointInput,HoldAll];
qqSelfCheckpointInput[expression_] := Module[{held,locals,rules},
 held=Inactivate[expression];
 held=held/.Inactive[s_Symbol]/;OwnValues[s]=!={} && (AssociationQ[s]||ListQ[s]):>s;
 (* A local assignment in a CompoundExpression has a new Module suffix on
    every restart. Canonicalize only assignment-bound names, not arbitrary
    free symbols or dummy indices contained in physical data. *)
 locals=DeleteDuplicates[Cases[held,Inactive[Set|SetDelayed][v_Symbol,_]:>v,Infinity]];
 rules=MapIndexed[Rule[#1,qqCheckpointLocal[First[#2]]]&,locals];
 held=held/.rules;
 (* These two trailing arguments are execution budgets, not physical data. *)
 held/.{Inactive[RGZWorkflow`Engine`rgzQQM0Term][pair_,_]:>Inactive[RGZWorkflow`Engine`rgzQQM0Term][pair,qqCheckpointBudget],
  Inactive[RGZWorkflow`Engine`rgzQQGaugeValue][value_,gauge_,_]:>Inactive[RGZWorkflow`Engine`rgzQQGaugeValue][value,gauge,qqCheckpointBudget]}];
qqSelfPacketQ[p_,key_,name_,identity_] := AssociationQ[p]&&
 Lookup[p,"Schema",None]==="QQSelfCheckpoint/2"&&Lookup[p,"Key",None]===key&&
 Lookup[p,"Name",None]===name&&Lookup[p,"InputIdentity",None]===identity&&
 KeyExistsQ[p,"Value"]&&!qqFFInvalidQ[p["Value"]]&&
 Lookup[p,"ValueHash",None]===Hash[p["Value"],"SHA256"];
SetAttributes[qqSelfCheckpointBound,HoldAll];
qqSelfCheckpointBound[dir_,name_,key_,expression_,seconds_] := Module[{file,r,saved,input,identity},
 If[!StringQ[dir]||!StringQ[name]||!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["QQSelfCheckpointArguments",<||>]]];
 file=FileNameJoin[{dir,name<>".wxf"}];
 If[StringMatchQ[name,"m0-sum-"~~DigitCharacter..],qqSelfProgress["M0Sum",ToExpression[StringDrop[name,7]],3]];
 qqProgressUpdate[<|"Checkpoint"->name,"CheckpointFile"->file,"StepLimitSeconds"->seconds|>];
 qqProgressOperation["Snapshot checkpoint inputs without running stage algebra"];
 input=timed["QQSelfInput:"<>name,qqSelfCheckpointInput[expression],seconds];
 If[qqFFInvalidQ[input],Return[Failure["QQSelfCheckpointInputInvalid",<|"Cause"->input|>]]];
 identity=Hash[{"QQSelfCheckpoint/2",key,name,input,qqCalculationContext[],workflowFingerprint[]},"SHA256"];
 If[FileExistsQ[file],
 qqProgressOperation["Read completed checkpoint"];qqProgressUpdate[<|"CacheState"->"Reading"|>];
 r=timed["QQSelfRead:"<>name,Quiet[Check[Import[file,"WXF"],$Failed]],seconds];
 If[FailureQ[r],Return[r]];
 If[qqSelfPacketQ[r,key,name,identity],
  qqProgressUpdate[<|"OperationFinished"->AbsoluteTime[],"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];
  Return[r["Value"]]];
 qqProgressUpdate[<|"CacheState"->"RecomputeIncompatibleCheckpoint"|>]];
 qqProgressUpdate[<|"CacheState"->"Computing"|>];
 r=timed["QQSelf:"<>name,expression,seconds];If[FailureQ[r],Return[r]];
 If[qqFFInvalidQ[r],Return[Failure["QQSelfCheckpointIncomplete",<|"Checkpoint"->name|>]]];
 qqProgressOperation["Save completed checkpoint"];
 saved=timed["QQSelfWrite:"<>name,atomicWXF[file,<|"Schema"->"QQSelfCheckpoint/2","Key"->key,"Name"->name,
  "InputIdentity"->identity,"Value"->r,"ValueHash"->Hash[r,"SHA256"]|>],seconds];
 If[FailureQ[saved],saved,
 qqProgressUpdate[<|"OperationFinished"->AbsoluteTime[],"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>];r]];
SetAttributes[qqSelfCheckpoint,HoldAll];
qqSelfCheckpoint[dir_,name_,key_,expression_,seconds_] := qqSelfCheckpointBound[dir,name,key,expression,seconds];
qqSelfProgressPanel[] := Module[{p=$qqProgress,now,elapsed,operationElapsed,runElapsed,master},
 now=If[MemberQ[{"Stopped","Complete"},Lookup[p,"Status",None]],Lookup[p,"StoppedAt",AbsoluteTime[]],AbsoluteTime[]];
 elapsed=Max[0,Floor[now-Lookup[p,"ItemStarted",now]]];
 operationElapsed=Max[0,Floor[If[NumericQ[Lookup[p,"OperationFinished",None]],p["OperationFinished"],now]-Lookup[p,"OperationStarted",now]]];
 runElapsed=Max[0,Floor[now-Lookup[p,"RunStarted",now]]];
 master=Lookup[p,"Master",Missing["NotAvailable"]];
 Panel[Grid[{
 {"Stage",Lookup[p,"Stage",""]<>"   "<>ToString[Lookup[p,"Index",0]]<>" / "<>ToString[Lookup[p,"Total",0]]},
 {"Master integral",If[MissingQ[master],"-",Short[master,3]]},
 {"Operation",Lookup[p,"Operation","Preparing"]},
 {"Current operation",ToString[operationElapsed]<>" s"},
 {"Current item",ToString[elapsed]<>" s; step limit "<>ToString[Lookup[p,"StepLimitSeconds","-"]]<>" s"},
 {"This run",ToString[runElapsed]<>" s"},
 {"Input expression",If[MissingQ[Lookup[p,"InputLeaves",Missing["NotAvailable"]]],"-",ToString[p["InputLeaves"]]<>" leaves"]},
 {"Algebra groups",ToString[Lookup[p,"AlgebraGroupIndex",0]]<>" / "<>ToString[Lookup[p,"AlgebraGroupTotal",0]]},
 {"PV transforms",ToString[Lookup[p,"PVIndex",0]]<>" / "<>ToString[Lookup[p,"PVTotal",0]]},
 {"Scalar masters",ToString[Lookup[p,"ScalarMasterIndex",0]]<>" / "<>ToString[Lookup[p,"ScalarMasterTotal",0]]},
 {"Output expression",If[MissingQ[Lookup[p,"OutputLeaves",Missing["NotAvailable"]]],"-",ToString[p["OutputLeaves"]]<>" leaves"]},
 {"Checkpoints",ToString[Lookup[p,"ReusedCheckpoints",0]]<>" reused; "<>ToString[Lookup[p,"ComputedCheckpoints",0]]<>" newly saved"},
 {"Coefficient blocks",ToString[Lookup[p,"CoefficientCacheReused",0]]<>" reused; "<>ToString[Lookup[p,"CoefficientCacheSaved",0]]<>" newly saved"},
 {"Coefficient subgroups",ToString[Lookup[p,"CoefficientSubReused",0]]<>" reused; "<>ToString[Lookup[p,"CoefficientSubSaved",0]]<>" newly saved"},
 {"Numerator coefficients",ToString[Lookup[p,"NumeratorCoefficientIndex",0]]<>" / "<>ToString[Lookup[p,"NumeratorCoefficientTotal",0]]},
 {"Numerator cache",ToString[Lookup[p,"NumeratorCacheReused",0]]<>" reused; "<>ToString[Lookup[p,"NumeratorCacheSaved",0]]<>" newly saved"},
 {"TID integrals",ToString[Lookup[p,"TensorIntegralReused",0]]<>" reused; "<>ToString[Lookup[p,"TensorIntegralComputed",0]]<>" computed"},
 {"Cache",Lookup[p,"CacheState","-"]},
 {"Status",Lookup[p,"Status","Computing"]}
 },Alignment->Left]]];
(* Observation only: the same expression is evaluated with or without a front end. *)
SetAttributes[qqSelfMonitor,HoldAll];
qqSelfMonitor[expression_,display_] := If[TrueQ[$Notebooks],
 Monitor[expression,Dynamic[qqSelfProgressPanel[],UpdateInterval->1,TrackedSymbols:>{$qqProgress}]],expression];
