(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Progress and equivalent algebra layer: physical checkpoint identities stay unchanged. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringSimplify.wl"}]];
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
SetAttributes[qqSelfRationalProgress,HoldAll];
qqSelfRationalProgress[coefficient_] := Module[{input,result,master},
 qqProgressOperation["Inspect coefficient"];
 master=Replace[HoldComplete[coefficient],
 HoldPattern[HoldComplete[Part[rows_,index_,2]]]:>rows[[index,1]]];
 input=coefficient;
 qqProgressUpdate[<|"Master"->If[Head[master]===HoldComplete,Missing["NotAvailable"],master],"InputLeaves"->LeafCount[input]|>];
 qqSimplifyCoefficient[input]];
SetAttributes[qqSelfDetailedEvaluate,HoldAll];
qqSelfDetailedEvaluate[expression_] := Module[{held=HoldComplete[expression]},
 If[MatchQ[held,HoldComplete[Factor[Cancel[Together[_]]]]],
 Replace[held,HoldPattern[HoldComplete[Factor[Cancel[Together[x_]]]]]:>qqSelfRationalProgress[x]],
 qqProgressOperation["Evaluate checkpoint"];expression]];
SetAttributes[qqSelfCheckpoint,HoldAll];
qqSelfCheckpoint[dir_,name_,key_,expression_,seconds_] := Module[{file,r,saved},
 file=FileNameJoin[{dir,name<>".wxf"}];
 If[StringMatchQ[name,"m0-sum-"~~DigitCharacter..],qqSelfProgress["M0Sum",ToExpression[StringDrop[name,7]],3]];
 qqProgressUpdate[<|"Checkpoint"->name,"CheckpointFile"->file,"StepLimitSeconds"->seconds|>];
 If[FileExistsQ[file],
 qqProgressOperation["Read completed checkpoint"];qqProgressUpdate[<|"CacheState"->"Reading"|>];
 r=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[r]||Lookup[r,"Key",None]=!=key||!KeyExistsQ[r,"Value"],Return[Failure["QQSelfCacheInvalid",<|"File"->file|>]]];
 If[!FreeQ[r["Value"],_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity|_Failure|_Missing],Return[Failure["QQSelfCacheInvalid",<|"File"->file|>]]];
 qqProgressUpdate[<|"OperationFinished"->AbsoluteTime[],"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];
 Return[r["Value"]]];
 qqProgressUpdate[<|"CacheState"->"Computing"|>];
 r=timed["QQSelf:"<>name,qqSelfDetailedEvaluate[expression],seconds];If[FailureQ[r],Return[r]];
 If[!FreeQ[r,_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity|_Failure|_Missing],Return[Failure["QQSelfCheckpointIncomplete",<|"Checkpoint"->name|>]]];
 qqProgressOperation["Save completed checkpoint"];saved=atomicWXF[file,<|"Key"->key,"Value"->r|>];
 If[FailureQ[saved],saved,
 qqProgressUpdate[<|"OperationFinished"->AbsoluteTime[],"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>];r]];
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
(* Linear restoration preserves completed physical checkpoints. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringRestore.wl"}]];
SetAttributes[qqSelfMonitorBody,HoldAll];
qqSelfMonitorBody[expression_] := Internal`InheritedBlock[{RGZWorkflow`Engine`rgzQQM0Restore},
 RGZWorkflow`Engine`rgzQQM0Restore[finite_]:=qqRestoreProgress[finite];expression];
SetAttributes[qqSelfMonitor,HoldAll];
qqSelfMonitor[expression_,display_]:=If[TrueQ[$Notebooks],
 Monitor[qqSelfMonitorBody[expression],Dynamic[qqSelfProgressPanel[],UpdateInterval->1,TrackedSymbols:>{$qqProgress}]],qqSelfMonitorBody[expression]];

Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringPartsProgress.wl"}]];
