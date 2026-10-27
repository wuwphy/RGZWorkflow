(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Guard old completion flags without changing physical diagram fingerprints. *)
DownValues[qqFFAssembleBare]=DownValues[RGZQQAmplitude]/.RGZQQAmplitude->qqFFAssembleBare;
RGZQQAmplitude[] := Module[{job=RGZResult["QQ"],bad},
 If[AssociationQ[job]&&(!And@@(KeyExistsQ[job,#]&/@{"Tree","OnShellSigmaV","Reduced"})||!AssociationQ[job["Reduced"]]||(TrueQ[Lookup[job,"SelfPrepared",False]]&&!KeyExistsQ[job,"MixedSelfEnergy"])),Return[Failure["QQAmplitudeIncomplete",<|"Action"->"RGZQQStart[] and recompute missing components."|>]]];
 If[AssociationQ[job],bad=Select[Join[{"Tree","OnShellSigmaV"},If[TrueQ[Lookup[job,"SelfPrepared",False]],{"MixedSelfEnergy"},{}]],
 KeyExistsQ[job,#]&&qqFFInvalidQ[job[#]]&];
 If[AssociationQ[Lookup[job,"Reduced",None]],bad=Join[bad,Select[Keys[job["Reduced"]],qqFFInvalidQ[job["Reduced"][#]]&]]];
 If[bad=!={},Return[Failure["QQAmplitudeIncomplete",<|"Parts"->bad,"Action"->"RGZQQRepairIncomplete[600]; then RGZQQAmplitude[]"|>]]]];
 Block[{$IterationLimit=Infinity},qqFFAssembleBare[]]];
qqRepairArchive[file_,valid_] := Module[{p,dest},
 If[!FileExistsQ[file],Return[False]];p=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[TrueQ[valid[p]],Return[False]];
 dest=file<>".invalid-"<>CreateUUID[];RenameFile[file,dest];True];
qqRepairSelfFromFinite[job_,seconds_] := Module[{key,dir,packets={},file,r,scalar,finiteRows,restored={},pairs,values={},answer,stop=Unique["qqRepairSelfStop"]},Catch[
 If[job["RegulatorMass"]=!=0,Throw[Failure["QQRepairSelfRequiresM0",<|"Action"->"Prepare the finite-regulator self energy again."|>],stop]];
 dir=FileNameJoin[{job["Directory"],"self"}];key=Hash[{job["Fingerprint"],job["MixedSourceHash"]},"SHA256"];
 Do[file=FileNameJoin[{dir,"m0-sum-"<>ToString[i]<>".wxf"}];
 If[!FileExistsQ[file],Throw[Failure["QQFiniteCheckpointRequired",<|"File"->file|>],stop]];
 r=Import[file,"WXF"];
 If[!AssociationQ[r]||Lookup[r,"Key",None]=!=key||qqFFInvalidQ[Lookup[r,"Value",$Failed]],Throw[Failure["QQFiniteCheckpointInvalid",<|"File"->file|>],stop]];
 AppendTo[packets,r["Value"]],{i,3}];
 If[Take[packets,2]=!={0,0},Throw[Failure["QQSelfM0Divergent",<||>],stop]];
 Do[qqRepairArchive[file,Function[p,AssociationQ[p]&&Lookup[p,"Key",None]===key&&!qqFFInvalidQ[Lookup[p,"Value",$Failed]]]],
 {file,FileNames["gauge*.wxf",dir]~Join~{FileNameJoin[{dir,"connected.wxf"}]}}];
 r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Throw[r,stop]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Throw[r,stop]];
 (* Restoration is linear in the masters. Never form a common denominator
    across all thirteen finite coefficients just to restore their symbols. *)
 finiteRows=timed["QQRepair:SplitFiniteM0",RGZWorkflow`Engine`RGZGaugePVPairs[RGZResidual39Diagnostic`Private`publicMaster[packets[[3]]]],seconds];
 If[FailureQ[finiteRows]||qqFFInvalidQ[finiteRows],Throw[Failure["QQRepairFiniteRestore",<|"Cause"->finiteRows|>],stop]];
 Do[qqProgressOperation["Restore finite master "<>ToString[i]<>" / "<>ToString[Length[finiteRows]]];
 r=timed["QQRepair:RestoreFiniteM0:"<>ToString[i],qqRestoreProgress[Times@@finiteRows[[i]]],seconds];
 If[FailureQ[r]||qqFFInvalidQ[r],Throw[Failure["QQRepairFiniteRestore",<|"Cause"->r|>],stop]];AppendTo[restored,r],{i,Length[finiteRows]}];
 scalar=Total[restored];
 pairs=qqSelfCheckpoint[dir,"gauge-pairs",key,RGZWorkflow`Engine`RGZGaugePVPairs[scalar],seconds];If[FailureQ[pairs],Throw[pairs,stop]];
 Do[qqSelfProgress["GaugeCoefficient",i,Length[pairs]];
 r=qqSelfCheckpoint[dir,"gauge-"<>ToString[i],key,RGZWorkflow`Engine`rgzQQGaugeValue[Times@@pairs[[i]],job["Gauge"],seconds],seconds];
 If[FailureQ[r],Throw[r,stop]];AppendTo[values,r],{i,Length[pairs]}];
 answer=qqSelfCheckpoint[dir,"connected",key,
 (-FeynCalc`SMP["g_s"]^2 RGZWorkflow`Engine`rgzQQBornCurrent[RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu]
 RGZWorkflow`Engine`RGZLoopPT[RGZWorkflow`Engine`p,RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu] Total[values])/.
 {RGZWorkflow`Engine`p->RGZWorkflow`Engine`qqK3-RGZWorkflow`Engine`qqK1,RGZWorkflow`Engine`M->job["RegulatorMass"]},seconds];answer,stop]];
RGZQQRepairIncomplete[seconds_:600] := engineEvaluation@Block[{$IterationLimit=Infinity},Module[{job=RGZResult["QQ"],r,file,bad={},repaired={},valid,now=AbsoluteTime[]},
 If[!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["TimeLimit",<||>]]];
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[job["Fingerprint"]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 $qqProgress=<|"Stage"->"QQRepair","Status"->"Computing","RunStarted"->now,"ItemStarted"->now,"StepLimitSeconds"->seconds|>;
 r=loadPTEngine[];If[FailureQ[r],Return[r]];
 If[!TrueQ[Lookup[job,"SelfPrepared",False]]||qqFFInvalidQ[job["MixedSelfEnergy"]],
 r=qqRepairSelfFromFinite[job,seconds];If[FailureQ[r]||qqFFInvalidQ[r],Return[If[FailureQ[r],r,Failure["QQSelfIncomplete",<||>]]]];
 AssociateTo[job,{"MixedSelfEnergy"->r,"SelfPrepared"->True}];invalidateQQDerived[];storeResult["QQ",job];AppendTo[repaired,"MixedSelfEnergy"]];
 Do[file=FileNameJoin[{job["Directory"],name<>"-reduced.wxf"}];
 valid=Function[p,AssociationQ[p]&&Lookup[p,"Fingerprint",None]===job["Fingerprint"]&&Lookup[p,"Part",None]===name&&!qqFFInvalidQ[Lookup[p,"Expression",$Failed]]];
 If[qqRepairArchive[file,valid]||!KeyExistsQ[job["Reduced"],name]||qqFFInvalidQ[job["Reduced"][name]],AppendTo[bad,name]],
 {name,{"AV1","AV2","Box1","Box2","NAV1","NAV2"}}];
 If[Join[bad,repaired]==={},Return[<|"Status"->"Complete","Name"->"QQRecovery","Statistics"-><|"Reused"->True,"Repaired"->{}|>|>]];
 invalidateQQDerived[];
 r=atomicWXF[FileNameJoin[{job["Directory"],"job.wxf"}],job];If[FailureQ[r],Return[r]];
 Do[r=RGZQQRunPart[name,seconds];If[FailureQ[r],Break[]];AppendTo[repaired,name],{name,bad}];
 If[FailureQ[r],Return[r]];
 <|"Status"->"Complete","Name"->"QQRecovery","Statistics"-><|"Repaired"->repaired,"ValidCachesPreserved"->True|>|>]];
