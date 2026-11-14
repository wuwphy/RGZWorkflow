(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZLoadFeynCalc[];
 RGZWorkflow`Private`loadQuarkEngine[];RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
checks=<||>;rec[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
RGZWorkflow`Private`atomicWXF[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],{}];
key=RGZWorkflow`Private`qqJobFingerprint[0,0];
makeJob[measure_] := <|"Fingerprint"->key,"Directory"->CreateDirectory[],"Gauge"->0,"RegulatorMass"->0,"LoopMeasureConversion"->measure,
 "Integrands"-><|"AV1"->FCI[FAD[{q,\[Mu]}]]|>,"NAVSource"-><|"TransverseIntegrand"->1|>,"Reduced"-><||>,"ReducedProvenance"-><||>|>;
Do[j=makeJob[measure];RGZWorkflow`Private`storeResult["QQ",j];r=RGZWorkflow`RGZQQRunPart["AV1",30];
 rec["RejectMeasure"<>ToString[measure,InputForm],MatchQ[r,Failure["QQMeasureConventionRequired",_]]&&!FileExistsQ[FileNameJoin[{j["Directory"],"AV1-reduced.wxf"}]]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"],
 {measure,{0,1,2/(2 Pi)^4,Infinity}}];
j=makeJob[1/(2 Pi)^4];RGZWorkflow`Private`storeResult["QQ",j];
r=RGZWorkflow`RGZQQRunPart["AV1",30];
rec["NativeMeasureAppliedOnce",AssociationQ[r]&&RGZWorkflow`RGZResult["QQ"]["Reduced"]["AV1"]===I A0[\[Mu]^2]/(16 Pi^2)];
r=RGZWorkflow`RGZQQRunPart["AV1",30];rec["CacheReuseRemainsVisible",AssociationQ[r]&&RGZWorkflow`RGZResult["QQProgress"]["CacheState"]==="Reused"];
Internal`InheritedBlock[{RGZWorkflow`Private`qqRunPartDetailed},Clear[RGZWorkflow`Private`qqRunPartDetailed];
 RGZWorkflow`Private`qqRunPartDetailed[___]:=Abort[];
 r=CheckAbort[RGZWorkflow`RGZQQRunPart["AV1",10],$Aborted];
 rec["OuterAbortRecordedAsStopped",FailureQ[r]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"]];
Do[Internal`InheritedBlock[{RGZWorkflow`Private`qqRunPartDetailed},Clear[RGZWorkflow`Private`qqRunPartDetailed];RGZWorkflow`Private`qqRunPartDetailed[___]:=bad;
 r=RGZWorkflow`RGZQQRunPart["AV1",10];rec["IncompleteReturnRejected"<>ToString[bad,InputForm],FailureQ[r]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"]],
 {bad,{$Aborted,$TimedOut,Indeterminate,Null,<|"Status"->"Computing"|>}}];
Internal`InheritedBlock[{RGZWorkflow`Private`atomicWXF},Clear[RGZWorkflow`Private`atomicWXF];RGZWorkflow`Private`atomicWXF[___]:=Abort[];
 r=RGZWorkflow`RGZQQRunPart["AV1",10];rec["SaveAbortRecordedAsStopped",FailureQ[r]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"]];
Internal`InheritedBlock[{RGZWorkflow`Private`atomicWXF},Clear[RGZWorkflow`Private`atomicWXF];RGZWorkflow`Private`atomicWXF[___]:=(Pause[0.4];<|"Status"->"Saved"|>);
 r=RGZWorkflow`RGZQQRunPart["AV1",0.1];rec["SaveTimeoutRecordedAsStopped",MatchQ[r,Failure["StageTimeout",_]]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"]];
target=FileNameJoin[{j["Directory"],"AV1-reduced.wxf"}];
Internal`InheritedBlock[{Import},Unprotect[Import];With[{f=target},Import[f,"WXF"]:=(Pause[0.4];$Failed)];
 r=RGZWorkflow`RGZQQRunPart["AV1",0.1];rec["ReadTimeoutRecordedAsStopped",MatchQ[r,Failure["StageTimeout",_]]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"]];
Internal`InheritedBlock[{Import},Unprotect[Import];With[{f=target},Import[f,"WXF"]:=Abort[]];
 r=RGZWorkflow`RGZQQRunPart["AV1",10];rec["ReadAbortRecordedAsStopped",FailureQ[r]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"]];
names={"AV1","AV2","NAV1","NAV2","Box1","Box2"};
assemblyJob=Join[j,<|"Process"->"Measure validation fixture","LoopMeasureConversion"->2/(2 Pi)^4,"Tree"->1,"OnShellSigmaV"->0,"MixedSelfEnergy"->0,"SelfPrepared"->True,
 "Integrands"->AssociationThread[names,ConstantArray[1,6]],"Reduced"->AssociationThread[names,ConstantArray[0,6]]|>];
AssociateTo[assemblyJob,"ReducedProvenance"->Association@Table[n->RGZWorkflow`Private`qqPartProof[assemblyJob,n,0],{n,names}]];
RGZWorkflow`Private`storeResult["QQ",assemblyJob];
rec["AssemblyUsesSamePhysicalMeasureRule",MatchQ[RGZWorkflow`RGZQQAmplitude[],Failure["QQMeasureConventionRequired",_]]];
r=RGZWorkflow`RGZQQRunPart["AV1",0];rec["InvalidTimeDoesNotRetainOldCompleteStatus",FailureQ[r]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"];
j=makeJob[1/(2 Pi)^4];RGZWorkflow`Private`storeResult["QQ",j];
Internal`InheritedBlock[{RGZWorkflow`Private`qqPartTimed},Clear[RGZWorkflow`Private`qqPartTimed];
 RGZWorkflow`Private`qqPartTimed[stage_,expression_,seconds_]:=If[StringStartsQ[stage,"QQPoleSplit:"],Failure["QuarkPoleReconstruction",<|"Denominator"->testDenominator|>],expression];
 r=RGZWorkflow`RGZQQRunPart["AV1",10];
 rec["PoleSplitRetainsActualCause",MatchQ[r,Failure["QQPoleSplitIncomplete",KeyValuePattern["Cause"->Failure["QuarkPoleReconstruction",_]]]]]];
Print["SUMMARY ",checks];Exit[If[And@@Values[checks],0,1]];
