(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`Private`loadQuarkEngine[];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
checks=<||>;rec[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
RGZWorkflow`Private`atomicWXF[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],{}];
names={"AV1","AV2","Box1","Box2","NAV1","NAV2"};
job=<|"Fingerprint"->RGZWorkflow`Private`qqJobFingerprint[0,0],"Directory"->CreateDirectory[],"Process"->"Assembly identity fixture",
 "Gauge"->0,"RegulatorMass"->0,"LoopMeasureConversion"->1/(2 Pi)^4,"Tree"->10,"MixedSelfEnergy"->20,"OnShellSigmaV"->30,"SelfPrepared"->True,
 "Integrands"->AssociationThread[names,Range[6]],"Reduced"->AssociationThread[names,Range[6]]|>;
proof[j_] := Join[j,<|"ReducedProvenance"->Association@Table[n->RGZWorkflow`Private`qqPartProof[j,n,j["Reduced"][n]],{n,names}]|>];job=proof[job];
RGZWorkflow`Private`storeResult["QQ",job];r=RGZWorkflow`RGZQQAmplitude[];packet=RGZWorkflow`RGZResult["QQAmplitude"];
rec["AssemblyRecordsActualInputs",AssociationQ[r]&&packet["AssemblyInputHash"]===RGZWorkflow`Private`qqAssemblyInputHash[job]];
rec["UnchangedAutomaticMemoryAccepted",AssociationQ[RGZWorkflow`Private`qqFFReadPacket[Automatic]]];
file=FileNameJoin[{job["Directory"],"bare-amplitude.wxf"}];
Do[
 changed=If[MemberQ[names,field],proof[Join[job,<|"Reduced"->Append[job["Reduced"],field->99]|>]],Join[job,<|field->99|>]];
 RGZWorkflow`Private`storeResult["QQ",changed];RGZWorkflow`Private`storeResult["QQAmplitude",packet];
 rec["ChangedMemoryRejected"<>field,MatchQ[RGZWorkflow`Private`qqFFReadPacket[Automatic],Failure["QQAmplitudeInputsChanged",_]]];
 RGZWorkflow`Private`invalidateQQDerived[];
 rec["ChangedDiskRejected"<>field,MatchQ[RGZWorkflow`Private`qqFFReadPacket[Automatic],Failure["QQAmplitudeInputsChanged",_]]],
 {field,Join[{"Tree","MixedSelfEnergy","OnShellSigmaV"},names]}];
rec["AutomaticAuditAlsoRejectsStaleAssembly",MatchQ[RGZWorkflow`Private`qqAuditRead[Automatic,"QQAmplitude"],Failure["QQAmplitudeInputsChanged",_]]];
rec["ExplicitHistoricalFileRemainsIndependent",AssociationQ[RGZWorkflow`Private`qqFFReadPacket[file]]];
RGZWorkflow`Private`storeResult["QQ",changed];r=RGZWorkflow`RGZQQAmplitude[];
rec["ReassemblyAcceptedWithoutNewTask",AssociationQ[r]&&RGZWorkflow`RGZResult["QQ"]["Fingerprint"]===job["Fingerprint"]&&RGZWorkflow`RGZResult["QQAmplitude"]["AssemblyInputHash"]=!=packet["AssemblyInputHash"]&&AssociationQ[RGZWorkflow`Private`qqFFReadPacket[Automatic]]];
legacy=KeyDrop[RGZWorkflow`RGZResult["QQAmplitude"],{"AssemblyInputHash"}];RGZWorkflow`Private`atomicWXF[file,legacy];RGZWorkflow`Private`invalidateQQDerived[];
rec["UnboundLegacyDiskRequiresReassembly",MatchQ[RGZWorkflow`Private`qqFFReadPacket[Automatic],Failure["QQAmplitudeInputsChanged",_]]];
rec["ExplicitLegacyImportStillAllowed",AssociationQ[RGZWorkflow`Private`qqFFReadPacket[file]]];
Print["SUMMARY ",checks];Exit[If[And@@Values[checks],0,1]];
