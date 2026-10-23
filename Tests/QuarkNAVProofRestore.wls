(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"qq-nav-proof-"<>CreateUUID[]}],"Display"->"Quiet"];
Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],{},"WXF"];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
Internal`InheritedBlock[{RGZWorkflow`Private`navFormulaFingerprint},
 Clear[RGZWorkflow`Private`navFormulaFingerprint];RGZWorkflow`Private`navFormulaFingerprint[]:=123;
 file=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"RGZ_NAV_pinch_"<>IntegerString[123,16,64],"result.wxf"}];
 CreateDirectory[DirectoryName[file]];
 packet=<|"Schema"->"RGZNAVPinch/1","ConnectionCount"->1,"Connections"->{<|"ExactPolynomialReconstruction"->True|>},"Kernel"->0|>;
 clear[]:=(RGZWorkflow`Private`$results=KeyDrop[RGZWorkflow`Private`$results,{"NAVPinch"}]);
 Export[file,packet,"WXF"];clear[];
 record["CurrentCompletedCacheHydratesMemory",RGZWorkflow`Private`qqPVRestoreNAVProof[]&&RGZResult["NAVPinch"]["Fingerprint"]===123];
 DeleteFile[file];record["ValidMemoryNeedsNoDisk",RGZWorkflow`Private`qqPVRestoreNAVProof[]];clear[];
 record["AbsentCacheDoesNotCalculate",!RGZWorkflow`Private`qqPVRestoreNAVProof[]&&MissingQ[RGZResult["NAVPinch"]]];
 Do[Export[file,Join[packet,bad],"WXF"];clear[];
 record[name,!RGZWorkflow`Private`qqPVRestoreNAVProof[]&&MissingQ[RGZResult["NAVPinch"]]],
 {test,{{"FailedReconstructionRejected",<|"Connections"->{<|"ExactPolynomialReconstruction"->False|>}|>},
 {"WrongCountRejected",<|"ConnectionCount"->2|>},{"OldFingerprintRejected",<|"Fingerprint"->122|>},
 {"WrongSchemaRejected",<|"Schema"->"old"|>},{"IncompleteKernelRejected",<|"Kernel"->$Aborted|>}}},
 {name,{test[[1]]}},{bad,{test[[2]]}}];
 old=Join[packet,<|"Fingerprint"->122|>];RGZWorkflow`Private`storeResult["NAVPinch",old];DeleteFile[file];
 record["UnavailableCacheCannotRelabelOldEvidence",!RGZWorkflow`Private`qqPVRestoreNAVProof[]&&RGZResult["NAVPinch"]===old];
];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
