(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{project,"Work"}]];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
prop=RGZCheckPropagators[120];Print[prop];
qcd=RGZQCDBenchmark[600];Print[qcd];
report=TestReport[{
 VerificationTest[If[AssociationQ[prop],prop["Statistics"],prop],<|"Minkowski"-><|"通过"->69|>,"Kernel"-><|"通过"->69|>|>,TestID->"Actual69Propagators"],
 VerificationTest[If[AssociationQ[qcd],qcd["Statistics"],qcd],<|"计算的五类规范差总和 / I11"->0,"抵消检查"->"通过"|>,TestID->"ActualQCDSelfPlusCandidates"]}];
Print[<|"Passed"->report["TestsSucceededCount"],"Failed"->report["TestsFailedCount"]|>];
Exit[If[report["TestsFailedCount"]===0,0,1]];
