(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
row=<|"编号"->1,"状态"->"未归零"|>;
Block[{RGZWorkflow`Private`loadPTEngine,RGZWorkflow`Engine`RGZPTFixedCompare,RGZWorkflow`Engine`rgzGaugeCompletedRows},
 RGZWorkflow`Private`loadPTEngine[]:=Null;
 RGZWorkflow`Engine`RGZPTFixedCompare[___]:=<|"状态"->"只检查了选定系数","本次检查项数"->1,"统计"-><|"未归零"->1|>,"残差分项"->{row}|>;
 success=RGZGaugeCheck[60];
 RGZWorkflow`Engine`rgzGaugeCompletedRows={row};
 RGZWorkflow`Engine`RGZPTFixedCompare[___]:=Failure["FixedDenCoefficientStopped",<|"编号"->2,"原因"->Failure["BatchCacheWriteFailed",<|"原因标签"->"BinaryCacheWriteTimeout"|>]|>];
 stopped=RGZGaugeCheck[60];
 report=TestReport[{
 VerificationTest[success["FullNAVIncluded"],False,TestID->"CandidateScopeExplicit"],
 VerificationTest[RGZResult["GaugeStopped"]["原因"][[1]],"BatchCacheWriteFailed",TestID->"StopCauseStored"],
 VerificationTest[RGZResult["GaugePartial"]["Rows"],{row},TestID->"CompletedRowsStoredSeparately"],
 VerificationTest[RGZResult["Gauge"]["FullNAVIncluded"],False,TestID->"LastSuccessfulReportPreserved"]}];
 Print[<|"Passed"->report["TestsSucceededCount"],"Failed"->report["TestsFailedCount"]|>];
 RGZReport[stopped];
 Exit[If[report["TestsFailedCount"]===0,0,1]]];
