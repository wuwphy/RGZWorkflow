(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Native FR integration checks; run in a fresh kernel. Optional RGZ_TEST_CORES=2 enables workers. *)
rgzTestProject=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{rgzTestProject,"RGZWorkflow.wl"}]];
rgzTestWork=Environment["RGZ_TEST_WORK"];
If[!StringQ[rgzTestWork] || rgzTestWork==="",rgzTestWork=FileNameJoin[{$TemporaryDirectory,"RGZWorkflow-integration-0.3.0"}]];
rgzTestCores=Environment["RGZ_TEST_CORES"];
rgzTestCores=If[StringQ[rgzTestCores] && StringMatchQ[rgzTestCores,DigitCharacter..],FromDigits[rgzTestCores],0];
Global`FR$Parallel=rgzTestCores>0;
If[rgzTestCores>0,Global`FR$KernelNumber=rgzTestCores];
RGZInitialize[rgzTestWork];
rgzTestReport=TestReport[{
 VerificationTest[RGZTreeBenchmark[60]["Status"],"Passed",TestID->"PrivateTreeBenchmark"],
 VerificationTest[RGZLoadFeynRules[]["Status"],"FRModelLoaded",TestID->"NativeFRLoad"],
 VerificationTest[RGZSymbolAudit[]["Status"],"Passed",TestID->"MasterWorkerSymbolsAndReplacements"],
 VerificationTest[Length[RGZResult["SymbolAudit"]["Workers"]],rgzTestCores,TestID->"RequestedWorkerCount"],
 VerificationTest[RGZExtractTree[120]["Status"],"Complete",TestID->"ActualFRQuadraticKernel"],
 VerificationTest[RGZExportVertices[300]["Statistics"],<|3->25,4->23|>,TestID->"ActualFRVertices"],
 VerificationTest[If[rgzTestCores===0,True,Module[{detected},
  detected=Internal`WithLocalSettings[
   ParallelEvaluate[RGZWorkflow`Private`rgzSavedAuditParameters=FeynRules`M$Parameters;
    FeynRules`M$Parameters=Append[FeynRules`M$Parameters,False],DistributedContexts->None],
   RGZSymbolAudit[]["Status"]==="Failed",
   ParallelEvaluate[FeynRules`M$Parameters=RGZWorkflow`Private`rgzSavedAuditParameters;
    Remove["RGZWorkflow`Private`rgzSavedAuditParameters"],DistributedContexts->None]];
  detected && RGZSymbolAudit[]["Status"]==="Passed"]],True,TestID->"DetectWorkerRuleMismatchWithoutOverwritingIt"],
 VerificationTest[Names["Global`m"|"Global`s"|"Global`p"|"Global`q"|"Global`\[Mu]"|"Global`\[Alpha]"],{},TestID->"NoGlobalPhysicalSymbolLeak"]
}];
Print[<|"Succeeded"->rgzTestReport["TestsSucceededCount"],"Failed"->rgzTestReport["TestsFailedCount"],"WorkDirectory"->rgzTestWork|>];
If[rgzTestReport["TestsFailedCount"]>0,Print[Map[Function[test,<|"TestID"->test["TestID"],"ActualOutput"->test["ActualOutput"],"ActualMessages"->test["ActualMessages"]|>],rgzTestReport["TestsFailed"]]]];
If[rgzTestCores>0,CloseKernels[]];
Exit[If[rgzTestReport["TestsFailedCount"]===0,0,1]];
