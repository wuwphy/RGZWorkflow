(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Symbol["Global`RGZNAVSum"];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{project,"Work"}]];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
savedPath=$ContextPath;$ContextPath={"System`"};
Symbol["Global`$LoadAddOns"];Symbol["Global`$FeynCalcStartupMessages"];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
 $ContextPath=savedPath;
report=TestReport[{
 VerificationTest[Join[Names["Global`name"],Names["Global`sym"]],{},TestID->"LoaderInternalSymbolsStayPrivate"],
 VerificationTest[Names["Global`RGZNAVSum"],{},TestID->"RemoveUndefinedAPIPlaceholder"],
 VerificationTest[Join[Names["Global`$LoadAddOns"],Names["Global`$FeynCalcStartupMessages"]],{},TestID->"ReloadDoesNotRecreateFCStartupGlobals"],
 VerificationTest[RGZLoadFeynCalc[]["Status"],"FeynCalcLoaded",TestID->"FCStillReadyAfterReload"]
}];
Print[<|"Passed"->report["TestsSucceededCount"],"Failed"->report["TestsFailedCount"]|>];
Exit[If[report["TestsFailedCount"]===0,0,1]];
