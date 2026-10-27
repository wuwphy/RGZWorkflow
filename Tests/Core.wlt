(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Run in a fresh native Mathematica kernel. No third-party packages required. *)
Get[FileNameJoin[{If[ValueQ[Global`$RGZTestProject],Global`$RGZTestProject,DirectoryName[DirectoryName[$InputFileName]]],"RGZWorkflow.wl"}]];
VerificationTest[RGZWorkflow`Private`$loaded==={} && RGZStatus[]===<||>,True,TestID->"LoadingDoesNotCompute"]
VerificationTest[FailureQ[RGZMixedStart[]],True,TestID->"InitializationGuard"]
VerificationTest[RGZRegisterStage["Double",Function[{x},2 x]];RGZRunExtension["Double",3],6,TestID->"ExplicitExtension"]
VerificationTest[FailureQ[RGZRunExtension["Missing",3]],True,TestID->"UnknownExtensionGuard"]
VerificationTest[RGZTreeBenchmark[60]["Status"],"Passed",TestID->"IndependentTreeKernelInversion"]
VerificationTest[Block[{Global`m=1,Global`s=2},RGZTreeBenchmark[60]["Status"]],"Passed",TestID->"BenchmarkIndependentOfGlobalAssignments"]
VerificationTest[FreeQ[RGZWorkflow`Private`engineContextPath[],"Global`"|"RGZWorkflow`"],True,TestID->"EngineDoesNotSearchUserContext"]
VerificationTest[AllTrue[Cases[RGZResult["TreeBenchmark"],x_Symbol /; Context[x]==="Global`",Infinity],False&],True,TestID->"BenchmarkContainsNoGlobalSymbols"]
VerificationTest[RGZWorkflow`Private`engineCode["Context[p]"],"RGZWorkflow`Engine`",TestID->"PrivateMomentumResolution"]
VerificationTest[RGZWorkflow`Private`engineCode["p"]=!=Global`p,True,TestID->"EngineMomentumIsNotUserMomentum"]
VerificationTest[RGZInitialize[FileNameJoin[{$TemporaryDirectory,"RGZWorkflow-test-"<>CreateUUID[]}]]["Status"],"Initialized",TestID->"InitializeBundledModel"]
VerificationTest[RGZSave[]["Status"],"Saved",TestID->"SessionWXFWrite"]
VerificationTest[RGZRestore[]["Status"],"Restored",TestID->"SessionWXFRead"]
VerificationTest[Module[{file=FileNameJoin[{RGZConfiguration[]["ResultsDirectory"],"OldSession.wxf"}]},Export[file,<|"Schema"->"RGZWorkflowSession/1"|>,"WXF"];RGZRestore["File"->file][[1]]],"SessionSchema",TestID->"RejectOldGlobalSession"]

VerificationTest[Block[{$Packages={"FeynCalc`"}},FailureQ[RGZLoadFeynRules[]]],True,TestID->"SeparateKernels"]

VerificationTest[Block[{Global`rgzDynamicProbe=99},RGZWorkflow`Private`engineEvaluation[ToExpression["rgzDynamicProbe",InputForm,Function[sym,Context[sym],HoldAll]]]],"RGZWorkflow`Engine`",TestID->"RuntimeSymbolsIgnoreGlobalDefinitions"]
VerificationTest[Module[{before={$Context,$ContextPath}},RGZWorkflow`Private`engineEvaluation[Null];{$Context,$ContextPath}===before],True,TestID->"RuntimeContextRestored"]
VerificationTest[RGZWorkflow`Private`candidateName /@ {"box","nav","av","qse"},{"box","navAll","avAll","qseAll"},TestID->"CandidateAliases"]
VerificationTest[RGZRunPart["bad","Project"][[1]],"CandidateStage",TestID->"InvalidCandidateBeforeDependencyLoad"]
VerificationTest[Block[{RGZWorkflow`Engine`rgzIDs={1,2}},RGZWorkflow`Private`selectedIDs[{All}][[2,"Action"]]],"Use \"IDs\" -> All (without braces), or an integer list such as {1}.",TestID->"VertexIDsActionableDiagnostic"]

VerificationTest[Module[{file=FileNameJoin[{RGZConfiguration[]["ResultsDirectory"],"LegacyPT.wxf"}],packet,r},
 RGZSave["File"->file];packet=Import[file,"WXF"];
 packet["PTJob"]=<|"Parts"-><||>|>;
 packet["Results"]=Join[packet["Results"],<|"Gauge"->"old gauge result","Pinch"->"old pinch result","MassLimit:QCD"->"old limit","QCD"-><|"kept"->True|>|>];
 Export[file,packet,"WXF"];r=RGZRestore["File"->file];
 {r["PTStatus"],Sort[r["InvalidatedResults"]],RGZResult["QCD"],AssociationQ[RGZWorkflow`Engine`rgzPTJob]}],
 {"RestartRequired",{"Gauge","MassLimit:QCD","Pinch"},<|"kept"->True|>,False},TestID->"RestoreKeepsQCDAndRejectsLegacyPinch"]

VerificationTest[{"Mode","NAVMode"}/.Options[RGZPTStart],{"FourPinch","FullMatrix"},TestID->"DefaultPinchIncludesFullNAV"]

VerificationTest[RGZDependencies[]["ModelContent"]["ClosedQuarkFlavors"],2,TestID->"TwoDynamicalMasslessQuarksDeclared"]
VerificationTest[RGZDependencies[]["ModelContent"]["GhostClasses"],{1,2,3,4},TestID->"ConstraintGhostInBaseModelDeclaration"]
VerificationTest[RGZDependencies[]["ModelContent"]["JacobianGhostIncluded"],True,TestID->"ModelAlreadyIncludesJacobianGhost"]

VerificationTest[Module[{file=FileNameJoin[{RGZConfiguration[]["ResultsDirectory"],"OtherModel.wxf"}]},Export[file,<|"Schema"->"RGZWorkflowSession/2","Version"->RGZWorkflow`$RGZWorkflowVersion,"ModelFingerprint"->0|>,"WXF"];RGZRestore["File"->file][[1]]],"DifferentModel",TestID->"RejectSessionFromOldPhysicalModel"]
