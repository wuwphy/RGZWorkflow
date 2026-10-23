(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Run after FeynRules.wls, in a separate fresh kernel with the same RGZ_TEST_WORK. *)
rgzTestProject=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{rgzTestProject,"RGZWorkflow.wl"}]];
rgzTestWork=Environment["RGZ_TEST_WORK"];
If[!StringQ[rgzTestWork] || rgzTestWork==="",rgzTestWork=FileNameJoin[{$TemporaryDirectory,"RGZWorkflow-integration-0.3.0"}]];
RGZInitialize[rgzTestWork];
rgzTestReport=TestReport[{
 VerificationTest[Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail]["Status"],"FeynCalcLoaded",TestID->"NativeFCLoad"],
 VerificationTest[RGZCheckPropagators[120]["Statistics"],<|"Minkowski"-><|"\:901a\:8fc7"->69|>,"Kernel"-><|"\:901a\:8fc7"->69|>|>,TestID->"All69Propagators"],
 VerificationTest[RGZResult["PropagatorConnections"],Association@Table["Ah:"<>ToString[j]->0,{j,8}],TestID->"GeneratedCovarianceDerivativeVertexConnections"],
 VerificationTest[RGZWorkflow`Private`engineCode["rgzDifference[rgzRational[RGZInsertPoleMasses[RGZSplitPropagator[rgzInsertionToCovariance[rgzRows[[4]][\"FeynCalc振幅\"]]]]],rgzRows[[4]][\"有理式\"]]"],0,TestID->"MixedPoleDecompositionRespectsCovarianceMomentum"],
 VerificationTest[RGZCheckVertices[30,"IDs"->{1}]["Statistics"],<|"FA=FR\:ff1b\:672a\:5f55\:5165\:624b\:63a8"->1|>,TestID->"FAFRVertexIdentity"],
 VerificationTest[AssociationQ[RGZMixedStart[]],True,TestID->"PrivateMixedMatrixManifest"],
 VerificationTest[RGZCheckOrders[60,"IDs"->{1},"Mode"->"Compare"]["Statistics"],<|"通过"->6|>,TestID->"SixLabeledLegOrders"],
 VerificationTest[RGZQCDBenchmark[600]["Statistics"],<|"计算的五类规范差总和 / I11"->0,"抵消检查"->"通过"|>,TestID->"QCDGaugeDifferenceCancelsAfterTreeChecks"],
 VerificationTest[Join[Names["Global`Lor"~~DigitCharacter..],Names["Global`LoopMom"~~DigitCharacter..],Names["Global`cacheFunc*"]],{},TestID->"NoDynamicDependencySymbolsInGlobal"],
 VerificationTest[RGZRunPart["nav","投影",30][[1]],"PTStartFirst",TestID->"CandidateAliasReachesPTGuard"]
}];
Print[<|"Succeeded"->rgzTestReport["TestsSucceededCount"],"Failed"->rgzTestReport["TestsFailedCount"]|>];
If[rgzTestReport["TestsFailedCount"]>0,Print[rgzTestReport["TestsFailed"]]];
Exit[If[rgzTestReport["TestsFailedCount"]===0,0,1]];
