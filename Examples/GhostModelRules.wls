(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Fresh FeynCalc kernel, after GhostActionRules.wls. *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
 RGZWorkflow`RGZInitialize[Environment["RGZ_GHOST_RULE_WORK"],"Display"->"Quiet"];
 If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
rgzActionPacket=Import[FileNameJoin[{Environment["RGZ_GHOST_RULE_WORK"],"ghost-action.wxf"}],"WXF"];
If[!TrueQ[rgzActionPacket["Passed"]],Exit[1]];
RGZWorkflow`Private`engineGet["Tree/VertexRules.wl",False];
rgzGhostComparisons=rgzCheckOne/@rgzIDs;
Print[KeyTake[#,{"ExternalLegs","Status","FA-FR"}]&/@rgzGhostComparisons];
(* The model has now been initialized by actual InsertFields/CreateFeynAmp.
   Compare its ordered free ghost covariances with the FR action inverses. *)
rgzGhostPropChecks={
 RGZUPropagator[0,rgzTestMomentum,rgzTestL1,rgzTestL2]===I FeynArts`FAPropagatorDenominator[rgzTestMomentum,0],
 RGZUPropagator[RGZAuxiliaryMass,rgzTestMomentum,rgzTestL1,rgzTestL2]===
 -I Global`FAMetricTensor[rgzTestL1,rgzTestL2] FeynArts`FAPropagatorDenominator[rgzTestMomentum,\[Mu]]};
rgzGhostModelPassed=Length[rgzGhostComparisons]===4&&And@@rgzGhostPropChecks&&AllTrue[rgzGhostComparisons,TrueQ[#["FA-FR"]===0]&];
Export[FileNameJoin[{Environment["RGZ_GHOST_RULE_WORK"],"ghost-model.wxf"}],
 <|"VertexComparisons"->rgzGhostComparisons,"PropagatorChecks"->rgzGhostPropChecks,"Passed"->rgzGhostModelPassed|>,"WXF"];
Print["Action to actual FA ghost rules: ",rgzGhostModelPassed];Exit[If[TrueQ[rgzGhostModelPassed],0,1]];
