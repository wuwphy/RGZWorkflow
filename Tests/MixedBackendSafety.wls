(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Native regressions for general pole identities, integral policy and
   recursively validated checkpoints. Only isolated temporary work is used. *)
If[$Context=!="RGZWorkflow`Engine`",
 With[{rgzSafetyScript=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
  Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
  If[!AssociationQ[RGZWorkflow`RGZInitialize[CreateDirectory[]]],Exit[1]];
  If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
  If[FailureQ[RGZWorkflow`Private`loadLoopEngine[]],Exit[1]];
  RGZWorkflow`Private`engineEvaluation[Get[rgzSafetyScript]]];Exit[1]];

rgzSafetyChecks=<||>;rgzSafetyRec[name_,value_]:=(AssociateTo[rgzSafetyChecks,name->TrueQ[value]];Print[name," ",TrueQ[value]]);
RGZMXSetDisplay["Quiet"];FCClearScalarProducts[];

(* The equality is in $Assumptions only: the returned expression must
   still reconstruct at a different mass point after leaving that scope. *)
Block[{$Assumptions=True},
 rgzSafetyDen=RGZLoopDelta[v];
 rgzSafetyNeutral=RGZMXSplitFactor[rgzSafetyDen,v,q];
 rgzSafetyConditional=Block[{$Assumptions=(m==\[Mu])},RGZMXSplitFactor[rgzSafetyDen,v,q]];
 rgzSafetyRec["ConditionalMassAssumptionsDoNotChangeSplit",SameQ[rgzSafetyNeutral,rgzSafetyConditional]&&!FailureQ[rgzSafetyConditional]];
 rgzSafetyRec["CallerAssumptionsRestored",$Assumptions===True];
 rgzSafetyRestored=FeynAmpDenominatorExplicit[RGZLoopPoleMasses[rgzSafetyConditional]];
 rgzSafetyPoint={m->2,\[Mu]->1,mG->2,Pair[Momentum[q,D],Momentum[q,D]]->7};
 rgzSafetyRec["ReportedGeneralMassCounterexampleFixed",FullSimplify[rgzSafetyRestored/.rgzSafetyPoint]===1/34];
 rgzSafetyRec["GeneralEmittedPoleIdentity",FullSimplify[Together[rgzSafetyRestored/.Pair[Momentum[q,D],Momentum[q,D]]->v]-1/rgzSafetyDen]===0];
 Do[rgzSafetySp=RGZMXSplitFactor[rgzSafetyCase,v,q];
  rgzSafetyBack=FeynAmpDenominatorExplicit[RGZLoopPoleMasses[rgzSafetySp]]/.Pair[Momentum[q,D],Momentum[q,D]]->v;
  rgzSafetyRec["RepeatedAndSimplePoleReconstruction"<>ToString[rgzSafetyCase,InputForm],!FailureQ[rgzSafetySp]&&FullSimplify[Together[rgzSafetyBack-1/rgzSafetyCase]]===0],
  {rgzSafetyCase,{v,(v-\[Mu]^2)^2,v (v-\[Mu]^2)}}];
 Do[rgzSafetySp=RGZMXSplitFactor[v-\[Mu]^2,v,rgzSafetyRoute];
  rgzSafetyExplicit=ExpandScalarProduct[FeynAmpDenominatorExplicit[rgzSafetySp]];
  rgzSafetyExpected=ExpandScalarProduct[FCI[1/(SPD[rgzSafetyRoute,rgzSafetyRoute]-\[Mu]^2)]];
  rgzSafetyRec["ShiftedRouteEmission"<>ToString[rgzSafetyRoute,InputForm],!FailureQ[rgzSafetySp]&&FullSimplify[Together[rgzSafetyExplicit-rgzSafetyExpected]]===0],{rgzSafetyRoute,{q-p,q+p}}];
 (* Deliberately corrupt a labelled mass only at the emitted-map boundary. *)
 Internal`InheritedBlock[{RGZLoopPoleMasses},Clear[RGZLoopPoleMasses];RGZLoopPoleMasses[x_]:=x;
  rgzSafetyRec["UnverifiedEmittedPoleLabelsRejected",MatchQ[RGZMXSplitFactor[rgzSafetyDen,v,q],Failure["PoleLabelReconstruction",_]]]]];

rgzSafetyInput=FCI[FAD[{q,0},{q,0}]];rgzSafetyTemp=CreateDirectory[];
Do[
 RGZMXSetAcceleration["Enabled"->(rgzSafetyMode==="Fast")];
 If[rgzSafetyMode==="Fast",rgzSafetyRec["AcceleratorLoaded",Length[DownValues[RGZMXIntegralUnitAccelerated]]>0]];
 If[rgzSafetyMode==="FastDisabled",RGZMXSetAcceleration["Enabled"->False]];
 rgzSafetyPath=FileNameJoin[{rgzSafetyTemp,rgzSafetyMode<>"-unit.wl"}];rgzMXLastIntegralUnit=<||>;
 rgzSafetyC0=Block[{$KeepLogDivergentScalelessIntegrals=False},RGZMXIntegralContext[]];
 rgzSafetyC1=Block[{$KeepLogDivergentScalelessIntegrals=True},RGZMXIntegralContext[]];
 rgzSafetyRec[rgzSafetyMode<>"ScalelessPoliciesHaveDifferentContexts",rgzSafetyC0=!=rgzSafetyC1];
 rgzSafetyZ0=Block[{$KeepLogDivergentScalelessIntegrals=False},RGZMXIntegralUnit[rgzSafetyInput,rgzSafetyPath,rgzSafetyC0,30]];
 rgzSafetyZ1=Block[{$KeepLogDivergentScalelessIntegrals=True},RGZMXIntegralUnit[rgzSafetyInput,rgzSafetyPath,rgzSafetyC1,30]];
 rgzSafetyFresh=Block[{$KeepLogDivergentScalelessIntegrals=True},RGZMXIntegralUnit[rgzSafetyInput,None,rgzSafetyC1,30]];
 rgzSafetyRec[rgzSafetyMode<>"DisabledConventionIsZero",rgzSafetyZ0===0];
 rgzSafetyRec[rgzSafetyMode<>"EnabledConventionRetainsLogScaleless",Block[{$KeepLogDivergentScalelessIntegrals=True},SameQ[rgzSafetyZ1,I Pi^2 B0[0,0,0]]]];
 rgzSafetyRec[rgzSafetyMode<>"CacheMatchesFreshEnabledPolicy",SameQ[rgzSafetyZ1,rgzSafetyFresh]&&rgzSafetyZ1=!=0];
 rgzSafetyAgain=Block[{$KeepLogDivergentScalelessIntegrals=True},RGZMXIntegralUnit[rgzSafetyInput,rgzSafetyPath,rgzSafetyC1,30]];
 rgzSafetyRec[rgzSafetyMode<>"ValidSamePolicyCacheReused",SameQ[rgzSafetyAgain,rgzSafetyFresh]&&rgzMXLastIntegralUnit["Status"]==="RestoreCache"];
 rgzSafetyLegacy=DeleteCases[rgzSafetyC1,{"MixedIntegralPolicy/2",___}];
 Put[<|"Version"->1,"Status"->"Complete","Input"->rgzSafetyInput,"Environment"->rgzSafetyLegacy,"Result"->123|>,rgzSafetyPath];
 rgzSafetyReplaced=Block[{$KeepLogDivergentScalelessIntegrals=True},RGZMXIntegralUnit[rgzSafetyInput,rgzSafetyPath,rgzSafetyC1,30]];
 rgzSafetyRec[rgzSafetyMode<>"UnprovenancedLegacyUnitNotReused",SameQ[rgzSafetyReplaced,rgzSafetyFresh]];
 Put[<|"Version"->1,"Status"->"Complete","Input"->rgzSafetyInput,"Environment"->rgzSafetyC1,
   "Result"->System`TerminatedEvaluation["IterationLimit"]|>,rgzSafetyPath];
 rgzSafetyRepaired=Block[{$KeepLogDivergentScalelessIntegrals=True},RGZMXIntegralUnit[rgzSafetyInput,rgzSafetyPath,rgzSafetyC1,30]];
 rgzSafetyRec[rgzSafetyMode<>"InvalidCachedIntegralRecomputed",SameQ[rgzSafetyRepaired,rgzSafetyFresh]],
 {rgzSafetyMode,{"Base","Fast","FastDisabled"}}];

rgzSafetyContext=RGZMXStageContext[];rgzSafetyJobDir=CreateDirectory[];
rgzSafetyMakeJob[] := (rgzMXJob=<|"FieldPair"->{1,1},"CacheDirectory"->rgzSafetyJobDir,
 "StageEnvironment"->rgzSafetyContext,"PerDiagram"->{<|"ID"->1,"Category"->"Fixture","UCategory"->0,
 "Amplitude"->0,"Status"-><|"Convert"->"Complete"|>,
 "StageEnvironment"-><|"Convert"->rgzSafetyContext|>,"FailedRecord"-><||>|>}|>);
Do[
 rgzSafetyMakeJob[];
 Internal`InheritedBlock[{RGZMXProject},Clear[RGZMXProject];RGZMXProject[___]:=<|"T"->rgzSafetyBad,"L"->0|>;
  rgzSafetyFirst=RGZMXRun[1,"Project",10];rgzSafetySecond=RGZMXRun[1,"Project",10];
  rgzSafetySaved=Get[FileNameJoin[{rgzSafetyJobDir,"graph_1.wl"}]];
  rgzSafetyRec["NestedInvalidStageRejected"<>ToString[rgzSafetyBad,InputForm],FailureQ[rgzSafetyFirst]&&FailureQ[rgzSafetySecond]&&
    rgzSafetySaved["Status"]["Project"]==="ComputeFailed"&&!KeyExistsQ[rgzSafetySaved,"Project"]]],
 {rgzSafetyBad,{System`TerminatedEvaluation["IterationLimit"],$TimedOut,Indeterminate,ComplexInfinity,
   {0,Failure["Nested",<||>]}}}];

rgzSafetyMakeJob[];
rgzSafetyRow=First[rgzMXJob["PerDiagram"]];
rgzSafetyRow=Join[rgzSafetyRow,<|"Project"-><|"T"->System`TerminatedEvaluation["IterationLimit"],"L"->0|>,
 "DecomposeT"->0,"IntegrateT"->0,"ScalarT"->0,
 "Status"-><|"Convert"->"Complete","Project"->"Complete","DecomposeT"->"Complete","IntegrateT"->"Complete","ScalarT"->"Complete"|>,
 "StageEnvironment"->AssociationThread[{"Convert","Project","DecomposeT","IntegrateT","ScalarT"},ConstantArray[rgzSafetyContext,5]]|>];
rgzSafetyClean=RGZMXMigrateRow[rgzSafetyRow];
rgzSafetyRec["InvalidCompletedStageAndDependentsInvalidated",Lookup[rgzSafetyClean["Status"],{"Project","DecomposeT","IntegrateT","ScalarT"}]===ConstantArray["NotRun",4]];
rgzSafetyRec["BadAmplitudeNeverPromotedToComplete",!RGZMXConvertedQ[<|"Amplitude"->$TimedOut|>]];
rgzMXJob["PerDiagram"]={rgzSafetyRow};
Internal`InheritedBlock[{RGZMXProject},Clear[RGZMXProject];RGZMXProject[___]:=<|"T"->7,"L"->0|>;
 rgzSafetyR=RGZMXRun[1,"Project",10];rgzSafetySkip=RGZMXRun[1,"Project",10];
 rgzSafetyRec["PoisonedCompleteCheckpointRecomputed",AssociationQ[rgzSafetyR]&&rgzSafetyR["Status"]==="Complete"&&First[rgzMXJob["PerDiagram"]]["Project"]["T"]===7];
 rgzSafetyRec["ValidCompleteCheckpointStillSkipped",AssociationQ[rgzSafetySkip]&&rgzSafetySkip["Status"]==="Complete; skipped"]];

rgzSafetyMakeJob[];
rgzSafetyChanged=Block[{$KeepLogDivergentScalelessIntegrals=!TrueQ[$KeepLogDivergentScalelessIntegrals]},RGZMXRun[1,"Project",10]];
rgzSafetyRec["RunningJobRejectsChangedScalelessPolicy",MatchQ[rgzSafetyChanged,Failure["StaleGraphEnvironment",_]]];
rgzSafetyManifest=FileNameJoin[{rgzSafetyJobDir,"job.wl"}];
Put[Join[KeyDrop[rgzMXJob,"PerDiagram"],<|"Version"->3,"ModelFingerprint"->RGZMXModelFingerprint[],"DiagramCount"->0|>],rgzSafetyManifest];
rgzSafetyChanged=Block[{$KeepLogDivergentScalelessIntegrals=!TrueQ[$KeepLogDivergentScalelessIntegrals]},RGZMXResume[rgzSafetyJobDir]];
rgzSafetyRec["ExplicitResumeRejectsChangedScalelessPolicy",MatchQ[rgzSafetyChanged,Failure["StaleGraphEnvironment",_]]];

(* PV results and scalar short circuits must reject nested failures too. *)
rgzMXLastPVUnit=<||>;
Internal`InheritedBlock[{PaVeReduce},Clear[PaVeReduce];PaVeReduce[___]:=System`TerminatedEvaluation["IterationLimit"];
 rgzSafetyRec["PVNestedTerminationRejected",FailureQ[RGZMXPVUnit[fixturePV,None,{},10]]]];
rgzSafetyRec["ScalarShortCircuitRejectsInvalidExpression",FailureQ[RGZMXScalarReduce[Indeterminate]]];
rgzSafetyRec["WrongProjectShapeRejected",!RGZMXStageValueQ["Project",<|"T"->0|>]];
rgzSafetyRec["AssumptionsAffectIntegrationContext",Block[{$Assumptions=True},RGZMXIntegralContext[]]=!=Block[{$Assumptions=(m==\[Mu])},RGZMXIntegralContext[]]];

(* Loading either integral module must not replace the single dispatcher. *)
rgzSafetyDispatcher=DownValues[RGZMXIntegralUnit];
RGZWorkflow`Private`engineGet["Algebra/TwoPointIntegrals.wl",False];
rgzSafetyRec["AcceleratorReloadDoesNotReplaceDispatcher",DownValues[RGZMXIntegralUnit]===rgzSafetyDispatcher];
RGZWorkflow`Private`engineGet["Algebra/MixedIntegrals.wl",False];
rgzSafetyRec["StandardReloadKeepsSameDispatcher",DownValues[RGZMXIntegralUnit]===rgzSafetyDispatcher];
RGZMXSetAcceleration["Enabled"->False];
RGZWorkflow`Private`loadLoopEngine[];
rgzSafetyRec["LoaderPreservesExplicitStandardRoute",!TrueQ[rgzMXAcceleration["Enabled"]]];
rgzSafetyPath=FileNameJoin[{rgzSafetyTemp,"explicit-standard.wl"}];rgzMXLastIntegralUnit=<||>;
rgzSafetyStandard=RGZMXIntegralUnit[FCI[FAD[{q,\[Mu]}]],rgzSafetyPath,RGZMXIntegralContext[],30];
rgzSafetyStandardPacket=Get[rgzSafetyPath];
RGZMXSetAcceleration["Enabled"->True];
rgzSafetyPath=FileNameJoin[{rgzSafetyTemp,"explicit-accelerated.wl"}];rgzMXLastIntegralUnit=<||>;
rgzSafetyFast=RGZMXIntegralUnit[FCI[FAD[{q,\[Mu]}]],rgzSafetyPath,RGZMXIntegralContext[],30];
rgzSafetyFastPacket=Get[rgzSafetyPath];
rgzSafetyRec["ExplicitRoutesAgreeForMassiveTadpole",SameQ[rgzSafetyStandard,rgzSafetyFast]&&!FailureQ[rgzSafetyStandard]];
rgzSafetyRec["ExplicitRoutesUseTheirOwnUnitImplementation",!KeyExistsQ[rgzSafetyStandardPacket,"Algorithm"]&&Lookup[rgzSafetyFastPacket,"Algorithm",None]==="ScalarIdentityReduce"];

(* Exercise actual FA graph generation and the complete conversion,
   projection, splitting, integration and scalar stages for both flavours. *)
RGZMXSetAcceleration["Enabled"->True];
rgzSafetyActual=RGZMixComputePair[1,1,60,"Category"->{"QuarkBubble"},"reuseAACache"->False];
rgzSafetyRec["NativeQuarkBubblePipeline",AssociationQ[rgzSafetyActual]&&rgzSafetyActual["Status"]==="Complete"];
rgzSafetyActualRows=rgzMXJob["PerDiagram"];
rgzSafetyRec["BothNativeQuarkFlavoursFinishScalarStages",Length[rgzSafetyActualRows]===2&&
 AllTrue[rgzSafetyActualRows,Lookup[#["Status"],"ScalarT",None]==="Complete"&&Lookup[#["Status"],"ScalarL",None]==="Complete"&]];
rgzSafetyRec["NativeGraphCacheResumesWithCurrentProvenance",!FailureQ[RGZMXResume[rgzMXJob["CacheDirectory"]]]];
rgzSafetyNativeSkip=RGZMXRun[1,"ScalarT",10];
rgzSafetyRec["NativeValidScalarCheckpointStillReused",AssociationQ[rgzSafetyNativeSkip]&&rgzSafetyNativeSkip["Status"]==="Complete; skipped"];
Print["SUMMARY ",<|"Checks"->Length[rgzSafetyChecks],"Failed"->Keys[Select[rgzSafetyChecks,!TrueQ[#]&]]|>];
Exit[If[AllTrue[Values[rgzSafetyChecks],TrueQ],0,1]];
