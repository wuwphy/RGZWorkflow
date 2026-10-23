(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
checks=<||>;
record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",checks[n]]);
dir=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"probe"}];If[!DirectoryQ[dir],CreateDirectory[dir]];
faDefinitions=DownValues[FeynArts`Initialize`coeffDef];
probe=Check[rgzQQProbeGraphs[dir,120],$Failed];
record["ScopedFAInitializationWithoutMessages",AssociationQ[probe]];
record["FADefinitionsRestored",DownValues[FeynArts`Initialize`coeffDef]===faDefinitions];
record["IndependentProbeGraphs",AssociationQ[probe]&&Length[probe["AVAndBoxes"]]===4];
If[!AssociationQ[probe],Exit[1]];
probe=MapAt[RGZLoopNormalize,probe,{{Key["Tree"]},{Key["AVAndBoxes"]},{Key["QuarkSelfEnergy"]}}];
probe=Check[rgzQQNormalizeColours[probe],$Failed];
record["ColourNormalizationAfterFullBackendLoad",AssociationQ[probe]];
If[!AssociationQ[probe],Exit[1]];
record["ColourNormalizationIsTyped",rgzQQNormalizeColours[{Col12,SUNFIndex[Col12]}]==={Col12,SUNFIndex[qqC12]}];
record["ExternalColourEndpointsCanonical",FreeQ[probe["Tree"],SUNFIndex[Col1|Col2|Col3|Col4]]&&Count[probe["Tree"],SUNFIndex[qqC1|qqC2|qqC3|qqC4],Infinity]===4];
parts=rgzQQClassify[probe["AVAndBoxes"]];
record["GraphClasses",AssociationQ[parts]&&Sort[Keys[parts]]==={"AV1","AV2","Box1","Box2"}];
record["ExternalSpinorsKept",Count[probe["Tree"],_Spinor,Infinity]===4];
record["ColourSumAudited",FailureQ[rgzQQStripSums[FeynArts`SumOver[qqBadColour,3] SUNFIndex[qqBadColour]]]];
tree=probe["Tree"]/.a_Symbol/;StringMatchQ[SymbolName[Unevaluated[a]],"Glu"~~DigitCharacter]:>qqA;
tree=tree/.a_Symbol/;StringMatchQ[SymbolName[Unevaluated[a]],"Lor"~~DigitCharacter]:>If[SymbolName[Unevaluated[a]]==="Lor1",qqMu,qqNu];
canonical=-I SMP["g_s"]^2 (qqT-\[Mu]^2)/((qqT-m^2)(qqT-\[Mu]^2)+mG^4) rgzQQBornCurrent[qqMu,qqMu];
record["TreeExternalOrderingSign",rgzQQWardSimplify[tree-canonical]===0];
delta=FeynAmpDenominatorExplicit[(parts["AV2"]/.\[Alpha]->qqGauge)-(parts["AV2"]/.\[Alpha]->0)];
expected=SMP["g_s"]^4 qqGauge (qqT-\[Mu]^2)/
 (2 SUNN ((qqT-m^2)(qqT-\[Mu]^2)+mG^4) (SPD[q,q]^2+qqGauge M^4)) rgzQQBornCurrent[qqMu,qqMu];
ward=CheckAbort[TimeConstrained[rgzQQWardSimplify[delta-expected],120,$TimedOut],$Aborted];
record["FullAVGeneralGaugeWard",ward===0];
raw=SUNSimplify[DiracSimplify[FeynAmpDenominatorExplicit[probe["QuarkSelfEnergy"]/.q->q+qqR]],SUNNToCACF->False];
raw=DiracSimplify[DiracTrace[GSD[qqR].raw],DiracTraceEvaluate->True]/(4 SPD[qqR,qqR]);
raw=FCE[ExpandScalarProduct[raw]]/.{SPD[q,q]->qqQ2,SPD[qqR,qqR]->qqR2,SPD[q,qqR]->qqQR};
raw=Normal[Series[raw/.{qqR2->qqE^2 qqR2,qqQR->qqE qqQR},{qqE,0,0}]]/.qqQR^2->qqQ2 qqR2/D;
raw=Cancel[Together[raw/.{qqQR->0,qqE->1}]];
expected=-SMP["g_s"]^2 (SUNN^2-1)/(2 SUNN) SDF[qqC1,qqC2] (
 -(D-1)(D-4)/D (qqQ2-\[Mu]^2)/(qqQ2 ((qqQ2-m^2)(qqQ2-\[Mu]^2)+mG^4))-\[Alpha]/(qqQ2^2+\[Alpha] M^4));
record["GeneratedOnShellResidueKernel",Cancel[Together[raw-expected]]===0];
record["IRPrescriptionGuard",FailureQ[rgzQQSigmaV0[1,0]]];
input=rgzQQSplit[parts["AV2"]/.{\[Alpha]->0,M->0},0,0];
record["AVPoleSplit",!FailureQ[input]];
oldScalarProducts=DownValues[Pair];
pv=CheckAbort[TimeConstrained[rgzQQReduce[input],120,$TimedOut],$Aborted];
record["AVPVReduction",FreeQ[pv,TID|FeynAmpDenominator|$TimedOut|$Aborted|$Failed|_Failure]];
record["ScalarProductsRestored",DownValues[Pair]===oldScalarProducts];
record["SMatrixRequiresProof",FailureQ[RGZWorkflow`RGZQQSMatrix[]]];

(* A vertex-file fingerprint fixture is used only for the assembly bookkeeping test. *)
Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],<|"TestFixtureOnly"->True|>,"WXF"];
record["RegulatorTadpoleSkipsCoefficientSimplification",rgzQQM0Term[{A0[-I Sqrt[\[Alpha] M^4]],SMP["g_s"]^2/M^4},30]==={0,0,0}];
laurentPoint=<|"p^2"->qqM0S,"m^2"->qqM0X,"mu^2"->qqM0Y,"mG^4"->qqM0Z,"N"->qqM0N|>;
laurentSample=(m^2+\[Alpha])/M^4+(mG^4-\[Alpha])/M^4+1/(m^2+M^2)+1/(m^2+M^2)^2;
oldCo=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=laurentPoint},RGZResidual39Diagnostic`Private`pointExpression[laurentSample]];
record["FactoredLaurentMatchesGlobalRationalExpansion",Cancel[Together[rgzQQLaurentByFactors[laurentSample,laurentPoint]-RGZResidual39Diagnostic`Private`rationalLaurent[Cancel[Together[oldCo]]]]]===0];
record["HigherPolesCancelBeforeRejection",rgzQQLaurentByFactors[1/M^6+(M^2-1)/M^6,laurentPoint]===RGZResidual39Diagnostic`Private`e^-2];
record["NonCancellingHigherPoleRejected",FailureQ[rgzQQLaurentByFactors[1/M^6,laurentPoint]]];
record["GaugeCoordinateExpansionMatchesLimit",Cancel[Together[rgzQQGaugeZeroCoefficient[(1+Sqrt[\[Alpha]])/(m^2+\[Alpha])+1/(m^2+\[Alpha])]-2/m^2]]===0];
record["GaugePolesCancelAcrossAddends",rgzQQGaugeZeroCoefficient[1/\[Alpha]+(\[Alpha]-1)/\[Alpha]]===1];
fractional=Sqrt[M^2]/(m^2+M^2)+(M^2-m^2) Sqrt[M^2]/((m^2+M^2)(m^2-M^2));
record["FractionalCancellationFallback",rgzQQM0Term[{B0[SPD[p,p],m^2,\[Mu]^2],fractional},30]==={0,0,0}];
record["M0RetainsLoopCoupling",rgzQQM0Scalar[SMP["g_s"]^2 A0[m^2],30]===SMP["g_s"]^2 A0[m^2]];
scaled=SMP["g_s"]^2 B0[SPD[p,p],-I Sqrt[\[Alpha] M^4],I Sqrt[\[Alpha] M^4]]/\[Alpha];
finite=rgzQQM0Scalar[scaled,30];
record["M0BeforeGaugeAvoidsPrematureZero",!FailureQ[finite]&&FreeQ[finite,Indeterminate|ComplexInfinity|_DirectedInfinity]];
record["GaugeLimitOfCollectedCoefficients",rgzQQGaugeValue[\[Alpha] finite,0,30]===SMP["g_s"]^2 B0[SPD[p,p],0,0]];
record["GenuineGaugePoleRejected",FailureQ[rgzQQGaugeValue[finite,0,30]]];
(* Exercise the public start path with bookkeeping-only zero matrices;
   the independent probe graphs and normalization remain real. *)
mixedFixture=<|"ModelFingerprint"->RGZWorkflow`Private`modelKey[],"ModelContent"->RGZWorkflow`Private`modelContent[],"T"->ConstantArray[0,{5,5}],"L"->ConstantArray[0,{8,8}],
 "\:5168\:989c\:8272\:5f20\:91cf\:77e9\:9635"->ConstantArray[0,{8,8}],
 "T\:573a\:987a\:5e8f"->RGZMixBasis["T"],"L\:573a\:987a\:5e8f"->RGZMixBasis["L"]|>;
RGZWorkflow`Private`storeResult["Mixed",mixedFixture];
start=Internal`InheritedBlock[{RGZWorkflow`RGZNAVSum},
 Clear[RGZWorkflow`RGZNAVSum];
 RGZWorkflow`RGZNAVSum[___]:=(RGZWorkflow`Private`storeResult["NAV",<|"TransverseIntegrand"->0|>];<|"Status"->"TestFixture"|>);
 Check[RGZWorkflow`RGZQQStart[120,"Gauge"->0,"RegulatorMass"->0],$Failed]];
record["PublicStartAfterRestoreEquivalent",AssociationQ[start]&&Lookup[start,"Status",None]==="IntegrandsReady"];
If[AssociationQ[start],tempJob=RGZWorkflow`RGZResult["QQ"];AssociateTo[tempJob,{"SelfPrepared"->False,"MixedSelfEnergy"->Missing["PrepareSelfFirst"]}];RGZWorkflow`Private`storeResult["QQ",tempJob]];
record["PublicStartNoInfinitePacket",FreeQ[RGZWorkflow`RGZResult["QQ"],Indeterminate|ComplexInfinity|_DirectedInfinity]];
record["StartDefersSelf",!TrueQ[RGZWorkflow`RGZResult["QQ"]["SelfPrepared"]]&&MissingQ[RGZWorkflow`RGZResult["QQ"]["MixedSelfEnergy"]]];
record["StartDefersNAVAttachment",MatchQ[RGZWorkflow`RGZResult["QQ"]["Integrands"]["NAV1"],Missing["DeferredNAV",1]]];
record["AmplitudeRequiresPreparedSelf",FailureQ[RGZWorkflow`RGZQQAmplitude[]]];
prep=RGZWorkflow`RGZQQPrepareSelf[30,"TotalSeconds"->120];
record["PublicPrepareSelf",AssociationQ[prep]&&RGZWorkflow`RGZResult["QQ"]["SelfPrepared"]];
record["PreparedSelfFinite",FreeQ[RGZWorkflow`RGZResult["QQ"]["MixedSelfEnergy"],_Missing|_Failure|Indeterminate|ComplexInfinity]];
record["PreparedSelfReused",Lookup[RGZWorkflow`RGZQQPrepareSelf[30],"Reused",False]];
(* Nonzero PV fixture: compare against the independently retained algorithm,
   deliberately stop after one coefficient and then resume its checkpoints. *)
sample=SMP["g_s"]^2 (A0[m^2]+(B0[SPD[p,p],-I Sqrt[\[Alpha] M^4],m^2]-B0[SPD[p,p],I Sqrt[\[Alpha] M^4],m^2])/Sqrt[\[Alpha] M^4]);
sampleJob=RGZWorkflow`RGZResult["QQ"];
sampleMatrix=ConstantArray[0,{5,5}];sampleMatrix[[1,1]]=sample;
AssociateTo[sampleJob,{"MixedTransverse"->sampleMatrix,"MixedSourceHash"->Hash[sampleMatrix,"SHA256"],"SelfPrepared"->False,"MixedSelfEnergy"->Missing["PrepareSelfFirst"],"Directory"->FileNameJoin[{dir,"resume-test-"<>IntegerString[Hash[AbsoluteTime[]],16]}]}];
CreateDirectory[sampleJob["Directory"]];RGZWorkflow`Private`storeResult["QQ",sampleJob];
interrupted=Internal`InheritedBlock[{RGZWorkflow`Private`qqSelfCheckpoint},
 RGZWorkflow`Private`qqSelfCheckpoint[d_,n_,k_,x_,t_]/;n==="m0-2":=Failure["IntentionalTestStop",<||>];
 RGZWorkflow`RGZQQPrepareSelf[30,"TotalSeconds"->120]];
record["PartialPreparationStops",FailureQ[interrupted]];
firstFile=FileNameJoin[{sampleJob["Directory"],"self","m0-1.wxf"}];
record["CompletedCoefficientCheckpointRetained",FileExistsQ[firstFile]];
firstDate=If[FileExistsQ[firstFile],FileDate[firstFile],None];
resumed=RGZWorkflow`RGZQQPrepareSelf[30,"TotalSeconds"->120];
record["InterruptedPreparationResumes",AssociationQ[resumed]&&RGZWorkflow`RGZResult["QQ"]["SelfPrepared"]];
record["CompletedCheckpointNotRewritten",FileDate[firstFile]===firstDate];
w=RGZGaugeStart[Join[mixedFixture,<|"T"->sampleMatrix|>]];
legacy=rgzQQGaugeValue[rgzQQM0Scalar[Total[RGZPTCommonSelf[w]],30],0,30];
expected=(-SMP["g_s"]^2 rgzQQBornCurrent[qqMu,qqNu] RGZLoopPT[p,qqMu,qqNu] legacy)/.p->qqK3-qqK1;
difference=Cancel[Together[RGZWorkflow`RGZResult["QQ"]["MixedSelfEnergy"]-expected]];
Export["/tmp/rgz-qq-self-difference.txt",ToString[difference,InputForm],"Text"];
record["CheckpointAlgorithmMatchesLegacy",difference===0];
(* Detailed progress is display-only and keeps the original checkpoint keys. *)
progressRows={{1,0},{B0[SPD[p,p],m^2,\[Mu]^2],(qqProgressTestX^2-1)/(qqProgressTestX-1)}};
RGZWorkflow`Private`qqSelfProgress["Matrix",0,25];
RGZWorkflow`Private`qqSelfProgress["M0Assemble:0",2,13];
progressName="progress-"<>IntegerString[Hash[AbsoluteTime[]],16];
progressResult=RGZWorkflow`Private`qqSelfCheckpoint[dir,progressName,"display-test",Factor[Cancel[Together[progressRows[[2,2]]]]],30];
progressPacket=RGZWorkflow`RGZResult["QQProgress"];
record["DetailedProgressPreservesRationalResult",progressResult===1+qqProgressTestX];
record["DetailedProgressShowsMaster",progressPacket["Master"]===progressRows[[2,1]]];
record["DetailedProgressHasTimingAndSize",And@@(KeyExistsQ[progressPacket,#]& /@ {"RunStarted","ItemStarted","OperationStarted","StepLimitSeconds","InputLeaves"})];
record["DetailedProgressPanelBuilt",Head[RGZWorkflow`Private`qqSelfProgressPanel[]]===Panel];
progressAgain=RGZWorkflow`Private`qqSelfCheckpoint[dir,progressName,"display-test",$Failed,30];
record["DetailedProgressReusesCheckpoint",progressAgain===progressResult&&RGZWorkflow`RGZResult["QQProgress"]["CacheState"]==="Reused"];
(* Final sum and restore progress must not change algebra or backend definitions. *)
restoreSample=(RGZResidual39Diagnostic`Private`h^2+RGZResidual39Diagnostic`Private`dim)/(qqM0X-qqM0Y) A0[qqM0Y];
restoreExpected=rgzQQM0Restore[restoreSample];restoreDefinitions=DownValues[rgzQQM0Restore];
restoreActual=RGZWorkflow`Private`qqSelfMonitor[rgzQQM0Restore[restoreSample],None];
restoreDifferences=RGZGaugePVPairs[restoreActual-restoreExpected];
record["RestoreProgressPreservesAlgebra",ListQ[restoreDifferences]&&AllTrue[restoreDifferences,TrueQ[Cancel[Together[#[[2]]]]===0]&]&&RGZWorkflow`RGZResult["QQProgress"]["Stage"]==="M0Restore:Sum"];
record["RestoreInstrumentationIsScoped",DownValues[rgzQQM0Restore]===restoreDefinitions];
RGZWorkflow`Private`qqSelfCheckpoint[dir,"m0-sum-3","sum-progress-test",1+qqProgressTestX,30];
sumProgress=RGZWorkflow`RGZResult["QQProgress"];
record["FinalSumStageAndSaveTimer",sumProgress["Stage"]==="M0Sum"&&sumProgress["Index"]===3&&NumericQ[sumProgress["OperationFinished"]]];
(* Equivalent assembly optimizer: branch-preserving poles, fallback and live reload. *)
optRoot=Sqrt[RGZWorkflow`Engine`qqM0Z-(RGZWorkflow`Engine`qqM0X-RGZWorkflow`Engine`qqM0Y)^2/4];
optPole=(RGZWorkflow`Engine`qqM0X+RGZWorkflow`Engine`qqM0Y)/2-I optRoot;
optSample=(qqOptS^2-optPole^2)/(qqOptS-optPole)^2+1/(qqOptS-optPole);
optResult=RGZWorkflow`Private`qqGroupedRational[optSample];
record["GroupedPoleAlgebraExact",optResult=!=$Failed&&Cancel[Together[optResult-optSample]]===0];
optFallback=RGZWorkflow`Private`qqSimplifyCoefficient[Sin[qqOptS]/(qqOptS-1)];
record["UnsupportedAlgebraUsesExactFallback",optFallback===Factor[Cancel[Together[Sin[qqOptS]/(qqOptS-1)]]]&&RGZWorkflow`RGZResult["QQProgress"]["AlgebraMethod"]==="OriginalRationalFallback"];
optFingerprint=RGZWorkflow`Private`qqJobFingerprint[0,0];optJob=RGZWorkflow`RGZResult["QQ"];
Block[{$Context="RGZWorkflow`Private`",$ContextPath={"RGZWorkflow`Private`","RGZWorkflow`","System`"}},Get[FileNameJoin[{RGZWorkflow`Private`$packageRoot,"Kernel","ScatteringProgress.wl"}]]];
record["EquivalentOptimizerReloadKeepsJob",optFingerprint===RGZWorkflow`Private`qqJobFingerprint[0,0]&&RGZWorkflow`RGZResult["QQ"]===optJob];
(* Physical box preconditioning verifies its rational inverse at generic masses. *)
record["QuarkProjectorWardLinesAtGenericD",And@@RGZWorkflow`Private`qqBoxWardLineChecks[]];
wardProbe1=RGZWorkflow`Private`qqBoxWardPrecondition[parts["Box1"]/.{\[Alpha]->0,M->0},"Box1"];
record["Box1ProjectorWardEnabled",TrueQ[RGZWorkflow`RGZResult["QQProgress"]["BoxProjectorWardApplied"]]];
record["Box1WardCoordinatesExact",!FailureQ[wardProbe1]&&TrueQ[RGZWorkflow`RGZResult["QQProgress"]["WardCoordinateVerified"]]&&!FailureQ[rgzQQSplit[wardProbe1,0,0]]];
wardProbe2=RGZWorkflow`Private`qqBoxWardPrecondition[parts["Box2"]/.{\[Alpha]->0,M->0},"Box2"];
record["Box2ProjectorWardEnabled",TrueQ[RGZWorkflow`RGZResult["QQProgress"]["BoxProjectorWardApplied"]]];
record["Box2WardCoordinatesExact",!FailureQ[wardProbe2]&&TrueQ[RGZWorkflow`RGZResult["QQProgress"]["WardCoordinateVerified"]]&&!FailureQ[rgzQQSplit[wardProbe2,0,0]]];
(* Scalar extraction and resumable tensor reduction are exact and scoped. *)
routeSample=FCI[FAD[q,{q,2},q-p,{q-p,3}]];
routeApart=RGZWorkflow`Private`qqRouteApart[routeSample];
record["SameRoutePartialFractionsExact",Cancel[Together[FeynAmpDenominatorExplicit[routeApart]-FeynAmpDenominatorExplicit[routeSample]]]===0&&Max[Length /@ Cases[routeApart,_FeynAmpDenominator,Infinity]]===2];
routeRepeated=FCI[FAD[q,q,{q,2},q-p]];
record["SameMassRepeatedPolesRetained",Cancel[Together[FeynAmpDenominatorExplicit[RGZWorkflow`Private`qqRouteApart[routeRepeated]]-FeynAmpDenominatorExplicit[routeRepeated]]]===0];
chainSample=FCI[SpinorUBarD[qqK3,0].((qqTensorA+qqTensorB) GSD[q] FAD[q,{q,2}]+qqTensorA GAD[qqTensorMu] FAD[q-p]).SpinorUD[qqK1,0]];
chainLinear=RGZWorkflow`Private`qqLinearizeChains[chainSample];
record["OpenChainLinearityMatchesIndependentDotSimplify",
 Expand[DotSimplify[chainSample,Expanding->True]-chainLinear]===0];
routeReference=Expand[chainLinear/.f_FeynAmpDenominator:>RGZWorkflow`Private`qqRouteApartDen[f]];
record["MemoizedDenominatorsMatchUncachedChainReference",
 Expand[RGZWorkflow`Private`qqRouteApart[chainLinear]-routeReference]===0];
DeclareNonCommutative[qqMatrixProbe];
record["DeclaredMatrixStaysInsideChain",
 RGZWorkflow`Private`qqLinearizeChains[FCI[GSD[q].(qqTensorA qqMatrixProbe).GSD[p]]]===FCI[qqTensorA GSD[q].qqMatrixProbe.GSD[p]]];
UnDeclareNonCommutative[qqMatrixProbe];
rankSample=FCI[SPD[q,q]^3 FAD[{q,qqRankMass},q,{q,qqRankMass},q-p]];
rankLowered=RGZWorkflow`Private`qqCancelRoutePowers[rankSample];
record["RouteSquareCancellationKeepsMassiveRepeatedPoles",
 Cancel[Together[FeynAmpDenominatorExplicit[rankLowered]-FeynAmpDenominatorExplicit[rankSample]]]===0&&rankLowered=!=rankSample];
rankOther=FCI[SPD[q,p]^2 FAD[{q,qqRankMass},q-p]];
record["MixedScalarProductsAreNotCancelled",RGZWorkflow`Private`qqCancelRoutePowers[rankOther]===rankOther];
coordSample=FCI[SPD[q,qqK1]^2 SPD[q,qqK3] FAD[{q,qqRankMass},q-qqK1,q-qqK1+qqK3]];
coordActual=RGZWorkflow`Private`qqNAVScalarCoordinates[coordSample,"NAV1"];
record["NAVCoordinatesExactWithoutExternalOnShellAssumptions",
 Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[coordActual-coordSample]]]]===0];
coordSample2=coordSample/.{qqK1->qqK2,qqK3->qqK4};
coordActual2=RGZWorkflow`Private`qqNAVScalarCoordinates[coordSample2,"NAV2"];
record["SecondNAVLineHasIndependentExactCoordinates",
 Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[coordActual2-coordSample2]]]]===0];
tensorSample=FCI[(qqTensorA+qqTensorB) FVD[q,qqTensorMu] FAD[{q,2},q-p]+(qqTensorA-qqTensorB) FVD[q,qqTensorMu] FAD[{q,3},q-p]];
tensorPieces=RGZWorkflow`Private`qqTensorPieces[tensorSample];
record["TensorWeightsReconstructInput",Length[tensorPieces]===2&&Expand[Total[Times@@@tensorPieces]-tensorSample]===0];
tensorOptions=Options[TID];tensorExpected=rgzQQReduce[tensorSample];
tensorJob=<|"Directory"->dir,"Fingerprint"->"tensor-regression-1"|>;
tensorActual=RGZWorkflow`Private`qqReduceTensorGroups[tensorSample,tensorJob,"TensorRegression",30];
record["TensorGroupsMatchOriginalReduction",!FailureQ[tensorActual]&&Cancel[Together[PaVeReduce[tensorActual-tensorExpected]]]===0&&Options[TID]===tensorOptions];
rankTIDSample=FCI[SPD[q,q] FAD[{q,2},{q-p,3}]];
rankTIDExpected=rgzQQReduce[rankTIDSample];
rankTIDActual=RGZWorkflow`Private`qqReduceTensorGroups[rankTIDSample,tensorJob,"RankPVRegression",30];
record["LoweredTensorRankMatchesIndependentPVReduction",
 !FailureQ[rankTIDActual]&&Cancel[Together[PaVeReduce[rankTIDActual-rankTIDExpected]]]===0];
anchorInput=FCI[FVD[q,qqAnchorMu] FAD[{q-qqK1,2},q-qqK3]];
anchorExpected=FCI[FVD[q+qqK1,qqAnchorMu] FAD[{q,2},q+qqK1-qqK3]];
record["TensorAnchorShiftsNumeratorAndDenominators",Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[RGZWorkflow`Private`qqTensorAnchor[anchorInput]-anchorExpected]]]]===0];
closedCurrent[mu_]:=SpinorUBarD[qqK3,0].GAD[mu].SpinorUD[qqK1,0] SpinorUBarD[qqK4,0].GAD[mu].SpinorUD[qqK2,0];
closedPieces1=RGZWorkflow`Private`qqTensorPieces[FCI[closedCurrent[qqClosedMu1] FAD[{q,2},q-p]]];
closedPieces2=RGZWorkflow`Private`qqTensorPieces[FCI[closedCurrent[qqClosedMu2] FAD[{q,2},q-p]]];
record["ExternalCurrentDummyNamesDoNotChangeCore",closedPieces1[[1,2]]===closedPieces2[[1,2]]];
(* Numerator preconditioning must retain exact repeated denominators and D. *)
numInput=FCI[GAD[qqNumMu].GSD[q].GAD[qqNumMu] FAD[{q,2},{q,2},q-p]];
numActual=RGZWorkflow`Private`qqTensorNumeratorSimplify[numInput,5];
record["NumeratorSimplificationPreservesDAndPoles",
 DiracSimplify[numActual-numInput,DiracEquation->True,SirlinSimplify->False]===0&&
 Cases[numActual,_PropagatorDenominator,Infinity]===Cases[numInput,_PropagatorDenominator,Infinity]&&
 !FreeQ[numActual,D]&&LeafCount[numActual]<LeafCount[numInput]];
routeExpanded=FCI[1/ExpandScalarProduct[SPD[q-qqK1,q-qqK1]]];
routeRestored=RGZWorkflow`Private`qqRestoreAffineSquares[routeExpanded];
record["ExpandedNAVQuarkRouteRestoredExactly",
 Cancel[Together[ExpandScalarProduct[routeRestored-routeExpanded]]]===0&&
 !FailureQ[rgzQQSplit[routeRestored,0,0]]];
numSum=FCI[GSD[q] FAD[q]+GSD[p] FAD[q-p]];
record["NumeratorSumIsNotRefactoredIncorrectly",RGZWorkflow`Private`qqTensorNumeratorSimplify[numSum,5]===numSum];
tensorReused=Internal`InheritedBlock[{rgzQQReduce},rgzQQReduce[_]:=$Failed;RGZWorkflow`Private`qqReduceTensorGroups[tensorSample,tensorJob,"TensorRegression",30]];
record["TensorCoreCheckpointsReused",tensorReused===tensorActual];
(* Part progress must forward the supplied limit and preserve resumable state. *)
partDir=FileNameJoin[{dir,"parts-"<>CreateUUID[]}];CreateDirectory[partDir];
partJob=<|"Directory"->partDir,"Fingerprint"->RGZWorkflow`Private`qqJobFingerprint[0,0],"Gauge"->0,"RegulatorMass"->0,
 "Integrands"-><|"Box1"->1,"Box2"->2|>,"Reduced"-><||>,"LoopMeasureConversion"->1|>;
RGZWorkflow`Private`storeResult["QQ",partJob];partLimits={};
Internal`InheritedBlock[{RGZWorkflow`Private`engineGet,rgzQQSplit,rgzQQReduce,rgzQQRestorePoleMasses,RGZWorkflow`Private`timed},
 RGZWorkflow`Private`engineGet[_,___]:=Null;rgzQQSplit[x_,_,_]:=x;rgzQQReduce[x_]:=x;
 rgzQQRestorePoleMasses[x_,_,_]:=x;
 RGZWorkflow`Private`timed[n_,x_,t_]:=(AppendTo[partLimits,{n,t}];x);
 partAnswer=RGZWorkflow`RGZQQRunPart["Box1",1800];
 record["PartProgressForwards1800",partAnswer["Status"]==="Complete"&&MemberQ[partLimits,{"QQTID:Box1",1800}]];
 partCount=Length[partLimits];partAgain=RGZWorkflow`RGZQQRunPart["Box1",1800];
 record["PartProgressReusesReduction",partAgain["Status"]==="Complete"&&Length[partLimits]===partCount];
 RGZWorkflow`Private`timed[n_,x_,t_]:=If[StringStartsQ[n,"QQTID:"],Failure["StageTimeout",<|"Stage"->n,"Seconds"->t|>],x];
 partStopped=RGZWorkflow`RGZQQRunPart["Box2",1800];
 record["PartTimeoutKeepsTaskAndLimit",FailureQ[partStopped]&&partStopped[[2]]["Seconds"]===1800&&AssociationQ[RGZWorkflow`RGZResult["QQ"]]&&KeyExistsQ[RGZWorkflow`RGZResult["QQ"]["Reduced"],"Box1"]&&RGZWorkflow`RGZResult["QQProgress"]["Status"]==="Stopped"]];
fixture=<|"Directory"->dir,"Fingerprint"->RGZWorkflow`Private`qqJobFingerprint[0,0],"Process"->"assembly fixture","Gauge"->0,"RegulatorMass"->0,
 "SelfPrepared"->True,"Tree"->2,"MixedSelfEnergy"->4,"OnShellSigmaV"->3,"Reduced"->AssociationThread[{"AV1","AV2","Box1","Box2","NAV1","NAV2"},ConstantArray[1,6]]|>;
RGZWorkflow`Private`storeResult["QQ",fixture];
r=RGZWorkflow`RGZQQAmplitude[];
record["FourExternalResiduesAndNoExtraPinch",RGZWorkflow`RGZResult["QQAmplitude"]["OneLoop"]===22];
record["BareDoesNotBecomeVerifiedSMatrix",FailureQ[RGZWorkflow`RGZQQSMatrix[]]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Exit[If[And@@Values[checks],0,1]];
