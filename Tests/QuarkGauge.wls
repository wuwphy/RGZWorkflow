(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
dir=FileNameJoin[{$TemporaryDirectory,"rgz-qq-gauge-tests-"<>CreateUUID[]}];RGZInitialize[dir,"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],{},"WXF"];
alpha=RGZWorkflow`Private`qqGaugeAlpha;
colour=First[RGZWorkflow`Private`qqFFColors[]];
spin=FeynCalc`FCI[FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK3,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK1,0]
 FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK4,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK2,0]];
born=colour spin; pv=FeynCalc`B0[testT,testM1,testM2];
fingerprint=RGZWorkflow`Private`qqJobFingerprint[alpha,0];
job=<|"Gauge"->alpha,"RegulatorMass"->0,"Fingerprint"->fingerprint,"GaugeIRPrescription"->"DimensionalScalelessZero"|>;
RGZWorkflow`Private`storeResult["QQ",job];
packet[loop_]:=<|"Gauge"->alpha,"RegulatorMass"->0,"IRPrescription"->"DimensionalScalelessZero","GaugeJobFingerprint"->fingerprint,"ModelFingerprint"->RGZWorkflow`Private`modelKey[],"AssemblyConvention"->RGZWorkflow`Private`qqAssemblyConvention,"OneLoop"->I loop,"Tree"->I alpha born|>;
RGZWorkflow`Private`storeResult["QQGaugeAmplitude",packet[born pv]];
r=RGZQQGaugeCheck[120,"CoefficientSeconds"->2];
record["IndependentOneLoopIgnoresGaugeDependentTreeFixture",AssociationQ[r]&&RGZResult["QQGaugeDirect"]["GaugeIndependenceVerified"]];
RGZWorkflow`Private`storeResult["QQGaugeAmplitude",packet[alpha (alpha-1) born pv]];
r=RGZQQGaugeCheck[120,"CoefficientSeconds"->2];
record["SymbolicCheckRejectsTwoPointFalsePositive",AssociationQ[r]&&!RGZResult["QQGaugeDirect"]["GaugeIndependenceVerified"]];
record["CheckIsNotNumericGaugeSampling",RGZResult["QQGaugeDirect"]["PointTest"]===False];
record["UncancelledDoesNotClaimPhysicalDependence",r["Status"]==="UncancelledCoefficients"];
record["RenormalizationNotInferred",RGZResult["QQGaugeDirect"]["Renormalized"]===False&&RGZResult["QQGaugeDirect"]["UVIRSeparated"]===False];
record["AlphaInsidePVArgumentsRejected",FailureQ[RGZWorkflow`Private`qqGaugeCompare[born FeynCalc`B0[testT,alpha,testM2],1,30,2,"PVArgumentTest"]]];
record["CoverageDetectsMissingGaugePart",!RGZWorkflow`Private`qqGaugeCompare[(1+alpha) born pv-born pv,1,30,2,"MissingCoverage"]["AllCoefficientsZero"]];
record["CoverageIgnoresGaugeIndependentRemainder",RGZWorkflow`Private`qqGaugeCompare[(1+alpha) born pv-alpha born pv,1,30,2,"ValidCoverage"]["AllCoefficientsZero"]];
record["CoverageZeroInputSupported",RGZWorkflow`Private`qqGaugeCompare[0,1,30,2,"ZeroCoverage"]["AllCoefficientsZero"]];
stopped=Block[{RGZWorkflow`Private`qqCoefficientReduce},
 Clear[RGZWorkflow`Private`qqCoefficientReduce];
 RGZWorkflow`Private`qqCoefficientReduce[_]:=(Pause[0.2];0);
 RGZWorkflow`Private`qqGaugeCompare[alpha born pv,1,30,0.01,"TimeoutDiagnostic"]];
record["ComparisonTimeoutReportsActualReason",FailureQ[stopped]&&stopped[[2]]["Reason"]==="CoefficientTimeout"];
record["ComparisonTimeoutReportsActualStage",FailureQ[stopped]&&stopped[[2]]["Stage"]==="TimeoutDiagnostic"&&stopped[[2]]["Seconds"]===0.01];
record["StoppedCoefficientIsNotSavedAsComplete",FailureQ[stopped]&&StringQ[stopped[[2]]["File"]]&&!FileExistsQ[stopped[[2]]["File"]]];
record["GaugeReferenceMustBeIndependent",FailureQ[RGZWorkflow`Private`qqGaugeCompare[born,alpha,30,2,"BadReference"]]];
record["NumericGaugeJobCannotClaimSymbolicProof",Block[{RGZWorkflow`Private`$results=RGZWorkflow`Private`$results},RGZWorkflow`Private`storeResult["QQ",Join[job,<|"Gauge"->0|>]];FailureQ[RGZQQGaugeCheck[30]]]];
record["ModelFingerprintMismatchRejected",Block[{RGZWorkflow`Private`$results=RGZWorkflow`Private`$results},RGZWorkflow`Private`storeResult["QQGaugeAmplitude",Join[packet[born],<|"ModelFingerprint"->0|>]];FailureQ[RGZQQGaugeCheck[30]]]];
record["MissingWardEvidenceDoesNotPass",FailureQ[RGZQQWardCoverage[30]]];
record["OriginalQQTaskCanBeRestored",Block[{RGZWorkflow`Private`$results=RGZWorkflow`Private`$results},RGZWorkflow`Private`storeResult["QQBeforeGauge",<|"QQ"-><|"Gauge"->0|>,"QQAmplitude"-><|"Amplitude"->17|>,"QQFormFactors"->Missing[]|>];RGZQQGaugeRestore[];RGZResult["QQ"]["Gauge"]===0&&RGZResult["QQAmplitude"]["Amplitude"]===17]];
record["UnsupportedIRPrescriptionRejected",FailureQ[RGZQQGaugeStart[30,"IRPrescription"->"Unspecified"]]];
record["IncompleteOneLoopRejected",Block[{RGZWorkflow`Private`$results=RGZWorkflow`Private`$results},RGZWorkflow`Private`storeResult["QQGaugeAmplitude",packet[TerminatedEvaluation["IterationLimit"]]];FailureQ[RGZQQGaugeCheck[30]]]];
(* Native FA generation and symbolic-gauge initialization, with only the
   expensive mixed matrix and NAV source replaced by explicit zero fixtures. *)
RGZWorkflow`Private`loadPTEngine[];
RGZWorkflow`Private`engineGet["RGZ_Quark_Amplitude.wl",False];
oldKeep=FeynCalc`$KeepLogDivergentScalelessIntegrals;
initChecks=Block[{RGZWorkflow`Private`$results=RGZWorkflow`Private`$results},
 RGZWorkflow`Private`storeResult["Mixed",<|"T"->ConstantArray[0,{5,5}],"T场顺序"->RGZWorkflow`Engine`RGZMixBasis["T"]|>];
 RGZWorkflow`Private`storeResult["QQ",<|"Gauge"->0|>];
 init=Internal`InheritedBlock[{RGZNAVSum},
  Clear[RGZNAVSum];
  RGZNAVSum[_,___]:=(RGZWorkflow`Private`storeResult["NAV",<|"TransverseIntegrand"->0|>];<|"Status"->"Fixture"|>);
  RGZQQGaugeStart[120]];
 nativeJob=RGZResult["QQ"];
 nativeSplit=RGZWorkflow`Engine`rgzQQSplit[RGZWorkflow`Private`qqRestoreAffineSquares[nativeJob["Integrands"]["AV1"]],alpha,0];
 nativeAlphaRetained=!FailureQ[nativeSplit]&&!FreeQ[nativeSplit,alpha];
 RGZWorkflow`Private`storeResult["QQ",Join[nativeJob,<|"Gauge"->oldAlgorithmGaugeSymbol|>]];
 secondInit=Internal`InheritedBlock[{RGZNAVSum},Clear[RGZNAVSum];
  RGZNAVSum[_,___]:=(RGZWorkflow`Private`storeResult["NAV",<|"TransverseIntegrand"->0|>];<|"Status"->"Fixture"|>);
  RGZQQGaugeStart[120]];
 backupSurvivesAlgorithmChange=AssociationQ[secondInit]&&RGZResult["QQBeforeGauge"]["QQ"]["Gauge"]===0;
 {AssociationQ[init]&&nativeJob["Gauge"]===alpha,
 nativeJob["RegulatorMass"]===0&&nativeJob["GaugeIRPrescription"]==="DimensionalScalelessZero",
 RGZWorkflow`Private`qqFFSpinReduce[D[FeynCalc`FCI[nativeJob["Tree"]],alpha]]===0,
 RGZResult["QQBeforeGauge"]["QQ"]["Gauge"]===0}];
record["NativeGeneralAlphaStartWithExplicitZeroMatrixFixture",initChecks[[1]]];
record["NativeStartTagsIRAndM0",initChecks[[2]]];
record["NativeGeneratedTreeIsGaugeIndependentOnShell",initChecks[[3]]];
record["StartPreservesPreviousTask",initChecks[[4]]];
record["AlgorithmUpgradePreservesOriginalNormalTask",backupSurvivesAlgorithmChange];
record["NativeAVSplitRetainsSymbolicGauge",nativeAlphaRetained];
record["IRGlobalSettingRestored",FeynCalc`$KeepLogDivergentScalelessIntegrals===oldKeep];
record["OriginalResidueGuardRemainsInForce",FailureQ[RGZWorkflow`Engine`rgzQQSigmaV0[1,0]]];
coverageCases=Internal`InheritedBlock[{RGZWorkflow`Private`qqGaugeWardEvidence,RGZWorkflow`Private`qqGaugePinchAmplitude},
 Clear[RGZWorkflow`Private`qqGaugeWardEvidence,RGZWorkflow`Private`qqGaugePinchAmplitude];
 RGZWorkflow`Private`qqGaugeWardEvidence[]:=<|"FixtureWardEvidence"->True|>;
 RGZWorkflow`Private`qqGaugePinchAmplitude[_]:=testPinch;
 testPinch=alpha born pv;RGZWorkflow`Private`storeResult["QQGaugeAmplitude",packet[(alpha+1) born pv]];
 RGZQQWardCoverage[120,"CoefficientSeconds"->2];case1=RGZResult["QQWardCoverage"];
 testPinch=born pv;RGZWorkflow`Private`storeResult["QQGaugeAmplitude",packet[(alpha+1) born pv]];
 RGZQQWardCoverage[120,"CoefficientSeconds"->2];case2=RGZResult["QQWardCoverage"];
 RGZWorkflow`Private`storeResult["QQGaugeAmplitude",packet[2 born pv]];
 RGZQQWardCoverage[120,"CoefficientSeconds"->2];case3=RGZResult["QQWardCoverage"];
 {case1["CoverageVerified"]&&!case1["PinchDifferenceZero"]&&!case1["GaugeIndependenceVerified"],
 !case2["CoverageVerified"]&&case2["PinchDifferenceZero"]&&!case2["GaugeIndependenceVerified"],
 case3["CoverageVerified"]&&case3["PinchDifferenceZero"]&&case3["GaugeIndependenceVerified"],
 case3["FunctionalOneLoopSTProved"]===False}];
record["CoverageAloneCannotCertifyGaugeZero",coverageCases[[1]]];
record["PinchZeroAloneCannotCertifyCoverage",coverageCases[[2]]];
record["BothConditionsRequiredForProcessCertificate",coverageCases[[3]]];
record["ProcessCheckNeverClaimsFunctionalSTProof",coverageCases[[4]]];
cache1=RGZWorkflow`Private`qqGaugeCompare[alpha born pv,1,60,1,"CheckpointReuse"];
cacheFiles1=FileNames["*.wxf",DirectoryName[cache1["File"]]];
cacheHash1=FileHash[First[cacheFiles1],"SHA256"];
cache2=RGZWorkflow`Private`qqGaugeCompare[alpha born pv,1,60,2,"CheckpointReuse"];
record["LargerCoefficientLimitReusesExactCheckpoint",FileNames["*.wxf",DirectoryName[cache2["File"]]]===cacheFiles1&&FileHash[First[cacheFiles1],"SHA256"]===cacheHash1];
m0dir=FileNameJoin[{dir,"pinch-m0"}];
m0zero=RGZWorkflow`Private`qqGaugePinchM0[0,123,m0dir,30];
record["GenericDZeroM0Limit",m0zero===0];
m0bad=RGZWorkflow`Private`qqGaugePinchM0[1/RGZWorkflow`Engine`M^2,124,FileNameJoin[{dir,"pinch-bad-m0"}],30];
record["NegativeMassPoleIsNotSilentlyDiscarded",FailureQ[m0bad]];
finiteInput=FeynCalc`SMP["g_s"]^2 FeynCalc`B0[FeynCalc`SPD[RGZWorkflow`Engine`p,RGZWorkflow`Engine`p],RGZWorkflow`Engine`m^2,RGZWorkflow`Engine`\[Mu]^2];
oldFinite=RGZWorkflow`Engine`rgzQQM0Scalar[finiteInput,30];
newFinite=RGZWorkflow`Private`qqGaugePinchM0[finiteInput,125,FileNameJoin[{dir,"pinch-finite-m0"}],30];
record["CheckpointedM0MatchesExistingGenericDLimit",!FailureQ[oldFinite]&&!FailureQ[newFinite]&&Cancel[Together[oldFinite-newFinite]]===0];
reuseChecks=Block[{RGZWorkflow`Private`$results=RGZWorkflow`Private`$results},
 oldDir=FileNameJoin[{dir,"old-reuse"}];newDir=FileNameJoin[{dir,"new-reuse"}];CreateDirectory[FileNameJoin[{oldDir,"self"}],CreateIntermediateDirectories->True];CreateDirectory[newDir];
 oldFingerprint=RGZWorkflow`Private`qqJobFingerprint[0,0];oldReuse=<|"Gauge"->0,"RegulatorMass"->0,"Fingerprint"->oldFingerprint,"MixedSourceHash"->42,"Directory"->oldDir|>;
 newReuse=<|"Fingerprint"->fingerprint,"MixedSourceHash"->42,"Directory"->newDir|>;
 oldReuseKey=Hash[{oldFingerprint,42},"SHA256"];newReuseKey=Hash[{fingerprint,42},"SHA256"];
 Do[Export[FileNameJoin[{oldDir,"self",name<>".wxf"}],<|"Key"->oldReuseKey,"Value"->RGZWorkflow`Engine`\[Alpha]+3|>,"WXF"],{name,{"m0-sum-3","matrix-1","gauge-1","connected"}}];
 RGZWorkflow`Private`storeResult["QQBeforeGauge",<|"QQ"->oldReuse|>];
 copied=RGZWorkflow`Private`qqGaugeReuseM0[newReuse];
 migrated=Import[FileNameJoin[{newDir,"self","m0-sum-3.wxf"}],"WXF"];
 {copied===2&&migrated["Key"]===newReuseKey&&migrated["Value"]===RGZWorkflow`Engine`\[Alpha]+3,
 !FileExistsQ[FileNameJoin[{newDir,"self","gauge-1.wxf"}]]&&!FileExistsQ[FileNameJoin[{newDir,"self","connected.wxf"}]],
 RGZWorkflow`Private`qqGaugeReuseM0[newReuse]===0,
 RGZWorkflow`Private`qqGaugeReuseM0[Join[newReuse,<|"MixedSourceHash"->43|>]]===0}];
record["OnlyPreGaugeM0CheckpointsMigrateWithNewKeys",reuseChecks[[1]]];
record["EvaluatedGaugeAndConnectedCachesNeverMigrate",reuseChecks[[2]]];
record["ExistingNewCheckpointsAreNotOverwritten",reuseChecks[[3]]];
record["DifferentMixedSourceCannotReuseM0",reuseChecks[[4]]];
record["FailedNewCheckCannotExposeAnOldPass",Block[{RGZWorkflow`Private`$results=RGZWorkflow`Private`$results},
 RGZWorkflow`Private`storeResult["QQGaugeDirect",<|"GaugeIndependenceVerified"->True|>];
 RGZWorkflow`Private`storeResult["QQGaugeAmplitude",packet[TerminatedEvaluation["IterationLimit"]]];
 RGZQQGaugeCheck[30];MissingQ[RGZResult["QQGaugeDirect"]]]];
record["AssignedMassCannotClaimGeneralParameterProof",Block[{RGZWorkflow`Engine`m=1},FailureQ[RGZQQGaugeCheck[30]]]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
