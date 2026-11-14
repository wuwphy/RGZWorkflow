(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Four PT follow-up regressions. Upstream formula/NAV fixtures are
   synthetic; real stage runners, integrators and comparators are used.
   All writable data are isolated in a temporary Work directory. *)
If[$Context=!="RGZWorkflow`Engine`",
 With[{rgzPTestScript=$InputFileName,rgzPTestRoot=DirectoryName[DirectoryName[$InputFileName]]},
  Get[FileNameJoin[{rgzPTestRoot,"RGZWorkflow.wl"}]];
  If[!AssociationQ[RGZWorkflow`RGZInitialize[CreateDirectory[]]],Exit[1]];
  If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
  RGZWorkflow`Private`loadLoopEngine[];Scan[If[FailureQ[RGZWorkflow`Private`engineGet[#]],Exit[1]]&,
   {"Algebra/TwoPointIntegrals.wl",
    "Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","Pinch/Kernels.wl","Algebra/CommonDenominator.wl","Pinch/Continuation.wl","Pinch/VertexWard.wl"}];
  RGZWorkflow`Private`engineEvaluation[Get[rgzPTestScript]]];Exit[1]];
rgzPTestChecks=<||>;rgzPTestRec[name_,x_]:=(AssociateTo[rgzPTestChecks,name->TrueQ[x]];Print[name," ",TrueQ[x]]);
RGZMXSetDisplay["Quiet"];FCClearScalarProducts[];
rgzPTestNames={"box","navAll","avAll","qseAll"};
rgzPTestQCD[value_] := <|"PerTermResults"->Table[<|"Parts"->name,"Status"->"PassedBeforeAndAfterIntegration",
 "ComputedI11Coefficient"->If[name==="gse",value,1],"LiteratureI11Coefficient"->If[name==="gse",value,1]|>,{name,Append[rgzPTestNames,"gse"]}],
 "SumCheck"-><|"CancellationCheck"->"Passed","ComputedFiveSectorGaugeDifference / I11"->0|>|>;
rgzPTestRec["ValidQCDConventionAccepted",AssociationQ[RGZPTCheckQCD[rgzPTestQCD[3]]]];
Do[rgzPTestRec["InvalidOriginalQCDCoefficientsRejected"<>ToString[bad,InputForm],FailureQ[RGZPTCheckQCD[rgzPTestQCD[bad]]]],
 {bad,{System`TerminatedEvaluation["IterationLimit"],$TimedOut,$Aborted,Indeterminate,ComplexInfinity,h[Indeterminate],h[ComplexInfinity],h[Failure["Fixture",<||>]],None,Null,Automatic,h[None]}}];
rgzPTestQ=rgzPTestQCD[3];rgzPTestRows=rgzPTestQ["PerTermResults"];
rgzPTestRows[[1]]=Join[First[rgzPTestRows],<|"ComputedValue"->h[System`TerminatedEvaluation["IterationLimit"]]|>];
rgzPTestRec["InvalidOtherQCDBenchmarkPayloadRejected",FailureQ[RGZPTCheckQCD[Join[rgzPTestQ,<|"PerTermResults"->rgzPTestRows|>]]]];
rgzPTestMismatch=rgzPTestQCD[3];rgzPTestRows=rgzPTestMismatch["PerTermResults"];
rgzPTestRows[[-1]]=Join[Last[rgzPTestRows],<|"ComputedI11Coefficient"->4|>];
rgzPTestRec["DifferentFiniteQCDCoefficientsRejected",FailureQ[RGZPTCheckQCD[Join[rgzPTestMismatch,<|"PerTermResults"->rgzPTestRows|>]]]];
rgzPTestRec["ZeroGSEReferenceStillRejected",FailureQ[RGZPTCheckQCD[rgzPTestQCD[0]]]];
Internal`InheritedBlock[{Cancel},Unprotect[Cancel];Clear[Cancel];Cancel[___]:=System`TerminatedEvaluation["IterationLimit"];
 rgzPTestRec["InvalidQCDSubtractionResultRejected",FailureQ[RGZPTCheckQCD[rgzPTestQCD[3]]]]];

rgzPTestKernel=FCI[MTD[\[Mu],\[Nu]] FAD[{q,rgzPTestMass}]];
rgzPTestData=<|"T"->ConstantArray[0,{5,5}],"L"->ConstantArray[0,{8,8}],"TFieldOrder"->RGZMixBasis["T"],"LFieldOrder"->RGZMixBasis["L"]|>;
Internal`InheritedBlock[{RGZPTCheckQCD,RGZCandidatePinches},
 Clear[RGZPTCheckQCD,RGZCandidatePinches];
 RGZPTCheckQCD[___]:=<|"Status"->"Passed","SharedFactor"->1|>;
 RGZCandidatePinches[___]:=Join[AssociationThread[rgzPTestNames,ConstantArray[rgzPTestKernel,4]],<|"Mode"->"FourPinch","ReferenceGauge"->None|>];
 Clear[rgzPTJob];RGZPTLegacyStart[rgzPTestData];
 rgzPTestRec["NativeFourPartPipelineCompletes",AssociationQ[RGZPTComputeAll[30]]];
 rgzPTestRec["EveryCompletedStageHasVerifiedProvenance",RGZPTJobCheck[]===True&&
  AllTrue[Values[rgzPTJob["Parts"]],RGZWorkflow`Private`ptPartProvenanceQ[#,rgzPTJob["Environment"]]&]];
 rgzPTestPart=rgzPTJob["Parts"]["box"];rgzPTestOriginalScalar=rgzPTestPart["Result"]["Scalar"];
 rgzPTestRec["ValidComparisonProvesFixtureZero",AssociationQ[RGZPTCompare[1,10]]];
 rgzPTestResult=RGZPTResult[];rgzPTestRec["ComparisonBoundToCurrentSum",AssociationQ[rgzPTestResult["GaugeCheck"]]&&
  rgzPTestResult["GaugeCheck"]["PTSourceIdentity"]===RGZWorkflow`Private`ptComparisonIdentity[rgzPTJob]];

 (* Clearing a stage chain leaves good disk records available for exact
    input/environment/provenance matching. *)
 RGZPTSetPart["box",Join[rgzPTestPart,<|"Result"-><||>|>]];
 rgzPTestDisk=RGZPTRun["box","Project",10];
 rgzPTestRec["ValidDiskStageRestored",AssociationQ[rgzPTestDisk]&&rgzPTestDisk["Status"]==="CacheRestored"];
 rgzPTestPath=FileNameJoin[{rgzPTJob["Directory"],"part_1_1.wl"}];rgzPTestRecord=Get[rgzPTestPath];
 RGZPTSetPart["box",Join[rgzPTJob["Parts"]["box"],<|"Result"-><||>|>]];
 Put[KeyDrop[rgzPTestRecord,"Provenance"],rgzPTestPath];rgzPTestDisk=RGZPTRun["box","Project",10];
 rgzPTestRec["UnprovenancedDiskStageRecomputed",AssociationQ[rgzPTestDisk]&&rgzPTestDisk["Status"]==="Complete"];
 RGZPTSetPart["box",Join[rgzPTJob["Parts"]["box"],<|"Result"-><||>|>]];
 rgzPTestBadProof=Join[rgzPTestRecord["Provenance"],<|"Input"->-1|>];
 Put[Join[rgzPTestRecord,<|"Provenance"->rgzPTestBadProof|>],rgzPTestPath];rgzPTestDisk=RGZPTRun["box","Project",10];
 rgzPTestRec["MismatchedDiskProofRecomputed",AssociationQ[rgzPTestDisk]&&rgzPTestDisk["Status"]==="Complete"];
 RGZPTComputePart["box",30];RGZPTCompare[1,10];rgzPTestPart=rgzPTJob["Parts"]["box"];
 RGZPTSetPart["box",Join[rgzPTestPart,<|"OriginalExpression"->2 rgzPTestKernel|>]];
 rgzPTestRec["ChangedOriginalClearsAllStages",rgzPTJob["Parts"]["box"]["Result"]===<||>&&rgzPTJob["Parts"]["box"]["StageProvenance"]===<||>];
 rgzPTestRec["ChangedOriginalClearsComparison",MissingQ[rgzPTJob["Compare"]]];
 rgzPTestRec["ChangedOriginalRecomputedCorrectly",AssociationQ[RGZPTComputePart["box",30]]&&
  Cancel[Together[rgzPTJob["Parts"]["box"]["Result"]["Scalar"]-2 rgzPTestOriginalScalar]]===0];
 RGZPTCompare[1,10];rgzPTestPart=rgzPTJob["Parts"]["box"];
 rgzPTestChangedResults=Join[rgzPTestPart["Result"],<|"Project"->3 rgzPTestPart["Result"]["Project"]|>];
 RGZPTSetPart["box",Join[rgzPTestPart,<|"Result"->rgzPTestChangedResults|>]];
 rgzPTestRec["ChangedStageInvalidatesDependentChain",rgzPTJob["Parts"]["box"]["Result"]===<||>];
 RGZPTComputePart["box",30];RGZPTCompare[1,10];rgzPTestPart=rgzPTJob["Parts"]["box"];
 RGZPTSetPart["box",Join[rgzPTestPart,<|"OriginalExpression"->\[Alpha] rgzPTestKernel|>]];RGZPTComputePart["box",30];
 rgzPTestResult=RGZPTResult[];
 rgzPTestRec["RecomputedChangedKernelCannotAdvertiseOldZero",MissingQ[rgzPTestResult["GaugeCheck"]]&&!FreeQ[rgzPTestResult["CandidateSumT"],\[Alpha]]];
 rgzPTestNewReport=RGZPTCompare[1,10];
 rgzPTestRec["FreshChangedComparisonIsNonzero",AssociationQ[rgzPTestNewReport]&&rgzPTestNewReport["Status"]=!="The checked expression has zero gauge residual"];
 rgzPTJob=Join[rgzPTJob,<|"SelfEnergyParts"->ReplacePart[rgzPTJob["SelfEnergyParts"],1->7]|>];
 rgzPTestRec["DirectSumChangeSuppressesStaleReport",MissingQ[RGZPTResult[]["GaugeCheck"]]];

 (* A valid session round trip preserves the proof chain; an inconsistent
    raw task must not pass restore's pure packet checks. *)
 rgzPTestSession=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"pt-session.wxf"}];
 rgzPTestRec["CurrentProofChainSavesAndRestores",AssociationQ[RGZWorkflow`RGZSave["File"->rgzPTestSession]]&&
  AssociationQ[RGZWorkflow`RGZRestore["File"->rgzPTestSession]]&&RGZPTJobCheck[]===True];
 rgzPTestRawParts=rgzPTJob["Parts"];
 rgzPTestRawParts["box"]=Join[rgzPTestRawParts["box"],<|"OriginalExpression"->9 rgzPTestKernel|>];
 AssociateTo[rgzPTJob,"Parts"->rgzPTestRawParts];
 rgzPTestRec["RawInputMutationWithoutProofRejected",FailureQ[RGZPTRun["box","Project",10]]];
 RGZWorkflow`RGZSave["File"->rgzPTestSession];rgzPTestRestored=RGZWorkflow`RGZRestore["File"->rgzPTestSession];
 rgzPTestRec["RestoreRejectsDisconnectedPTStageChain",AssociationQ[rgzPTestRestored]&&rgzPTestRestored["PTStatus"]==="RestartRequired"];

 (* Minimal synthetic full NAV evidence; no physical NAV certificate is
    claimed. Both the packet validator and the full continuation are real. *)
 Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],<|"Fixture"->True|>,"WXF"];
 rgzPTestNAV=<|"Schema"->"RGZNAVPinch/1","ConnectionCount"->1,
  "Connections"->{<|"Connection"->1,"Fields"->{"A","A","A"},"Kernel"->0,"ExactPolynomialReconstruction"->True,"OriginalTensorReconstruction"->True,
    "ValidationPolicy"->"NAVWard/3","InputIdentity"->123,"KernelHash"->Hash[0,"SHA256"]|>},
  "Kernel"->0,"Fingerprint"->RGZWorkflow`Private`navFormulaFingerprint[]|>;
 Clear[rgzPTJob];rgzPTestInit=RGZPTLegacyStart[rgzPTestData,"NAVPacket"->rgzPTestNAV];
 rgzPTestRec["FullNAVTaskRetainsValidatedPacket",AssociationQ[rgzPTestInit]&&rgzPTJob["NAVPacket"]===rgzPTestNAV&&RGZPTJobCheck[]===True];
 rgzPTestFull=RGZPTContinueAll[rgzPTestData,30,10];
 rgzPTestRec["ContinueAllPreservesFullNAVScope",AssociationQ[rgzPTestFull]&&rgzPTJob["NAVMode"]==="FullMatrix"&&TrueQ[rgzPTestFull["FullNAVIncluded"]]];
 rgzPTestRec["FullNAVContinuationActuallyCompletes",RGZPTJobCheck[]===True&&AssociationQ[RGZPTResult[]]];
 rgzPTJob=KeyDrop[rgzPTJob,"NAVPacket"];
 rgzPTestMissing=RGZPTContinueAll[rgzPTestData,30,10];
 rgzPTestRec["MissingFullNAVProofStopsWithoutDowngrade",MatchQ[rgzPTestMissing,Failure["FullNAVProofRequired",_]]&&rgzPTJob["NAVMode"]==="FullMatrix"];
 AssociateTo[rgzPTJob,"NAVPacket"->rgzPTestNAV];
 rgzPTestRec["ConflictingNAVOptionsRejected",MatchQ[RGZPTContinueAll[rgzPTestData,30,10,"NAVMode"->"AAReplacement","NAVPacket"->rgzPTestNAV],Failure["ConflictingNAVOptions",_]]];
 rgzPTestAA=RGZPTContinueAll[rgzPTestData,30,10,"NAVMode"->"AAReplacement","ComparisonGauge"->2];
 rgzPTestRec["ExplicitScopeChangeIsHonouredAndReported",AssociationQ[rgzPTestAA]&&rgzPTJob["NAVMode"]==="AAReplacement"&&rgzPTestAA["NAVMode"]==="AAReplacement"&&!TrueQ[rgzPTestAA["FullNAVIncluded"]]];
 rgzPTestRec["ExplicitComparisonGaugeHonoured",AssociationQ[rgzPTestAA]&&rgzPTestAA["ReferenceGauge"]===2];
 rgzPTestRec["GaugeDifferenceNotSilentlyChangedToFourPinch",MatchQ[RGZPTContinueAll[rgzPTestData,30,10,"Mode"->"GaugeDifference"],Failure["FullPinchRequired",_]]]];
Print["SUMMARY ",<|"Checks"->Length[rgzPTestChecks],"Failed"->Keys[Select[rgzPTestChecks,!TrueQ[#]&]]|>];
Exit[If[AllTrue[Values[rgzPTestChecks],TrueQ],0,1]];
