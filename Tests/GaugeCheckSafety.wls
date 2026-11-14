(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Gauge residual checks fail closed at input, coefficient, cache and batch
   boundaries. All writable caches are temporary; no production Work is used. *)
If[$Context=!="RGZWorkflow`Engine`",
 With[{rgzGTestScript=$InputFileName,rgzGTestRoot=DirectoryName[DirectoryName[$InputFileName]]},
  Get[FileNameJoin[{rgzGTestRoot,"RGZWorkflow.wl"}]];
  If[!AssociationQ[RGZWorkflow`RGZInitialize[CreateDirectory[]]],Exit[1]];
  If[!AssociationQ[RGZWorkflow`RGZTreeBenchmark[60]],Exit[1]];
  If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
  RGZWorkflow`Private`loadLoopEngine[];Scan[If[FailureQ[RGZWorkflow`Private`engineGet[#]],Exit[1]]&,
   {"Algebra/TwoPointIntegrals.wl","Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl"}];
  RGZWorkflow`Private`engineEvaluation[Get[rgzGTestScript]]];Exit[1]];
rgzGTestChecks=<||>;rgzGTestRec[name_,x_]:=(AssociateTo[rgzGTestChecks,name->TrueQ[x]];Print[name," ",TrueQ[x]]);
rgzGTestZeroQ[x_] := AssociationQ[x]&&Lookup[x,"Status",None]==="The checked expression has zero gauge residual";
RGZMXSetDisplay["Quiet"];FCClearScalarProducts[];
rgzGTestData=<|"T"->Array[rgzGTestPiT,{5,5}],"L"->Array[rgzGTestPiL,{8,8}],
 "TFieldOrder"->RGZMixBasis["T"],"LFieldOrder"->RGZMixBasis["L"]|>;
Do[
 rgzGTestRec["RejectInvalidInput"<>ToString[bad,InputForm],FailureQ[RGZGaugeCompare[bad,1,10,"CacheDirectory"->None]]];
 rgzGTestRec["RejectInvalidEqualBranches"<>ToString[bad,InputForm],FailureQ[RGZGaugeCoefficient[bad,bad,1]]];
 rgzGTestRec["RejectInvalidPrepare"<>ToString[bad,InputForm],FailureQ[RGZGaugePrepare[bad,1,10]]];
 rgzGTestRec["InvalidStateNeverZero"<>ToString[bad,InputForm],RGZGaugeState[bad]=!="Zero"];
 rgzGTestBroken=Join[rgzGTestData,<|"T"->ReplacePart[rgzGTestData["T"],{1,1}->bad]|>];
 rgzGTestRec["RejectInvalidMatrixEntry"<>ToString[bad,InputForm],FailureQ[RGZGaugeStart[rgzGTestBroken]]],
 {bad,{System`TerminatedEvaluation["IterationLimit"],Indeterminate,ComplexInfinity,Infinity,$TimedOut,
   h[System`TerminatedEvaluation["IterationLimit"]],h[Failure["Fixture",<||>]]}}];
Do[
 rgzGTestRec["RejectForeignGaugeContext"<>ToString[expr,InputForm],MatchQ[RGZGaugeCompare[expr,1,10,"CacheDirectory"->None],Failure["GaugeSymbolContext",_]]],
 {expr,{Global`\[Alpha] A0[rgzGTestMass],A0[Global`\[Alpha] rgzGTestMass],(\[Alpha]+Global`\[Alpha]) A0[rgzGTestMass],Global`alpha A0[rgzGTestMass],FCGV["\[Alpha]"] A0[rgzGTestMass],FCGV["alpha"] A0[rgzGTestMass]}}];
rgzGTestRec["DirectCoefficientRejectsForeignContext",MatchQ[RGZGaugeCoefficient[Global`\[Alpha],Global`\[Alpha],1],Failure["GaugeSymbolContext",_]]];
rgzGTestRec["AssignedEngineGaugeRejected",Block[{\[Alpha]=1},FailureQ[RGZGaugeCoefficient[1,1,1]]]];
rgzGTestRec["GenuineGaugeIndependentExpressionPasses",rgzGTestZeroQ[RGZGaugeCompare[A0[rgzGTestMass],1,10,"CacheDirectory"->None]]];
rgzGTestRec["GaugeDependentCoefficientDoesNotPass",!rgzGTestZeroQ[RGZGaugeCompare[\[Alpha] A0[rgzGTestMass],1,10,"CacheDirectory"->None]]];
rgzGTestRec["AllSuppliedContributionsCombined",rgzGTestZeroQ[RGZGaugeCompare[{\[Alpha] A0[rgzGTestMass],-\[Alpha] A0[rgzGTestMass]},1,10,"CacheDirectory"->None]]];
rgzGTestRec["GaugeDependentPVMassNotMistakenForConstant",!rgzGTestZeroQ[RGZGaugeCompare[A0[rgzLoopM1^2],1,10,"CacheDirectory"->None]]];
rgzGTestCoeff=RGZGaugeCoefficient[rgzLoopM1^2,rgzLoopM1^2,1];
rgzGTestRec["CoefficientPoleMassesRestoredBeforeGaugeSubstitution",RGZGaugeCoefficientResultQ[rgzGTestCoeff]&&
 Cancel[Together[rgzGTestCoeff["ResidualCoefficient"]-(-I Sqrt[\[Alpha] M^4]+I Sqrt[M^4])]]===0];
rgzGTestRec["B0UsesOnlyMassExchangeSymmetry",SameQ[RGZGaugeCanonicalPV[B0[s,rgzGTestA,rgzGTestB]],RGZGaugeCanonicalPV[B0[s,rgzGTestB,rgzGTestA]]]];
rgzGTestRec["GeneralC0PermutationNotGuessed",!SameQ[RGZGaugeCanonicalPV[C0[s,t,u,rgzGTestA,rgzGTestB,rgzGTestC]],RGZGaugeCanonicalPV[C0[t,s,u,rgzGTestA,rgzGTestB,rgzGTestC]]]];
rgzGTestCoeff=RGZGaugeCoefficient[Sqrt[rgzGTestX^2],0,1];
rgzGTestRec["PrincipalSquareRootBranchRetained",RGZGaugeCoefficientResultQ[rgzGTestCoeff]&&(rgzGTestCoeff["ResidualCoefficient"]/.rgzGTestX->-2)===2];
rgzGTestCoeff=RGZGaugeCoefficient[Sqrt[1+Sqrt[rgzGTestX]],0,1];
rgzGTestRec["NestedRootDefinitionsRetained",RGZGaugeCoefficientResultQ[rgzGTestCoeff]&&(rgzGTestCoeff["ResidualCoefficient"]/.rgzGTestX->9)===2];
rgzGTestRec["SingularRootDenominatorRejected",FailureQ[RGZGaugeBatchReduce[1/(rgzGTestR^2-rgzGTestA),{rgzGTestR},{rgzGTestA}]]];
rgzGTestRec["SparseSingularDenominatorRejected",FailureQ[RGZGaugeBatchMerge[{1/(rgzGTestR^2-rgzGTestA),1},{rgzGTestR},{rgzGTestA}]]];

(* Independent tree inversion and arbitrary non-symmetric insertions. *)
rgzGTestWork=RGZGaugeStart[rgzGTestData];rgzGTestBench=RGZWorkflow`RGZResult["TreeBenchmark"];
rgzGTestRules=Thread[Lookup[rgzGTestBench["Parameters"],{"s","m","mu","mG","alpha","M"}]->{FCI[SPD[p,p]],m,\[Mu],mG,\[Alpha],M}];
rgzGTestDT=rgzGTestBench["DT"]/.rgzGTestRules;rgzGTestDL=rgzGTestBench["DL"]/.rgzGTestRules;
rgzGTestCancel[x_] := AllTrue[Flatten[{x}],Cancel[Together[#]]===0&];
rgzGTestRec["TransverseExternalWeightsMatchIndependentInverse",rgzGTestCancel[FCI[rgzGTestWork["TreeTransverseAA"] rgzGTestWork["TransverseExternalLegsweight"]]-rgzGTestDT[[1]]]];
rgzGTestRec["LeftLongitudinalSignsMatchIndependentInverse",rgzGTestCancel[FCI[rgzGTestWork["LongitudinalLeftExternalLegs"]]-rgzGTestDL[[1]]]];
rgzGTestRec["RightLongitudinalSignsMatchIndependentInverse",rgzGTestCancel[FCI[rgzGTestWork["LongitudinalRightExternalLegs"]]-rgzGTestDL[[All,1]]]];
rgzGTestRec["NonsymmetricTransverseInsertionNoExtraFactor",rgzGTestCancel[FCI[rgzGTestWork["AttachedT"]]-(rgzGTestDT.rgzGTestData["T"].rgzGTestDT)[[1,1]]]];
rgzGTestRec["NonsymmetricLongitudinalInsertionNoTransposeShortcut",rgzGTestCancel[FCI[rgzGTestWork["AttachedL"]]-(rgzGTestDL.rgzGTestData["L"].rgzGTestDL)[[1,1]]]];

(* Real validated cache round trip, then isolated corruption. *)
rgzGTestRoot=CreateDirectory[];
rgzGTestFirst=RGZGaugeCompare[A0[rgzGTestMass],1,10,"CacheDirectory"->rgzGTestRoot];
rgzGTestAgain=RGZGaugeCompare[A0[rgzGTestMass],1,10,"CacheDirectory"->rgzGTestRoot];
rgzGTestRec["ValidCoefficientCacheReused",rgzGTestZeroQ[rgzGTestAgain]&&AllTrue[rgzGTestAgain["ResidualParts"],#["Source"]==="CacheRestored"&]];
rgzGTestFile=First[FileNames["coefficient_*.wl",rgzGTestRoot,Infinity]];rgzGTestSaved=Get[rgzGTestFile];
rgzGTestBadRow=Join[rgzGTestSaved["Result"],<|"ResidualCoefficient"->Indeterminate,"Status"->"Zero"|>];
Put[Join[rgzGTestSaved,<|"Result"->rgzGTestBadRow|>],rgzGTestFile];
rgzGTestRec["InvalidZeroLabelCacheRejected",MissingQ[RGZGaugeCacheRead[rgzGTestFile,rgzGTestSaved["Identity"]]]];
rgzGTestAgain=RGZGaugeCompare[A0[rgzGTestMass],1,10,"CacheDirectory"->rgzGTestRoot];
rgzGTestRec["PoisonedCoefficientRecomputed",rgzGTestZeroQ[rgzGTestAgain]&&AllTrue[rgzGTestAgain["ResidualParts"],#["Source"]=!="CacheRestored"&]];
rgzGTestBadRow=Join[rgzGTestSaved["Result"],<|"ResidualCoefficient"->1,"RadicalSymbolCoefficient"->1,"Status"->"Zero"|>];
rgzGTestRec["StatusMustMatchCoefficientValue",!RGZGaugeRowQ[rgzGTestBadRow]];
rgzGTestRec["InvalidCoefficientCacheWriteRejected",FailureQ[RGZGaugeCacheWrite[rgzGTestBadRow,FileNameJoin[{rgzGTestRoot,"bad.wl"}],1]]&&!FileExistsQ[FileNameJoin[{rgzGTestRoot,"bad.wl"}]]];
Put[KeyDrop[rgzGTestSaved,"ValidationPolicy"],rgzGTestFile];
rgzGTestRec["LegacyCoefficientCacheNotAssumedValidated",MissingQ[RGZGaugeCacheRead[rgzGTestFile,rgzGTestSaved["Identity"]]]];
Internal`InheritedBlock[{RGZGaugeCoefficient},Clear[RGZGaugeCoefficient];RGZGaugeCoefficient[___]:=<|"ResidualCoefficient"->Indeterminate,"RadicalSymbolCoefficient"->Indeterminate,"RadicalDefinition"->{}|>;
 rgzGTestFault=RGZGaugeCompare[A0[rgzGTestMass],1,10,"CacheDirectory"->None];
 rgzGTestRec["InvalidFreshCoefficientCannotProduceZeroReport",!rgzGTestZeroQ[rgzGTestFault]&&Lookup[Lookup[rgzGTestFault,"Statistics",<||>],"Failed",0]>0]];
Internal`InheritedBlock[{RGZGaugePoleSquares},Clear[RGZGaugePoleSquares];RGZGaugePoleSquares[___]:=System`TerminatedEvaluation["IterationLimit"];
 rgzGTestRec["InvalidTransformedEqualBranchesRejected",FailureQ[RGZGaugeCoefficient[1,1,1]]]];

rgzGTestBatchFile=FileNameJoin[{rgzGTestRoot,"batch.wl"}];
rgzGTestRec["ValidZeroBatchCacheRoundTrip",RGZGaugeBatchCacheWrite[0,rgzGTestBatchFile,17]===True&&RGZGaugeBatchCacheRead[rgzGTestBatchFile,17]===0];
Do[
 Put[<|"Identity"->17,"ValidationPolicy"->"GaugeFiniteScalar/3","Result"->bad|>,rgzGTestBatchFile];
 rgzGTestRec["InvalidBatchCacheRejected"<>ToString[bad,InputForm],MissingQ[RGZGaugeBatchCacheRead[rgzGTestBatchFile,17]]];
 rgzGTestRec["InvalidBatchWriteRejected"<>ToString[bad,InputForm],FailureQ[RGZGaugeBatchCacheWrite[bad,None,17]]];
 rgzGTestRec["InvalidBatchReductionRejected"<>ToString[bad,InputForm],FailureQ[RGZGaugeBatchReduce[bad,{},{}]]];
 rgzGTestRec["InvalidSparseMergeRejected"<>ToString[bad,InputForm],FailureQ[RGZGaugeBatchMerge[{bad,1},{},{}]]],
 {bad,{System`TerminatedEvaluation["IterationLimit"],Indeterminate,ComplexInfinity,Global`\[Alpha]}}];
Put[<|"Identity"->17,"ValidationPolicy"->"GaugeFiniteScalar/3","Result"-><||>|>,rgzGTestBatchFile];
rgzGTestRec["MalformedBatchPreparationCacheRejected",MissingQ[RGZGaugeBatchCacheRead[rgzGTestBatchFile,17]]];
rgzGaugeSparseTestMemo=<|"Identity"->Hash[{RGZGaugeMergeContext[],{-rgzGTestX,rgzGTestX},{},{}},"SHA256"],
 "Result"->System`TerminatedEvaluation["IterationLimit"],"Diagnostic"-><||>|>;
rgzGTestRec["PoisonedMemoryMemoIgnored",RGZGaugeBatchMerge[{-rgzGTestX,rgzGTestX},{},{}]===0];
rgzGaugeStoppedUnit=<|"AlphaShortCoefficient"->1,"ReferenceShortCoefficient"->1,"ReferenceGauge"->1,"ID"->1,"MasterIntegral"->1|>;
rgzGTestBatch=RGZGaugeResumeStopped[10,"GroupSize"->1,"CacheDirectory"->None];
rgzGTestRec["ValidResumableBatchStillProvesSingleCoefficient",AssociationQ[rgzGTestBatch]&&rgzGTestBatch["CoefficientStatus"]==="Zero"];
rgzGaugeStoppedUnit=Join[rgzGaugeStoppedUnit,<|"AlphaShortCoefficient"->Indeterminate|>];
rgzGTestRec["InvalidStoppedInputRejected",FailureQ[RGZGaugeResumeStopped[10,"CacheDirectory"->None]]];
(* The optional fixed-denominator algorithm must not replace ordinary IO. *)
rgzGTestBatchDefinitions={DownValues[RGZGaugeBatchCacheRead],DownValues[RGZGaugeBatchCacheWrite]};
RGZWorkflow`Private`engineGet["Algebra/CommonDenominator.wl",False];
rgzGTestRec["FixedModuleDoesNotReplaceOrdinaryBatchIO",rgzGTestBatchDefinitions==={DownValues[RGZGaugeBatchCacheRead],DownValues[RGZGaugeBatchCacheWrite]}];
rgzGTestFixedDefinitions={DownValues[RGZGaugeFixedCacheRead],DownValues[RGZGaugeFixedCacheWrite]};
RGZWorkflow`Private`engineGet["Algebra/GaugeCoefficients.wl",False];
rgzGTestRec["OrdinaryReloadDoesNotReplaceFixedBatchIO",rgzGTestFixedDefinitions==={DownValues[RGZGaugeFixedCacheRead],DownValues[RGZGaugeFixedCacheWrite]}];
rgzGTestTextPath=FileNameJoin[{rgzGTestRoot,"explicit-text.wl"}];
rgzGTestRec["OrdinaryWriteRouteRemainsText",RGZGaugeBatchCacheWrite[0,rgzGTestTextPath,33]===True&&FileExistsQ[rgzGTestTextPath]&&!FileExistsQ[rgzGTestTextPath<>".wxf"]&&RGZGaugeBatchCacheRead[rgzGTestTextPath,33]===0];
rgzGTestBinaryPath=FileNameJoin[{rgzGTestRoot,"explicit-fixed.wl"}];
rgzGTestRec["FixedWriteRouteRemainsWXF",RGZGaugeFixedCacheWrite[0,rgzGTestBinaryPath,34]===True&&!FileExistsQ[rgzGTestBinaryPath]&&FileExistsQ[rgzGTestBinaryPath<>".wxf"]&&RGZGaugeFixedCacheRead[rgzGTestBinaryPath,34]===0];
rgzGTestRec["PriorOrdinaryWXFScalarStillReadable",RGZGaugeBatchCacheRead[rgzGTestBinaryPath,34]===0];
rgzGTestPrepared=RGZGaugeBatchPrepare[1,1,1,1];
rgzGTestLegacyPreparedPath=FileNameJoin[{rgzGTestRoot,"prior-prepared.wl"}];
rgzGTestRec["PriorOrdinaryWXFPreparationStillReadable",RGZGaugeFixedCacheWrite[rgzGTestPrepared,rgzGTestLegacyPreparedPath,35]===True&&RGZGaugeBatchCacheRead[rgzGTestLegacyPreparedPath,35]===rgzGTestPrepared];
rgzGTestMalformedPath=FileNameJoin[{rgzGTestRoot,"malformed-prior-prepared.wl"}];
RGZGaugeFixedCacheWrite[<||>,rgzGTestMalformedPath,36];
rgzGTestRec["PriorWXFStillRequiresOrdinaryPreparationShape",MissingQ[RGZGaugeBatchCacheRead[rgzGTestMalformedPath,36]]];

Print["SUMMARY ",<|"Checks"->Length[rgzGTestChecks],"Failed"->Keys[Select[rgzGTestChecks,!TrueQ[#]&]]|>];
Exit[If[AllTrue[Values[rgzGTestChecks],TrueQ],0,1]];
