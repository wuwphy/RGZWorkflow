(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Native accelerator parity and PT policy/invalid-result regressions.
   All caches and fixtures are isolated; no production Work is restored. *)
If[$Context=!="RGZWorkflow`Engine`",
 With[{rgzPTTestScript=$InputFileName,rgzPTTestRoot=DirectoryName[DirectoryName[$InputFileName]]},
  Get[FileNameJoin[{rgzPTTestRoot,"RGZWorkflow.wl"}]];
  If[!AssociationQ[RGZWorkflow`RGZInitialize[CreateDirectory[]]],Exit[1]];
  If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
  RGZWorkflow`Private`loadLoopEngine[];Scan[If[FailureQ[RGZWorkflow`Private`engineGet[#]],Exit[1]]&,
   {"Algebra/TwoPointIntegrals.wl",
    "Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","Pinch/Kernels.wl","Algebra/CommonDenominator.wl","Pinch/Continuation.wl"}];
  RGZWorkflow`Private`engineEvaluation[Get[rgzPTTestScript]]];Exit[1]];
rgzPTTestChecks=<||>;
rgzPTTestRec[name_,value_]:=(AssociateTo[rgzPTTestChecks,name->TrueQ[value]];Print[name," ",TrueQ[value]]);
RGZMXSetDisplay["Quiet"];RGZMXSetAcceleration["Enabled"->True];FCClearScalarProducts[];
(* Native TID keeps degenerate B0/C0 masters. Use their exact one-point
   Gamma recurrence at nonzero mass before comparing to the A0 form. *)
rgzPTTestCanonical[x_] := PaVeReduce[x] /. (
 HoldPattern[PaVe[0,inv_List,masses_List,___Rule]] /;
 Length[masses]>0&&Length[DeleteDuplicates[masses]]===1&&
 !TrueQ[First[masses]===0]&&AllTrue[inv,#===0&] :>
 With[{n=Length[masses],a=First[masses]},
  (-1)^(n-1) Product[t-D/2,{t,1,n-1}] A0[a]/((n-1)! a^(n-1))]);
rgzPTTestCompare[name_,kernel_] := Module[{fast,ordinary,diff,method},
 fast=RGZMXFastEvaluate[kernel];method=rgzMXFastLastMethod;
 ordinary=TID[kernel,q,ToPaVe->True,UsePaVeBasis->True,ApartFF->False];
 diff=If[RGZMXValidValueQ[{fast,ordinary}],Cancel[Together[rgzPTTestCanonical[fast]-rgzPTTestCanonical[ordinary]]],Missing["Stopped"]];
 rgzPTTestRec[name<>"EqualsNativeTID",diff===0];
 rgzPTTestRec[name<>"UsesOptimizedPath",method==="ScalarIdentityReduce"]];
Block[{$Assumptions=True,$KeepLogDivergentScalelessIntegrals=False},
 Do[rgzPTTestCompare["Tadpole"<>ToString[n],FCI[FAD@@ConstantArray[{q,rgzPTTestMassA},n]]],{n,1,3}];
 Do[
  rgzPTTestKernel=FCI[FAD[{q+sign p,rgzPTTestMassA}]];
  rgzPTTestMetadata=RGZMXFastRational[rgzPTTestKernel];
  rgzPTTestRec["ShiftedTadpoleMetadata"<>ToString[sign],AssociationQ[rgzPTTestMetadata]&&rgzPTTestMetadata["Mass"]==={{sign,rgzPTTestMassA}}];
  rgzPTTestCompare["ShiftedTadpole"<>ToString[sign],rgzPTTestKernel];
  rgzPTTestCompare["ShiftedTadpoleNumerator"<>ToString[sign],rgzPTTestKernel FCI[SPD[p,q] SPD[q,q]]];
  rgzPTTestKernel=FCI[FAD[{q,rgzPTTestMassA},{q+sign p,rgzPTTestMassB}]];
  rgzPTTestMetadata=RGZMXFastRational[rgzPTTestKernel];
  rgzPTTestRec["BothBubbleMassesRecorded"<>ToString[sign],AssociationQ[rgzPTTestMetadata]&&
   Sort[rgzPTTestMetadata["Mass"]]===Sort[{{0,rgzPTTestMassA},{sign,rgzPTTestMassB}}]];
  Do[rgzPTTestCompare["Bubble"<>ToString[{sign,r,k}],rgzPTTestKernel FCI[SPD[p,q]^r SPD[q,q]^k]],{r,0,1},{k,0,1}];
  rgzPTTestKernel=FCI[FAD[{q,rgzPTTestMassA},{q,rgzPTTestMassA},{q+sign p,rgzPTTestMassB}]];
  rgzPTTestCompare["RepeatedBubble"<>ToString[sign],rgzPTTestKernel];
  rgzPTTestMetadata=RGZMXFastRational[rgzPTTestKernel];
  Internal`InheritedBlock[{TID},Clear[TID];TID[expr_,___]:=HoldComplete[expr];
   rgzPTTestDelegated=RGZMXFastTerm[First[rgzPTTestMetadata["Contributions"]],rgzPTTestMetadata["Variable"],rgzPTTestMetadata["Mass"]];
   rgzPTTestExpected=With[{fd=ChangeDimension[rgzPTTestKernel,D]},HoldComplete[fd]];
   rgzPTTestRec["RepeatedBubbleRouteMassesPowersAndUnitNumerator"<>ToString[sign],SameQ[rgzPTTestDelegated,rgzPTTestExpected]]],
 {sign,{-1,1}}];
 rgzPTTestRec["TwoShiftedRoutesStillFallBackSafely",MissingQ[RGZMXFastScalar[FCI[FAD[{q-p,rgzPTTestMassA},{q+p,rgzPTTestMassB}]]]]];
 rgzPTTestRec["J2RetainsGenericD",Cancel[Together[RGZMXFastTadpole[1,x,y,s,rgzPTTestA,2]-I Pi^2 (D-2) A0[rgzPTTestA]/(2 rgzPTTestA)]]===0];
 rgzPTTestRec["J3RetainsGenericD",Cancel[Together[RGZMXFastTadpole[1,x,y,s,rgzPTTestA,3]-I Pi^2 (D-2)(D-4) A0[rgzPTTestA]/(8 rgzPTTestA^2)]]===0];
 rgzPTTestRec["ScalelessZeroPolicyUnchanged",RGZMXFastEvaluate[FCI[FAD[{q,0},{q,0}]]]===0]];
Block[{$KeepLogDivergentScalelessIntegrals=True},
 rgzPTTestRec["ScalelessRetentionStillUsesNativeTID",SameQ[RGZMXFastEvaluate[FCI[FAD[{q,0},{q,0}]]],I Pi^2 B0[0,0,0]]&&rgzMXFastLastMethod==="TID"]];

(* Real PT initializer/runner, with explicitly synthetic upstream formula
   providers. The test kernel projects to the logarithmic scaleless core. *)
rgzPTTestNames={"box","navAll","avAll","qseAll"};
rgzPTTestInput=FCI[MTD[\[Mu],\[Nu]] FAD[{q,0},{q,0}]];
rgzPTTestData=<|"T"->ConstantArray[0,{5,5}],"L"->ConstantArray[0,{8,8}],
 "TFieldOrder"->RGZMixBasis["T"],"LFieldOrder"->RGZMixBasis["L"]|>;
rgzPTTestMode="FourPinch";
Internal`InheritedBlock[{RGZPTCheckQCD,RGZCandidatePinches},
 Clear[RGZPTCheckQCD,RGZCandidatePinches];
 RGZPTCheckQCD[___]:=<|"Status"->"Passed","SharedFactor"->1|>;
 RGZCandidatePinches[___]:=Join[AssociationThread[rgzPTTestNames,ConstantArray[rgzPTTestInput,4]],<|"Mode"->rgzPTTestMode,"ReferenceGauge"->0|>];
 Clear[rgzPTJob];
 Block[{$KeepLogDivergentScalelessIntegrals=False},
  rgzPTTestInit=RGZPTLegacyStart[rgzPTTestData,"Mode"->"FourPinch"];
  rgzPTTestRec["PTInitializerBuildsCurrentPolicyTask",AssociationQ[rgzPTTestInit]&&RGZPTJobCheck[]===True];
  rgzPTTestOldEnvironment=rgzPTJob["Environment"];
  rgzPTTestRun=RGZPTComputePart["box",30];
  rgzPTTestRec["PTCompleteOrdinaryScalelessPipeline",AssociationQ[rgzPTTestRun]&&rgzPTJob["Parts"]["box"]["Result"]["Scalar"]===0];
  rgzPTTestSame=RGZPTLegacyStart[rgzPTTestData,"Mode"->"FourPinch"];
  rgzPTTestRec["SameEnvironmentKeepsValidPreviousResults",AssociationQ[rgzPTTestSame]&&KeyExistsQ[rgzPTJob["Parts"]["box"]["Result"],"Scalar"]];
  rgzPTTestSkip=RGZPTRun["box","Integrate",10];
  rgzPTTestRec["SameEnvironmentAllowsValidSkip",AssociationQ[rgzPTTestSkip]&&rgzPTTestSkip["Status"]==="Complete; skipped"];
  Block[{$KeepLogDivergentScalelessIntegrals=True},
   rgzPTTestRec["ChangedPolicyRejectedByPTJobGuard",MatchQ[RGZPTJobCheck[],Failure["StalePTEnvironment",_]]];
   rgzPTTestRec["ChangedPolicyCannotSkipPTIntegral",MatchQ[RGZPTRun["box","Integrate",10],Failure["StalePTEnvironment",_]]]];
  Block[{$Assumptions=(m==\[Mu])},rgzPTTestRec["ChangedAssumptionsRejected",MatchQ[RGZPTJobCheck[],Failure["StalePTEnvironment",_]]]];
  SPD[p,p]=7;rgzPTTestRec["ChangedKinematicsRejected",MatchQ[RGZPTJobCheck[],Failure["StalePTEnvironment",_]]];FCClearScalarProducts[]];
 Block[{$KeepLogDivergentScalelessIntegrals=True},
  rgzPTTestNew=RGZPTLegacyStart[rgzPTTestData,"Mode"->"FourPinch"];
  rgzPTTestRec["NewPolicyChangesTaskFingerprint",AssociationQ[rgzPTTestNew]&&rgzPTJob["Environment"]=!=rgzPTTestOldEnvironment];
  rgzPTTestRec["NewPolicyDoesNotImportOldCompletedResults",AllTrue[Values[rgzPTJob["Parts"]],#["Result"]===<||>&]];
  rgzPTTestRun=RGZPTComputePart["box",30];
  rgzPTTestRec["PTNewPolicyActuallyRecomputesScalelessIntegral",AssociationQ[rgzPTTestRun]&&SameQ[rgzPTJob["Parts"]["box"]["Result"]["Scalar"],I Pi^2 B0[0,0,0]]];
  rgzPTTestMode="GaugeDifference";
  rgzPTTestNew=RGZPTLegacyStart[rgzPTTestData,"Mode"->"GaugeDifference"];
  rgzPTTestRec["ChangedModeDoesNotImportMatchingExpressions",AssociationQ[rgzPTTestNew]&&rgzPTJob["Parts"]["box"]["Result"]===<||>];
  rgzPTTestMode="FourPinch"];

 Do[
  rgzPTTestRec["InvalidValuePredicate"<>ToString[bad,InputForm],!RGZPTGood["Project",bad]];
  Internal`InheritedBlock[{RGZPTProject},Clear[RGZPTProject];RGZPTProject[___]:=bad;
   Clear[rgzPTJob];RGZPTLegacyStart[rgzPTTestData,"Mode"->"FourPinch"];
   rgzPTTestFirst=RGZPTRun["box","Project",10];
   rgzPTTestSaved=Get[FileNameJoin[{rgzPTJob["Directory"],"part_1_1.wl"}]];
   rgzPTTestRec["InvalidFreshStageNotSavedComplete"<>ToString[bad,InputForm],FailureQ[rgzPTTestFirst]&&
    rgzPTTestSaved["Status"]==="Incomplete"&&!KeyExistsQ[rgzPTJob["Parts"]["box"]["Result"],"Project"]]],
 {bad,{$TimedOut,System`TerminatedEvaluation["IterationLimit"],h[Failure["Fixture",<||>]],h[System`TerminatedEvaluation["IterationLimit"]],Indeterminate}}];

 Clear[rgzPTJob];RGZPTLegacyStart[rgzPTTestData,"Mode"->"FourPinch"];
 rgzPTTestPath=FileNameJoin[{rgzPTJob["Directory"],"part_1_1.wl"}];
 Put[<|"Input"->rgzPTTestInput,"Environment"->rgzPTJob["Environment"],"Status"->"Complete",
  "Result"->h[System`TerminatedEvaluation["IterationLimit"]]|>,rgzPTTestPath];
 rgzPTTestRun=RGZPTRun["box","Project",10];
 rgzPTTestRec["PoisonedDiskCheckpointRecomputed",AssociationQ[rgzPTTestRun]&&rgzPTTestRun["Status"]==="Complete"&&RGZPTGood["Project",rgzPTJob["Parts"]["box"]["Result"]["Project"]]];
 rgzPTTestPart=rgzPTJob["Parts"]["box"];AssociateTo[rgzPTTestPart,"Result"-><|"Project"->h[Failure["Fixture",<||>]]|>];rgzPTTestRejected=RGZPTSetPart["box",rgzPTTestPart];
 rgzPTTestRec["InvalidInMemoryMutationRejected",FailureQ[rgzPTTestRejected]&&AssociationQ[RGZPTRun["box","Project",10]]]];
Print["SUMMARY ",<|"Checks"->Length[rgzPTTestChecks],"Failed"->Keys[Select[rgzPTTestChecks,!TrueQ[#]&]]|>];
Exit[If[AllTrue[Values[rgzPTTestChecks],TrueQ],0,1]];
