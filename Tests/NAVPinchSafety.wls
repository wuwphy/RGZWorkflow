(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZTreeBenchmark[60];RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`Private`loadLoopEngine[];Scan[RGZWorkflow`Private`engineGet,{"Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","Pinch/VertexWard.wl"}];RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[]];
rgzNTestChecks=<||>;rgzNTestRec[name_,v_]:=(AssociateTo[rgzNTestChecks,name->TrueQ[v]];Print[name," ",TrueQ[v]]);
RGZMXSetDisplay["Quiet"];
Do[rgzNTestRec["WardReject"<>ToString[bad,InputForm],FailureQ[rgzNAVWardSplit[bad]]];
 rgzNTestRec["BuildReject"<>ToString[bad,InputForm],FailureQ[rgzNAVPinchBuild[<|"ConnectionCount"->1,"Connections"->{<|"Fields"->{"A","A","A"},"TransverseTensor"->bad|>}|>,CreateDirectory[],10]]],
 {bad,{TerminatedEvaluation["IterationLimit"],Indeterminate,ComplexInfinity,$TimedOut,h[$Aborted]}}];
Do[rgzNTestRec["UnknownMomentum"<>ToString[v,InputForm],FailureQ[rgzNAVWardSplit[FVD[v,rgzNAVRho]]]],
 {v,{rgzNTestK,q+rgzNTestK,p q,p^2,1}}];
Do[rgzNTestSplit=rgzNAVWardSplit[FVD[v,rgzNAVRho] FVD[p,rgzNAVNu]];
 rgzNTestRec["LinearMomentum"<>ToString[v,InputForm],AssociationQ[rgzNTestSplit]&&TrueQ[rgzNTestSplit["OriginalTensorReconstruction"]]&&TrueQ[rgzNTestSplit["ExactPolynomialReconstruction"]]],
 {v,{p,q,p+q,q-p,-p-q,2p-3q}}];
rgzNTestSplit=rgzNAVWardSplit[FVD[q,rgzNAVRho] FVD[p+q,rgzNAVSigma] FVD[p,rgzNAVNu]];
rgzNTestRec["DoubleCountedOnce",AssociationQ[rgzNTestSplit]&&rgzNTestSplit["LeftCoefficient"]===0&&rgzNTestSplit["RightCoefficient"]===FVD[p+q,rgzNAVSigma] FVD[p,rgzNAVNu]];
rgzNTestRec["ZeroValid",AssociationQ[rgzNAVWardSplit[0]]];
rgzNTestRec["InternalTensorSupported",AssociationQ[rgzNAVWardSplit[FCI[FVD[p+q,rgzNAVRho]]]]];
rgzNTestRec["InternalUnknownRejected",FailureQ[rgzNAVWardSplit[FCI[FVD[q+rgzNTestK,rgzNAVRho]]]]];
rgzNTestRec["AssignedCoordinateRejected",Block[{rgzNAVRQ=1},FailureQ[rgzNAVWardSplit[FVD[q,rgzNAVRho]]]]];
rgzNTestRec["CoordinateCollisionRejected",FailureQ[rgzNAVWardSplit[rgzNAVRQ FVD[q,rgzNAVRho]]]];
rgzNTestNav=<|"ConnectionCount"->1,"Connections"->{<|"Fields"->{"A","A","A"},"TransverseTensor"->FVD[q,rgzNAVRho] MTD[rgzNAVSigma,rgzNAVNu]|>}|>;
rgzNTestDir=CreateDirectory[];rgzNTestFile=FileNameJoin[{rgzNTestDir,"ward-1.wxf"}];
rgzNTestPacket=rgzNAVPinchBuild[rgzNTestNav,rgzNTestDir,10];
rgzNTestRec["BuildValid",RGZWorkflow`Private`navPinchMatchesQ[rgzNTestPacket,rgzNTestNav]];
(* A successful cache hit must bypass the injected failing splitter. *)
Internal`InheritedBlock[{rgzNAVWardSplit},Clear[rgzNAVWardSplit];rgzNAVWardSplit[___]:=Failure["UnexpectedRecompute",<||>];
 rgzNTestRec["ValidCacheReused",AssociationQ[rgzNAVPinchBuild[rgzNTestNav,rgzNTestDir,10]]]];
rgzNTestOld=Import[rgzNTestFile,"WXF"];
rgzNTestChanged=ReplacePart[rgzNTestNav,{"Connections",1,"TransverseTensor"}->2 FVD[q,rgzNAVRho] MTD[rgzNAVSigma,rgzNAVNu]];
rgzNTestRec["ChangedInputRejected",!RGZWorkflow`Private`navPinchMatchesQ[rgzNTestPacket,rgzNTestChanged]];
rgzNTestNew=rgzNAVPinchBuild[rgzNTestChanged,rgzNTestDir,10];
rgzNTestRec["ChangedInputRecomputed",RGZWorkflow`Private`navPinchMatchesQ[rgzNTestNew,rgzNTestChanged]&&Import[rgzNTestFile,"WXF"]["InputIdentity"]=!=rgzNTestOld["InputIdentity"]];
rgzNTestRec["NonzeroProjectionPreserved",rgzNTestPacket["Kernel"]=!=0 && Cancel[Together[rgzNTestNew["Kernel"]-2 rgzNTestPacket["Kernel"]]]===0];
RGZWorkflow`Private`atomicWXF[rgzNTestFile,Join[rgzNTestOld,<|"Kernel"->123|>]];
rgzNTestNew=rgzNAVPinchBuild[rgzNTestNav,rgzNTestDir,10];
rgzNTestRec["CorruptKernelRecomputed",RGZWorkflow`Private`navPinchMatchesQ[rgzNTestNew,rgzNTestNav]&&rgzNTestNew["Kernel"]=!=123];
RGZWorkflow`Private`atomicWXF[rgzNTestFile,KeyDrop[rgzNTestOld,{"ValidationPolicy","InputIdentity","KernelHash","OriginalTensorReconstruction"}]];
rgzNTestRec["LegacyRecomputed",RGZWorkflow`Private`navPinchMatchesQ[rgzNAVPinchBuild[rgzNTestNav,rgzNTestDir,10],rgzNTestNav]];
Internal`InheritedBlock[{rgzNAVWardSplit},Clear[rgzNAVWardSplit];rgzNAVWardSplit[___]:=<|"PinchTensor"->TerminatedEvaluation["IterationLimit"],"OriginalTensorReconstruction"->True,"ExactPolynomialReconstruction"->True|>;
 rgzNTestRec["InvalidSplitRejected",FailureQ[rgzNAVPinchBuild[rgzNTestNav,CreateDirectory[],10]]]];
Internal`InheritedBlock[{Contract},Unprotect[Contract];Clear[Contract];Contract[___]:=TerminatedEvaluation["IterationLimit"];
 rgzNTestTmp=CreateDirectory[];
 rgzNTestRec["InvalidProjectionNotSaved",FailureQ[rgzNAVPinchBuild[rgzNTestNav,rgzNTestTmp,10]]&& !FileExistsQ[FileNameJoin[{rgzNTestTmp,"ward-1.wxf"}]]]];
rgzNTestRec["EmptyInputRejected",FailureQ[rgzNAVPinchBuild[<|"ConnectionCount"->0,"Connections"->{}|>,CreateDirectory[],10]]];
(* Follow-up: colour extraction, field validation and actual pole outputs. *)
rgzNTestTensor=FVD[q,rgzNAVRho] MTD[rgzNAVSigma,rgzNAVNu];
rgzNTestRec["SingleColourFactor",rgzNAVWardSplit[SUNF[rgzNAVA,rgzNAVB,rgzNAVC] rgzNTestTensor]["PinchTensor"]===rgzNAVWardSplit[rgzNTestTensor]["PinchTensor"]];
Do[rgzNTestRec["RejectColourStructure"<>ToString[c,InputForm],FailureQ[rgzNAVWardSplit[c rgzNTestTensor]]],
 {c,{SUNF[rgzNAVA,rgzNAVB,rgzNAVC]^2,1/SUNF[rgzNAVA,rgzNAVB,rgzNAVC],1+SUNF[rgzNAVA,rgzNAVB,rgzNAVC],SUNDelta[rgzNAVA,rgzNAVB]}}];
rgzNTestRec["UnknownFieldsRejected",FailureQ[rgzNAVPinchBuild[<|"ConnectionCount"->1,"Connections"->{<|"Fields"->{"Typo","A","A"},"TransverseTensor"->0|>}|>,CreateDirectory[],10]]];
rgzNTestRec["AssignedPoleLabelsRejected",Block[{rgzLoopM1=\[Mu],rgzLoopM2=\[Mu]},FailureQ[rgzNAVExactPoleFactor[rgzNTestV^2+\[Alpha] M^4,rgzNTestV,q]]]];
Internal`InheritedBlock[{RGZLoopPoleMasses},Clear[RGZLoopPoleMasses];RGZLoopPoleMasses[x_]:=x;
 rgzNTestRec["WrongEmittedPoleLabelsRejected",MatchQ[rgzNAVExactPoleFactor[RGZLoopDelta[rgzNTestV],rgzNTestV,q],Failure["NAVEmittedPoleReconstruction",_]]]];
Do[rgzNTestPole=rgzNAVExactPoleFactor[(rgzNTestV-\[Mu]^2)^rgzNTestPower,rgzNTestV,rgzNTestRoute];
 rgzNTestBack=ExpandScalarProduct[FeynAmpDenominatorExplicit[rgzNTestPole]];
 rgzNTestExpected=ExpandScalarProduct[FCI[1/(SPD[rgzNTestRoute,rgzNTestRoute]-\[Mu]^2)^rgzNTestPower]];
 rgzNTestRec["ShiftedRepeatedPole"<>ToString[{rgzNTestRoute,rgzNTestPower},InputForm],!FailureQ[rgzNTestPole]&&Cancel[Together[rgzNTestBack-rgzNTestExpected]]===0],
 {rgzNTestRoute,{q,q+p,q-p}},{rgzNTestPower,{1,2,3}}];
Do[rgzNTestRec["ZeroMuPole"<>ToString[n],Block[{\[Mu]=0},rgzNTestPole=rgzNAVExactPoleFactor[rgzNTestV^n,rgzNTestV,q];
 !FailureQ[rgzNTestPole]&&Cancel[Together[FeynAmpDenominatorExplicit[RGZLoopPoleMasses[rgzNTestPole]]-FCI[1/SPD[q,q]^n]]]===0]];
 rgzNTestRec["ZeroAlphaPole"<>ToString[n],Block[{\[Alpha]=0},rgzNTestPole=rgzNAVExactPoleFactor[rgzNTestV^n,rgzNTestV,q];
 !FailureQ[rgzNTestPole]&&Cancel[Together[FeynAmpDenominatorExplicit[RGZLoopPoleMasses[rgzNTestPole]]-FCI[1/SPD[q,q]^n]]]===0]],{n,{1,2,3}}];
rgzNTestRec["CoincidentMassivePoles",Block[{m=2,\[Mu]=2,mG=0},rgzNTestPole=rgzNAVExactPoleFactor[(rgzNTestV-4)^2,rgzNTestV,q];
 !FailureQ[rgzNTestPole]&&Cancel[Together[FeynAmpDenominatorExplicit[RGZLoopPoleMasses[rgzNTestPole]]-FCI[1/(SPD[q,q]-4)^2]]]===0]];
rgzNTestPole=Block[{$Assumptions=m==\[Mu]},rgzNAVExactPoleFactor[RGZLoopDelta[rgzNTestV],rgzNTestV,q]];
rgzNTestRec["GeneralPoleNeutralAssumptions",!FailureQ[rgzNTestPole]&&FullSimplify[FeynAmpDenominatorExplicit[RGZLoopPoleMasses[rgzNTestPole]]/.{m->2,\[Mu]->1,mG->2,Pair[Momentum[q,D],Momentum[q,D]]->7}]===1/34];
Print["SUMMARY ",<|"Checks"->Length[rgzNTestChecks],"Failed"->Keys[Select[rgzNTestChecks,!TrueQ[#]&]]|>];
Exit[If[And@@Values[rgzNTestChecks],0,1]];
