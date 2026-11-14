(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZTreeBenchmark[60];RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`Private`loadLoopEngine[];Scan[RGZWorkflow`Private`engineGet,{"Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","OneLoop/QuarkGraphs.wl","OneLoop/QuarkSelfEnergy.wl","Renormalization/QuarkMassLimit.wl","Renormalization/RegulatorLimit.wl"}];RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[]];
rgzQSChecks=<||>;rgzQSRec[n_,v_]:=(AssociateTo[rgzQSChecks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);RGZMXSetDisplay["Quiet"];FCClearScalarProducts[];
(* Inert raw-file fixture supplies a portable fingerprint input, not model evidence. *)
RGZWorkflow`Private`atomicWXF[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],{}];
rgzQSKey=RGZWorkflow`Private`qqJobFingerprint[0,0];
rgzQSRec["RealTaskFingerprint",IntegerQ[rgzQSKey]];
rgzQSRec["PolicySeparatesScaleless",Block[{$KeepLogDivergentScalelessIntegrals=False},RGZWorkflow`Private`qqJobFingerprint[0,0]]=!=Block[{$KeepLogDivergentScalelessIntegrals=True},RGZWorkflow`Private`qqJobFingerprint[0,0]]];
rgzQSRec["PolicySeparatesAssumptions",Block[{$Assumptions=True},RGZWorkflow`Private`qqJobFingerprint[0,0]]=!=Block[{$Assumptions=m==\[Mu]},RGZWorkflow`Private`qqJobFingerprint[0,0]]];
Do[rgzQSRec["InvalidM0"<>ToString[bad,InputForm],FailureQ[rgzQQM0Term[{A0[-I Sqrt[\[Alpha] M^4]],bad},10]]],
 {bad,{TerminatedEvaluation["IterationLimit"],Indeterminate,ComplexInfinity,$TimedOut,h[$Aborted]}}];
rgzQSRec["ValidDiscardedTadpole",rgzQQM0Term[{A0[-I Sqrt[\[Alpha] M^4]],1},10]==={0,0,0}];
rgzQSRec["AssignedQQLabelsRejected",Block[{qqPVm3=\[Mu],qqPVm4=\[Mu]},FailureQ[rgzQQPoleFactor[RGZLoopDelta[rgzQSV],rgzQSV,q,0,0]]]];
Internal`InheritedBlock[{rgzQQRestorePoleMasses},Clear[rgzQQRestorePoleMasses];rgzQQRestorePoleMasses[x_,_,_]:=x;
 rgzQSRec["CorruptEmissionRejected",MatchQ[rgzQQPoleFactor[RGZLoopDelta[rgzQSV],rgzQSV,q,0,0],Failure["QuarkEmittedPoleReconstruction",_]]]];
Do[rgzQSPole=rgzQQPoleFactor[(rgzQSV-\[Mu]^2)^n,rgzQSV,route,0,0];
 rgzQSRec["RepeatedShiftedPole"<>ToString[{route,n},InputForm],!FailureQ[rgzQSPole]&&Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[rgzQSPole]]-ExpandScalarProduct[FCI[1/(SPD[route,route]-\[Mu]^2)^n]]]]===0],{route,{q,q+qqK1,q-qqK3}},{n,{1,2,3}}];
rgzQSDir=CreateDirectory[];
rgzQSJob=<|"Fingerprint"->rgzQSKey,"Directory"->rgzQSDir,"Gauge"->0,"RegulatorMass"->0,"LoopMeasureConversion"->1/(2 Pi)^4,
 "Integrands"-><|"AV1"->FCI[FAD[{q,\[Mu]}]]|>,"NAVSource"-><|"TransverseIntegrand"->1|>,"Reduced"-><||>,"ReducedProvenance"-><||>|>;
RGZWorkflow`Private`storeResult["QQ",rgzQSJob];
rgzQSRun=RGZWorkflow`RGZQQRunPart["AV1",30];rgzQSStored=RGZWorkflow`RGZResult["QQ"];
rgzQSRec["NativePartCompletes",AssociationQ[rgzQSRun]&&RGZWorkflow`Private`qqReducedPartsValidQ[rgzQSStored]];
rgzQSRec["NativePhysicalMeasure",rgzQSStored["Reduced"]["AV1"]===I A0[\[Mu]^2]/(16 Pi^2)];
Internal`InheritedBlock[{RGZWorkflow`Private`qqReduceTensorGroups},Clear[RGZWorkflow`Private`qqReduceTensorGroups];RGZWorkflow`Private`qqReduceTensorGroups[___]:=Failure["UnexpectedRecompute",<||>];
 rgzQSRec["ValidPartCacheReused",AssociationQ[RGZWorkflow`RGZQQRunPart["AV1",30]]]];
rgzQSChanged=Join[RGZWorkflow`RGZResult["QQ"],<|"Integrands"-><|"AV1"->2 FCI[FAD[{q,\[Mu]}]]|>|>];
rgzQSRec["ChangedInputProofRejected",!RGZWorkflow`Private`qqReducedPartsValidQ[rgzQSChanged]];
RGZWorkflow`Private`storeResult["QQ",rgzQSChanged];rgzQSRun=RGZWorkflow`RGZQQRunPart["AV1",30];
rgzQSRec["ChangedInputRecomputed",AssociationQ[rgzQSRun]&&RGZWorkflow`RGZResult["QQ"]["Reduced"]["AV1"]===I A0[\[Mu]^2]/(8 Pi^2)];
rgzQSFile=FileNameJoin[{rgzQSDir,"AV1-reduced.wxf"}];rgzQSCached=Import[rgzQSFile,"WXF"];
RGZWorkflow`Private`atomicWXF[rgzQSFile,Join[rgzQSCached,<|"Expression"->999|>]];
rgzQSRec["CorruptResultRecomputed",AssociationQ[RGZWorkflow`RGZQQRunPart["AV1",30]]&&RGZWorkflow`RGZResult["QQ"]["Reduced"]["AV1"]=!=999];
RGZWorkflow`Private`atomicWXF[rgzQSFile,KeyDrop[rgzQSCached,{"Provenance"}]];
rgzQSRec["LegacyPartRecomputed",AssociationQ[RGZWorkflow`RGZQQRunPart["AV1",30]]&&KeyExistsQ[Import[rgzQSFile,"WXF"],"Provenance"]];
rgzQSRec["TaskSwitchRejected",Block[{$Assumptions=m==\[Mu]},MatchQ[RGZWorkflow`RGZQQRunPart["AV1",10],Failure["QQRestartRequired",_]]]];
rgzQSBad=Join[RGZWorkflow`RGZResult["QQ"],<|"ReducedProvenance"-><||>,"Tree"->1,"OnShellSigmaV"->0,"MixedSelfEnergy"->0,"SelfPrepared"->True|>];
RGZWorkflow`Private`storeResult["QQ",rgzQSBad];rgzQSRec["AssemblyRejectsUnboundParts",FailureQ[RGZWorkflow`RGZQQAmplitude[]]];
(* Independent physical residue numerator, not copied from rgzQQSigmaV0. *)
rgzQSNumerator=GAD[rgzQSMu].(rgzQSEta GSD[rgzQSR]-GSD[q]).GAD[rgzQSNu];
rgzQSDen=SPD[q,q]-2 rgzQSEta SPD[rgzQSR,q]+rgzQSEta^2 SPD[rgzQSR,rgzQSR];
Do[rgzQSTensor=If[kind==="T",RGZLoopPT[q,rgzQSMu,rgzQSNu],FVD[q,rgzQSMu] FVD[q,rgzQSNu]/SPD[q,q]];
 rgzQSTrace=Contract[DiracTrace[FCI[GSD[rgzQSR].rgzQSNumerator],DiracTraceEvaluate->True] FCI[rgzQSTensor]]/(4 FCI[SPD[rgzQSR,rgzQSR]]);
 rgzQSLinear=Coefficient[Normal[Series[rgzQSTrace/FCI[rgzQSDen],{rgzQSEta,0,1}]],rgzQSEta,1];
 rgzQSCoeff=Cancel[Together[(rgzQSLinear FCI[SPD[q,q]])/.Pair[Momentum[rgzQSR,D],Momentum[q,D]]^2->Pair[Momentum[rgzQSR,D],Momentum[rgzQSR,D]] Pair[Momentum[q,D],Momentum[q,D]]/D]];
 rgzQSRec["PhysicalResidue"<>kind,Cancel[Together[rgzQSCoeff-If[kind==="T",-(D-1)(D-4)/D,-1]]]===0],{kind,{"T","L"}}];
rgzQSRec["FourExternalLegWeight",Coefficient[Normal[Series[(1+rgzQSZ/2)^4,{rgzQSZ,0,1}]],rgzQSZ,1]===2];
rgzQSRec["TaskScalelessSwitchRejected",Block[{$KeepLogDivergentScalelessIntegrals=!TrueQ[$KeepLogDivergentScalelessIntegrals]},MatchQ[RGZWorkflow`RGZQQRunPart["AV1",10],Failure["QQRestartRequired",_]]]];
rgzQSRec["NativeScalelessRetention",Block[{$KeepLogDivergentScalelessIntegrals=True},rgzQQReduce[FCI[FAD[q,q]]]===I Pi^2 B0[0,0,0]]];
rgzQSPole=Block[{$Assumptions=m==\[Mu]},rgzQQPoleFactor[RGZLoopDelta[rgzQSV],rgzQSV,q,0,0]];
rgzQSRec["PhysicalGeneralMassPoint",!FailureQ[rgzQSPole]&&FullSimplify[FeynAmpDenominatorExplicit[rgzQQRestorePoleMasses[rgzQSPole,0,0]]/.{m->2,\[Mu]->1,mG->2,Pair[Momentum[q,D],Momentum[q,D]]->7}]===1/34];
rgzQSRec["CurrentConservation",DiracSimplify[FCI[SpinorUBarD[qqK3,0].GSD[qqK3-qqK1].SpinorUD[qqK1,0]]]===0];
rgzQSRight=SpinorUBarD[qqK3,0].GAD[rgzQSMu].GSD[qqK1-q].GSD[q].SpinorUD[qqK1,0]+SPD[qqK1-q,qqK1-q] SpinorUBarD[qqK3,0].GAD[rgzQSMu].SpinorUD[qqK1,0];
rgzQSRec["RightWardIdentity",Expand[ExpandScalarProduct[DiracSimplify[FCI[rgzQSRight]]]/.FCI[rgzQQInvariantRules]]===0];
rgzQSLeft=SpinorUBarD[qqK3,0].GSD[qqK3-q].GSD[q].GAD[rgzQSMu].SpinorUD[qqK1,0]+SPD[q,q] SpinorUBarD[qqK3,0].GAD[rgzQSMu].SpinorUD[qqK1,0];
rgzQSRec["LeftWardIdentity",Expand[ExpandScalarProduct[DiracSimplify[FCI[rgzQSLeft]]]/.FCI[rgzQQInvariantRules]]===0];
rgzQSRec["DimensionalResidueFiniteTerm",Limit[(-(D-1)(D-4)/D)/((4-D)/2),D->4]===3/2];
(* The assembly equation must be readable and unchanged by the refactor. *)
rgzQSNames={"AV1","AV2","Box1","Box2","NAV1","NAV2"};
rgzQSAssemblyJob=<|"Fingerprint"->RGZWorkflow`Private`qqJobFingerprint[0,0],"Directory"->CreateDirectory[],
 "Gauge"->0,"RegulatorMass"->0,"LoopMeasureConversion"->1/(2 Pi)^4,"Process"->"Synthetic assembly identity",
 "Tree"->rgzQSTree,"OnShellSigmaV"->rgzQSSigma,"MixedSelfEnergy"->rgzQSRawSelf,"SelfPrepared"->True,
 "Integrands"->AssociationThread[rgzQSNames,Range[6]],"Reduced"->AssociationThread[rgzQSNames,{rgzQSAV1,rgzQSAV2,rgzQSBox1,rgzQSBox2,rgzQSNAV1,rgzQSNAV2}]|>;
AssociateTo[rgzQSAssemblyJob,"ReducedProvenance"->Association@Table[n->RGZWorkflow`Private`qqPartProof[rgzQSAssemblyJob,n,rgzQSAssemblyJob["Reduced"][n]],{n,rgzQSNames}]];
RGZWorkflow`Private`storeResult["QQ",rgzQSAssemblyJob];rgzQSAssembled=RGZWorkflow`RGZQQAmplitude[];
rgzQSA=RGZWorkflow`RGZResult["QQAmplitude"];
rgzQSRec["AssemblyEquation",AssociationQ[rgzQSAssembled]&&Expand[rgzQSA["OneLoop"]-rgzQSRawSelf/(2 Pi)^4-rgzQSAV1-rgzQSAV2-rgzQSBox1-rgzQSBox2-rgzQSNAV1-rgzQSNAV2-2rgzQSSigma rgzQSTree]===0];
rgzQSRec["AssemblyPreservesRawSelf",RGZWorkflow`RGZResult["QQ"]["MixedSelfEnergy"]===rgzQSRawSelf];
rgzQSRec["AssemblyMeasureMetadata",rgzQSA["AssemblyConvention"]==="QQPhysicalLoopMeasure/1"&&rgzQSA["MixedSelfEnergyLoopMeasure"]===1/(2 Pi)^4];
Print["SUMMARY ",<|"Checks"->Length[rgzQSChecks],"Failed"->Keys[Select[rgzQSChecks,!TrueQ[#]&]]|>];Exit[If[And@@Values[rgzQSChecks],0,1]];
