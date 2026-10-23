(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"rgz-uvir-test-"<>CreateUUID[]}],"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
eU=FeynCalc`EpsilonUV;eI=FeynCalc`EpsilonIR;gs=FeynCalc`SMP["g_s"];
u=RGZWorkflow`Private`qqUVIRMaster[FeynCalc`B0[st,0,0],False];
record["NativeBubbleUVPole",Cancel[(u["UV"]/.D->4-2eU)-1/eU]===0];record["GenericBubbleIRFinite",u["IR"]===0];
u=RGZWorkflow`Private`qqUVIRMaster[FeynCalc`A0[mm],False];record["TadpoleUVAndIR",Cancel[(u["UV"]/.D->4-2eU)-mm/eU]===0&&u["IR"]===0];
u=RGZWorkflow`Private`qqUVIRMaster[FeynCalc`C0[0,0,st,0,0,I],False];record["ComplexMassNotSilentlyContinued",MatchQ[u["IR"],Missing["ComplexMassIRUnsupported"]]];
record["PoleKeepsDtimesDoubleIR",Expand[RGZWorkflow`Private`qqUVIRPole[(D-2)/eI^2/.D->4-2eI,eI]-(2/eI^2-2/eI)]===0];
record["IRCountertermRejected",FailureQ[RGZWorkflow`Private`qqLocalParameterCT[gs^2,<|"g_s"->1/eI|>]]];
record["KinematicCountertermRejected",FailureQ[RGZWorkflow`Private`qqLocalParameterCT[gs^2,<|"g_s"->RGZWorkflow`Engine`qqT/eU|>]]];
record["ParameterShiftSign",RGZWorkflow`Private`qqLocalParameterCT[gs^2/(RGZWorkflow`Engine`qqT-RGZWorkflow`Engine`m^2),<|"m^2"->dm|>]===dm gs^2/(RGZWorkflow`Engine`qqT-RGZWorkflow`Engine`m^2)^2];
color=First[RGZWorkflow`Private`qqFFColors[]];spin=FeynCalc`FCI[FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK3,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK1,0] FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK4,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK2,0]];
base=color spin;packet=<|"Fingerprint"->"uvir-fixture","AssemblyConvention"->"QQPhysicalLoopMeasure/1","Tree"->I gs^2 base,"OneLoop"->I gs^4 base FeynCalc`B0[st,0,0],"Amplitude"->gs^2 base+gs^4 base FeynCalc`B0[st,0,0],"Gauge"->0,"RegulatorMass"->0|>;
RGZWorkflow`Private`storeResult["QQAmplitude",packet];RGZQQFormFactors[60,"LoopOrder"->1,"CoefficientSeconds"->2];
r=RGZQQUVIR[60,"UsePackageX"->False];Print[r];p=RGZResult["QQUVIR"];
record["PublicRepresentedUV",AssociationQ[r]&&p["CompleteRepresentedUV"]&&Expand[p["Rows"][[1]]["UVPole"]-gs^4/eU]===0];
record["LostScalelessTermsNotCertified",p["UVIRSeparated"]===False&&p["ScalelessLedgerComplete"]===False];
firstKey=p["Key"];r=RGZQQUVIR[60,"UsePackageX"->False,"CoefficientSeconds"->15];
record["PoleCheckpointsReusableAcrossTimeLimits",AssociationQ[r]&&RGZResult["QQUVIR"]["Key"]===firstKey&&Length[FileNames["row-*.wxf",DirectoryName[p["File"]]]]===1];

record["FinitePartNotInvented",MatchQ[p["Rows"][[1]]["FinitePart"],_Missing]];
r=RGZQQRenormalize[60];record["LocalConstantsRequired",MatchQ[r,Failure["QQLocalCountertermsRequired",_]]];
r=RGZQQRenormalize[60,"Mode"->"PoleSubtraction"];Print[r];rp=RGZResult["QQRenormalization"];
record["FormalSubtractionExplicit",AssociationQ[r]&&rp["Status"]==="FormalUVPoleSubtraction"&&rp["RepresentedUVClosed"]&&rp["Renormalized"]===False];
r=RGZQQRenormalize[60,"ParameterCounterterms"-><|"g_s"->-gs^3/(2eU)|>];Print[r];rp=RGZResult["QQRenormalization"];
record["LocalParameterCTClosesKnownUV",AssociationQ[r]&&rp["RepresentedUVClosed"]&&rp["LocalParameterCountertermsUsed"]];
record["IRNotSubtractedByUVCT",rp["Rows"][[1]]["IRPole"]===p["Rows"][[1]]["IRPole"]];
r=RGZQQRenormalize[60,"ParameterCounterterms"-><|"g_s"->gs^3/(2eU)|>];record["WrongCTCannotForceClosure",AssociationQ[r]&&!RGZResult["QQRenormalization"]["RepresentedUVClosed"]];
(* D-4 coefficient poles need a full master expansion, not a UV-only guess. *)
RGZWorkflow`Private`storeResult["QQAmplitude",Join[packet,<|"OneLoop"->packet["OneLoop"]/(D-4)|>]];RGZQQFormFactors[60,"LoopOrder"->1,"CoefficientSeconds"->2];
r=RGZQQUVIR[60,"UsePackageX"->False];record["D4CoefficientSingularityReported",AssociationQ[r]&&!RGZResult["QQUVIR"]["CompleteRepresentedUV"]];
record["IncompleteUVBlocksRenormalization",FailureQ[RGZQQRenormalize[30,"Mode"->"PoleSubtraction"]]];
r=RGZLoadFeynHelpers[];record["InstalledPackageXLoads",AssociationQ[r]];
tri=RGZWorkflow`Private`qqUVIRMaster[FeynCalc`C0[0,0,st,0,0,0]];
record["ActualTriangleDoubleIRPole",Cancel[Coefficient[Expand[tri["IR"]],eI,-2]-1/st]===0];
keepBefore=FeynCalc`$KeepLogDivergentScalelessIntegrals;
r=RGZQQUVIRIntegral[FeynCalc`FCI[FeynCalc`FAD[{testLoop,0,2}]],testLoop,60,"ImplicitPrefactor"->1/(I Pi^2)];Print[r];raw=RGZResult["QQUVIRIntegral"];
record["RawScalelessUVIRRecovered",AssociationQ[r]&&Expand[raw["UVPole"]-1/eU]===0&&Expand[raw["IRPole"]+1/eI]===0];
record["RawSplitReconstructsZeroWhenEpsilonsIdentified",Expand[raw["Expression"]/.eI->eU]===0&&raw["ReconstructionVerified"]];
record["RawSplitDoesNotCertifyAmplitude",raw["UVIRSeparated"]&&raw["Renormalized"]===False&&StringContainsQ[raw["Scope"],"supplied raw integral"]];
record["ScalelessSettingRestored",FeynCalc`$KeepLogDivergentScalelessIntegrals===keepBefore];
record["ReducedZeroCannotPretendRecovery",FailureQ[RGZQQUVIRIntegral[0,testLoop,10]]];
r=RGZQQUVIRIntegral[I FeynCalc`FCI[FeynCalc`FAD[{testLoop,0,2}]],testLoop,60,"ImplicitPrefactor"->1/(I Pi^2)];
record["OverallPhaseIsNotComplexMass",AssociationQ[r]&&Expand[RGZResult["QQUVIRIntegral"]["UVPole"]-I/eU]===0];

Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
