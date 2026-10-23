(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
dir=FileNameJoin[{$TemporaryDirectory,"rgz-measure-pv-"<>CreateUUID[]}];
RGZInitialize[dir,"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`loadPTEngine[];
RGZWorkflow`Private`engineGet["RGZ_Quark_Amplitude.wl",False];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
q=RGZWorkflow`Engine`q;p=RGZWorkflow`Engine`p;
raw=RGZWorkflow`Engine`rgzQQReduce[FeynCalc`FCI[FeynCalc`FAD[q,{q-p,2}]]];
expected=I/(16 Pi^2) FeynCalc`FCI[FeynCalc`B0[FeynCalc`SPD[p,p],0,4]];
Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],{},"WXF"];
job=<|"Schema"->"RGZQQ/3","Directory"->dir,"Fingerprint"->RGZWorkflow`Private`qqJobFingerprint[0,0],
 "Gauge"->0,"RegulatorMass"->0,"Process"->"native measure unit test","SelfPrepared"->True,
 "Tree"->0,"OnShellSigmaV"->0,"MixedSelfEnergy"->raw,"LoopMeasureConversion"->1/(2 Pi)^4,
 "Reduced"->AssociationThread[{"AV1","AV2","Box1","Box2","NAV1","NAV2"},ConstantArray[0,6]]|>;
sourceFile=FileNameJoin[{dir,"raw-self.wxf"}];Export[sourceFile,raw,"WXF"];sourceHash=FileHash[sourceFile,"SHA256"];
RGZWorkflow`Private`storeResult["QQ",job];
r=RGZQQAmplitude[];amp=RGZResult["QQAmplitude"];
record["NativeBubbleHasPhysicalLoopNormalization",AssociationQ[r]&&Expand[amp["OneLoop"]-expected]===0];
record["RawSelfJobNotRescaled",RGZResult["QQ"]["MixedSelfEnergy"]===raw];
record["RawSelfCheckpointUnchanged",FileHash[sourceFile,"SHA256"]===sourceHash];
saved=Import[FileNameJoin[{dir,"bare-amplitude.wxf"}],"WXF"];
record["DiskPacketRecordsAssemblyConvention",saved["AssemblyConvention"]===RGZWorkflow`Private`qqAssemblyConvention&&saved["OneLoop"]===amp["OneLoop"]];
RGZQQAmplitude[];
record["RepeatedAssemblyDoesNotApplyMeasureTwice",RGZResult["QQAmplitude"]["OneLoop"]===amp["OneLoop"]];
record["BareRemainsUnrenormalized",amp["Renormalized"]===False&&amp["GaugeIndependenceVerified"]===False];
RGZWorkflow`Private`storeResult["QQ",Join[job,<|"Tree"->11,"OnShellSigmaV"->2,
 "Reduced"->AssociationThread[{"AV1","AV2","Box1","Box2","NAV1","NAV2"},Range[6]]|>]];
RGZQQAmplitude[];
record["VertexBoxAndResiduesNotRescaled",Expand[RGZResult["QQAmplitude"]["OneLoop"]-expected-21-44]===0];
RGZWorkflow`Private`storeResult["QQ",KeyDrop[job,"LoopMeasureConversion"]];
record["PhysicalJobWithoutMeasureRejected",FailureQ[RGZQQAmplitude[]]];
Internal`InheritedBlock[{RGZWorkflow`Private`qqGaugeValidJob,RGZWorkflow`Private`qqMeasurePinchOriginal},
 RGZWorkflow`Private`qqGaugeValidJob[]:=job;RGZWorkflow`Private`qqMeasurePinchOriginal[_]:=raw;
 record["PinchAttachmentUsesSameMeasure",Expand[RGZWorkflow`Private`qqGaugePinchAmplitude[10]-expected]===0]];
Internal`InheritedBlock[{RGZWorkflow`Private`qqMeasureGaugePacketOriginal},
 RGZWorkflow`Private`qqMeasureGaugePacketOriginal[]:=<|"OneLoop"->17|>;
 record["OldUnnormalizedGaugePacketRejected",FailureQ[RGZWorkflow`Private`qqGaugePacket[]]]];

pv[m_]:=RGZWorkflow`Private`qqPVMasslessSingleScale[m];
t=testT;b=FeynCalc`B0[t,0,0];
record["MasslessB1Identity",Cancel[Together[pv[FeynCalc`PaVe[1,{t},{0,0},FeynCalc`PaVeAutoReduce->False]]+b/2]]===0];
record["MasslessB00Identity",Cancel[Together[pv[FeynCalc`PaVe[0,0,{t},{0,0},FeynCalc`PaVeAutoReduce->False]]+t b/(4(D-1))]]===0];
record["ScalarZeroIndexIsNotMetricIndex",pv[FeynCalc`PaVe[0,{t},{0,0},FeynCalc`PaVeAutoReduce->False]]===b];
dd=FeynCalc`PaVe[0,0,{0,0,0,0,0,t},{0,0,0,0},FeynCalc`PaVeAutoReduce->False];
cc=FeynCalc`C0[0,0,t,0,0,0];
record["D00MatchesIndependentNativeReduction",Cancel[Together[pv[dd]-pv[cc]/(2(D-3))]]===0];
native=FeynCalc`PaVeReduce[dd];native=native/.m_FeynCalc`PaVe:>With[{z=pv[m]},If[z===$Failed,m,z]];
record["NativeD00ConversionAgrees",Cancel[Together[native-pv[dd]]]===0];
record["DAndIRPolesNotSpecialized",!FreeQ[pv[dd],D]&&!FreeQ[pv[dd],Power[x_,_Integer?Negative]/;!FreeQ[x,D]]];
record["MassiveIntegralLeftUnchanged",pv[FeynCalc`PaVe[1,{t},{0,1},FeynCalc`PaVeAutoReduce->False]]===$Failed];
record["TwoScaleIntegralLeftUnchanged",pv[FeynCalc`PaVe[1,{0,t,testU},{0,0,0},FeynCalc`PaVeAutoReduce->False]]===$Failed];
record["NonBipartiteGeometryLeftUnchanged",pv[FeynCalc`PaVe[1,{t,t,t},{0,0,0},FeynCalc`PaVeAutoReduce->False]]===$Failed];
record["OddMetricIndexRejected",pv[FeynCalc`PaVe[0,1,{t},{0,0},FeynCalc`PaVeAutoReduce->False]]===$Failed];
record["GenericPVReducerDefaultIntegratesKnownIdentity",Cancel[Together[
 RGZWorkflow`Private`qqFFPVNormalize[dd,False,3]-(FeynCalc`PaVeOrder[pv[dd],FeynCalc`Collecting->False]/.
  p_FeynCalc`PaVe:>RGZWorkflow`Private`qqPVCanonical[p])]]===0];
color=First[RGZWorkflow`Private`qqFFColors[]];
spin=FeynCalc`FCI[FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK3,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK1,0]
 FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK4,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK2,0]];
alpha=RGZWorkflow`Private`qqGaugeAlpha;
comparison=RGZWorkflow`Private`qqGaugeCompare[alpha color spin (dd-cc/(2(D-3))),1,60,5,"ExactPVRelationRegression"];
record["GaugeComparisonCombinesDependentPVEntries",AssociationQ[comparison]&&comparison["AllCoefficientsZero"]];
unknown=RGZWorkflow`Private`qqGaugeCompare[alpha color spin FeynCalc`PaVe[1,{0,t,testU},{0,0,0},FeynCalc`PaVeAutoReduce->False],1,60,5,"UnsupportedPVRelationRegression"];
record["UnsupportedIntegralCannotBecomeFalsePass",AssociationQ[unknown]&&!unknown["AllCoefficientsZero"]];
RGZWorkflow`Private`storeResult["QQ",job];RGZQQAmplitude[];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZQQAmplitude[];
record["ReloadDoesNotStackMeasureCorrections",Expand[RGZResult["QQAmplitude"]["OneLoop"]-expected]===0&&RGZResult["QQ"]["MixedSelfEnergy"]===raw];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Exit[If[And@@Values[checks],0,1]];
