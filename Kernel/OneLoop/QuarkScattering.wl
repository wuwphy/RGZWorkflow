(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Prepare the conventional distinct-flavour massless QQ scattering task.

   Inputs:
   The mixed transverse matrix, actual model vertices, gauge and regulator mass.

   Dependencies:
   MixedMatrices.wl loads shared tools; QuarkGraphs.wl, QuarkSelfEnergy.wl and VertexSum.wl supply the conventional terms. No PT engine.
*)

(* Full conventional amplitudes: no pinch contribution is added twice. *)
(* Stored probe reductions use the package's fixed raw-TID-to-iM conversion. *)
qqPhysicalLoopMeasureQ[measure_] := SameQ[measure,1/(2 Pi)^4];
qqCalculationContext[] := Hash[{"QQIntegralPolicy/1",$Assumptions,
 FeynCalc`$KeepLogDivergentScalelessIntegrals,Options[FeynCalc`TID],
 DownValues[FeynCalc`Pair],DownValues[FeynCalc`ScalarProduct],DownValues[FeynCalc`SPD]},"SHA256"];
qqPartInputIdentity[j_,name_] := Hash[{"QQPartInput/1",j["Fingerprint"],qqCalculationContext[],name,
 j["Gauge"],j["RegulatorMass"],j["LoopMeasureConversion"],j["Integrands"][name],
 If[MatchQ[j["Integrands"][name],Missing["DeferredNAV",_Integer]],j["NAVSource"],None]},"SHA256"];
qqPartProof[j_,name_,value_] := <|"Policy"->"QQPart/1","InputIdentity"->qqPartInputIdentity[j,name],
 "ExpressionHash"->Hash[value,"SHA256"]|>;
qqPartCacheQ[p_,j_,name_] := AssociationQ[p]&&Lookup[p,"Fingerprint",None]===j["Fingerprint"]&&
 Lookup[p,"Part",None]===name&&KeyExistsQ[p,"Expression"]&&!qqFFInvalidQ[p["Expression"]]&&FreeQ[p["Expression"],_FeynCalc`TID|_FeynCalc`FeynAmpDenominator]&&
 Lookup[p,"Provenance",None]===qqPartProof[j,name,p["Expression"]];
qqReducedPartsValidQ[j_] := AssociationQ[Lookup[j,"Reduced",None]]&&
 AssociationQ[Lookup[j,"Integrands",None]]&&AssociationQ[Lookup[j,"ReducedProvenance",<||>]]&&
 AllTrue[Keys[j["Reduced"]],KeyExistsQ[j["Integrands"],#]&&!qqFFInvalidQ[j["Reduced"][#]]&&
 Lookup[Lookup[j,"ReducedProvenance",<||>],#,None]===qqPartProof[j,#,j["Reduced"][#]]&];
qqJobFingerprint[gauge_,reg_]:=If[!FileExistsQ[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}]],Missing["VerticesRequired"],Hash[{"RGZQQ/3",qqCalculationContext[],workflowFingerprint[],FileHash[FileNameJoin[{$packageRoot,"Kernel","OneLoop/QuarkScattering.wl"}],"SHA256"],$Version,modelKey[],navFormulaFingerprint[],FileHash[FileNameJoin[{calculationDirectory[],"OneLoop/QuarkGraphs.wl"}],"SHA256"],gauge,reg},"SHA256"]];
invalidateQQDerived[] := ($results=KeyDrop[$results,
 Select[Keys[$results],StringStartsQ[#,"QQ"]&&!MemberQ[{"QQ","QQBeforeGauge","QQProgress"},#]&]]);
qqSupportedModelQ[c_] := AssociationQ[c]&&Lookup[c,"ClosedQuarkFlavors",None]===2&&
 TrueQ[Lookup[c,"JacobianGhostIncluded",False]]&&Lookup[c,"QuarkClasses",None]==={1,2}&&
 Lookup[c,"GhostClasses",None]==={1,2,3,4}&&Lookup[c,"ScalarGhostClasses",None]==={1,4};
qqJobPacketQ[j_,key_,source_,directory_,content_] := AssociationQ[j]&&
 Lookup[j,"Schema",None]==="RGZQQ/3"&&qqJobFingerprint[Lookup[j,"Gauge",None],Lookup[j,"RegulatorMass",None]]===key&&Lookup[j,"Fingerprint",None]===key&&Lookup[j,"Directory",None]===directory&&
 Lookup[j,"MixedSourceHash",None]===source&&Lookup[j,"ModelFingerprint",None]===modelKey[]&&Lookup[j,"ModelContent",None]===content&&
 TrueQ[Lookup[j,"JacobianGhostIncluded",False]]&&Lookup[j,"ClosedQuarkFlavors",None]===2&&
 And@@(KeyExistsQ[j,#]&/@{"Gauge","RegulatorMass","Tree","OnShellSigmaV","LoopMeasureConversion","MixedTransverse","MixedSelfEnergy"})&&
 MemberQ[{True,False},Lookup[j,"SelfPrepared",None]]&&AssociationQ[Lookup[j,"Integrands",None]]&&AssociationQ[Lookup[j,"Reduced",None]]&&
 And@@(KeyExistsQ[j["Integrands"],#]&/@{"AV1","AV2","Box1","Box2","NAV1","NAV2","QuarkSelfEnergy"})&&
 qqReducedPartsValidQ[j]&&AssociationQ[Lookup[j,"NAVSource",None]]&&
 KeyExistsQ[j["NAVSource"],"TransverseIntegrand"]&&Dimensions[j["MixedTransverse"]]==={5,5}&&Hash[j["MixedTransverse"],"SHA256"]===source&&
 !qqFFInvalidQ[KeyTake[j,{"Gauge","RegulatorMass","Tree","OnShellSigmaV","LoopMeasureConversion","MixedTransverse","NAVSource","Reduced"}]]&&
 qqPhysicalLoopMeasureQ[j["LoopMeasureConversion"]]&&
 !qqFFInvalidQ[j["Integrands"]/.Missing["DeferredNAV",1|2]->0]&&
 If[TrueQ[j["SelfPrepared"]],qqPreparedSelfQ[j],
  j["MixedSelfEnergy"]===Missing["PrepareSelfFirst"]||!qqFFInvalidQ[j["MixedSelfEnergy"]]];
qqSessionJobValidQ[j_] := Module[{key,dir,content},
 If[!AssociationQ[j]||!And@@(KeyExistsQ[j,#]&/@{"Gauge","RegulatorMass","MixedSourceHash"})||
 !FileExistsQ[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}]],Return[False]];
 key=qqJobFingerprint[j["Gauge"],j["RegulatorMass"]];If[!IntegerQ[key],Return[False]];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_qq_"<>IntegerString[key,16]}];content=modelContent[];
 TrueQ[qqSupportedModelQ[content]&&qqJobPacketQ[j,key,j["MixedSourceHash"],dir,content]]];
Options[RGZQQStart]={"Gauge"->0,"RegulatorMass"->0};
RGZQQStart[seconds_:300,OptionsPattern[]]:=engineEvaluation@Module[{r,mixed,nav,dir,key,probe,gauge=OptionValue["Gauge"],reg=OptionValue["RegulatorMass"],parts,packet,sigma,cached,sourceHash,content},
 If[!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];
 content=modelContent[];If[FailureQ[content],Return[content]];
 If[!qqSupportedModelQ[content],Return[Failure["UnsupportedQQModelContent",<|"Explanation"->"Full QQ scattering requires the constraint ghost and exactly two dynamical quark flavours."|>]]];
 r=masslessQQReady[];If[FailureQ[r],Return[r]];
 r=loadQuarkEngine[];If[FailureQ[r],Return[r]];
 If[qqFFInvalidQ[{gauge,reg}]||!FreeQ[{gauge,reg},RGZWorkflow`Engine`q],Return[Failure["QuarkAmplitudeParameters",<||>]]];
 sigma=RGZWorkflow`Engine`rgzQQSigmaV0[gauge,reg];If[qqFFInvalidQ[sigma],Return[Failure["QQResidueIncomplete",<||>]]];
 mixed=RGZResult["Mixed"];
 If[!AssociationQ[mixed]||!KeyExistsQ[mixed,"T"]||Dimensions[mixed["T"]]=!={5,5}||
 Lookup[mixed,"TFieldOrder",None]=!=RGZWorkflow`Engine`RGZMixBasis["T"]||
 qqFFInvalidQ[mixed["T"]],Return[Failure["MixedMatricesRequired",<|"Action"->"RGZMixedResult[] or RGZRestore[]"|>]]];
 If[Lookup[mixed,"ModelFingerprint",None]=!=modelKey[]||Lookup[mixed,"ModelContent",None]=!=modelContent[],Return[Failure["MixedModelContentMismatch",<|"Action"->"Recompute the mixed matrices with the current model, including eta and both quark loops."|>]]];
 sourceHash=Hash[mixed["T"],"SHA256"];
 r=timed["QQNAVSource",RGZNAVSum[seconds,"QuarkMass"->0],seconds];If[FailureQ[r],Return[r]];nav=RGZResult["NAV"];
 key=qqJobFingerprint[gauge,reg];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_qq_"<>IntegerString[key,16]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 If[FileExistsQ[FileNameJoin[{dir,"job.wxf"}]],
 cached=Quiet[Check[Import[FileNameJoin[{dir,"job.wxf"}],"WXF"],$Failed]];
 If[!AssociationQ[cached]||Lookup[cached,"Fingerprint",None]=!=key,Return[Failure["QQJobCacheInvalid",<|"Directory"->dir|>]]];
 If[Lookup[cached,"MixedSourceHash",None]===sourceHash,
 If[!qqJobPacketQ[cached,key,sourceHash,dir,content],Return[Failure["QQJobCacheInvalid",<|"Directory"->dir|>]]];
 invalidateQQDerived[];storeResult["QQ",cached];Return[<|"Status"->"IntegrandsReady","Name"->"FullQQAmplitude","Reused"->True,"SelfPrepared"->TrueQ[Lookup[cached,"SelfPrepared",False]]|>]]];
 probe=timed["FullProbeGraphs",RGZWorkflow`Engine`rgzQQProbeGraphs[dir,seconds],seconds];If[FailureQ[probe],Return[probe]];
 probe=MapAt[RGZWorkflow`Engine`RGZLoopNormalize,probe,{{Key["Tree"]},{Key["AVAndBoxes"]},{Key["QuarkSelfEnergy"]}}];
 probe=RGZWorkflow`Engine`rgzQQNormalizeColours[probe];
 parts=RGZWorkflow`Engine`rgzQQClassify[probe["AVAndBoxes"]];If[FailureQ[parts],Return[parts]];
 parts=Join[parts,<|"NAV1"->Missing["DeferredNAV",1],"NAV2"->Missing["DeferredNAV",2],"QuarkSelfEnergy"->probe["QuarkSelfEnergy"]|>];
 packet=<|"Schema"->"RGZQQ/3","ExternalOrderingSign"->-1,"Fingerprint"->key,"Directory"->dir,"ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],"JacobianGhostIncluded"->True,"ClosedQuarkFlavors"->2,"Process"->"q qprime -> q qprime; two dynamical massless flavors", "Gauge"->gauge,"RegulatorMass"->reg,
 "Kinematics"-><|"k1"->RGZWorkflow`Engine`qqK1,"k2"->RGZWorkflow`Engine`qqK2,"k3"->RGZWorkflow`Engine`qqK3,"k4"->RGZWorkflow`Engine`qqK4,"s"->RGZWorkflow`Engine`qqS,"t"->RGZWorkflow`Engine`qqT|>,
 "Tree"->probe["Tree"],"Integrands"->parts,"MixedTransverse"->mixed["T"],"MixedSourceHash"->sourceHash,
 "NAVSource"-><|"TransverseIntegrand"->nav["TransverseIntegrand"]|>,"MixedSelfEnergy"->Missing["PrepareSelfFirst"],"SelfPrepared"->False,"Reduced"-><||>,"ReducedProvenance"-><||>,"Status"->"IntegrandsReady",
 "LoopMeasureConversion"->1/(2 Pi)^4,"OnShellSigmaV"->sigma,"GaugeIndependenceVerified"->False,"Renormalization"->"MSbar coupling and on-shell quark residues; not yet evaluated"|>;
 packet=MapAt[(#/.{RGZWorkflow`Engine`\[Alpha]->gauge,RGZWorkflow`Engine`M->reg})&,packet,{{Key["Tree"]},{Key["Integrands"]}}];
 If[!qqJobPacketQ[packet,key,sourceHash,dir,content],Return[Failure["QQJobIncomplete",<||>]]];
 r=atomicWXF[FileNameJoin[{dir,"job.wxf"}],packet];If[FailureQ[r],Return[r]];
 invalidateQQDerived[];storeResult["QQ",packet];<|"Status"->"IntegrandsReady","Name"->"FullQQAmplitude","Rows"->7,"File"->FileNameJoin[{dir,"job.wxf"}]|>
];
(* Mixed insertion preparation is in QQMixedSelf.wl; full-amplitude assembly
   is in Amplitude.wl and part evaluation in EvaluateGraphs.wl. *)
