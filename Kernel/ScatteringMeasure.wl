(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Assembly correction only. RGZQQ/3 mixed-self checkpoints retain RAW TID
   normalization. Never rescale those source files or the stored QQ job.
   This leaves all graph and self preparation fingerprints reusable. *)
qqAssemblyConvention="QQPhysicalLoopMeasure/1";
qqDeclaredLoopMeasure[j_Association] := Lookup[j,"LoopMeasureConversion",
 If[Lookup[j,"Schema",None]==="RGZQQ/3",Missing["LoopMeasureRequired"],1]];
DownValues[qqMeasureAssembleOriginal]=DownValues[RGZQQAmplitude]/.RGZQQAmplitude->qqMeasureAssembleOriginal;
RGZQQAmplitude[] := Module[{job=RGZResult["QQ"],measure,scaled,r,packet,metadata,file,writer},
 If[!AssociationQ[job]||!KeyExistsQ[job,"MixedSelfEnergy"],Return[qqMeasureAssembleOriginal[]]];
 measure=qqDeclaredLoopMeasure[job];
 If[qqFFInvalidQ[measure]||TrueQ[measure===0],Return[Failure["QQMeasureConventionRequired",<||>]]];
 scaled=Join[job,<|"MixedSelfEnergy"->measure job["MixedSelfEnergy"]|>];
 metadata=<|"AssemblyConvention"->qqAssemblyConvention,"MixedSelfEnergyLoopMeasure"->measure,
  "MixedSelfEnergySourceConvention"->"RawTID; source checkpoints unchanged"|>;
 file=FileNameJoin[{job["Directory"],"bare-amplitude.wxf"}];
 DownValues[writer]=DownValues[atomicWXF]/.atomicWXF->writer;
 (* Decorate the single atomic write rather than serialize the amplitude twice. *)
 r=Block[{$results=$results},
  storeResult["QQ",scaled];
  Internal`InheritedBlock[{atomicWXF},
   With[{target=file,meta=metadata},atomicWXF[target,p_Association]:=writer[target,Join[p,meta]]];
   r=qqMeasureAssembleOriginal[];
   If[!FailureQ[r],packet=Join[RGZResult["QQAmplitude"],metadata]];r]];
 If[FailureQ[r],Return[r]];
 storeResult["QQAmplitude",packet];
 $results=KeyDrop[$results,{"QQGaugeAmplitude","QQGaugeDirect","QQWardCoverage","QQFormFactors"}];
 Join[r,KeyTake[metadata,{"AssemblyConvention"}]]];

(* Old assembled packets must not bypass the corrected normalization merely
   because their raw graph fingerprint is still valid. *)
DownValues[qqMeasureGaugePacketOriginal]=DownValues[qqGaugePacket]/.qqGaugePacket->qqMeasureGaugePacketOriginal;
qqGaugePacket[] := Module[{p=qqMeasureGaugePacketOriginal[]},
 If[FailureQ[p],Return[p]];
 If[Lookup[p,"AssemblyConvention",None]=!=qqAssemblyConvention,
  Return[Failure["QQGaugeAssembleFirst",<|"Action"->"RGZQQGaugeAssemble[]",
   "Explanation"->"Reassemble with the physical loop measure; raw checkpoints remain valid."|>]]];p];

(* The legacy pinch attachment is raw as well. Its original cache remains
   raw; convert exactly once at the QQ comparison boundary. *)
DownValues[qqMeasurePinchOriginal]=DownValues[qqGaugePinchAmplitude]/.qqGaugePinchAmplitude->qqMeasurePinchOriginal;
qqGaugePinchAmplitude[seconds_] := Module[{j=qqGaugeValidJob[],p,measure},
 If[FailureQ[j],Return[j]];measure=qqDeclaredLoopMeasure[j];
 If[qqFFInvalidQ[measure],Return[Failure["QQMeasureConventionRequired",<||>]]];
 p=qqMeasurePinchOriginal[seconds];If[FailureQ[p],p,measure p]];
