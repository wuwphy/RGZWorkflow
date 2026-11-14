(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Assemble the completed conventional contributions into the bare QQ amplitude.

   Inputs:
   A prepared QQ task, tree graph, mixed insertion, six reduced parts and SigmaV(0).

   Dependencies:
   QuarkScattering.wl and EvaluateGraphs.wl provide task data; QuarkSelfEnergy.wl provides the residue. No graphs or PT pieces are generated here.
*)

(* i M^(1) = i M_self + i M_AV + i M_NAV + i M_box + 2 SigmaV i M_tree.
   Only the mixed-self source is still in raw TID units. Reduced probe
   graphs already contain the loop measure; SigmaV contains 1/(16 Pi^2).
   No pinch kernel is added to the conventional amplitude. *)
qqAssemblyConvention="QQPhysicalLoopMeasure/1";
qqDeclaredLoopMeasure[j_Association] := Lookup[j,"LoopMeasureConversion",
 If[Lookup[j,"Schema",None]==="RGZQQ/3",Missing["LoopMeasureRequired"],1]];
(* Task identity alone does not identify a particular assembly. Bind the
   current mathematical inputs in their fixed physical order. *)
qqAssemblyInputHash[j_Association] := Module[{names={"AV1","AV2","Box1","Box2","NAV1","NAV2"},fields},
 fields={"Tree","MixedSelfEnergy","OnShellSigmaV","Gauge","RegulatorMass","LoopMeasureConversion"};
 If[!TrueQ[Lookup[j,"SelfPrepared",False]]||!And@@(KeyExistsQ[j,#]&/@fields)||
 !AssociationQ[Lookup[j,"Reduced",None]]||!And@@(KeyExistsQ[j["Reduced"],#]&/@names)||
 !qqReducedPartsValidQ[j]||qqFFInvalidQ[{Lookup[j,fields],Lookup[j["Reduced"],names]}],
 Return[Missing["IncompleteAssemblyInputs"]]];
 Hash[{"QQAssemblyInputs/1",qqAssemblyConvention,Lookup[j,"Fingerprint",None],
 Lookup[j,fields],Lookup[j["Reduced"],names]},"SHA256"]];
RGZQQAmplitude[] := engineEvaluation@Block[{$IterationLimit=Infinity},Module[
 {job=RGZResult["QQ"],required={"AV1","AV2","Box1","Box2","NAV1","NAV2"},missing,bad,
  measure,self,av,nav,boxes,residues,loop,packet,saved},
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[!And@@(KeyExistsQ[job,#]&/@{"Tree","OnShellSigmaV","Reduced"})||!AssociationQ[job["Reduced"]]||
   (TrueQ[Lookup[job,"SelfPrepared",False]]&&!KeyExistsQ[job,"MixedSelfEnergy"]),
  Return[Failure["QQAmplitudeIncomplete",<|"Action"->"RGZQQStart[] and recompute missing components."|>]]];
 bad=Select[Join[{"Tree","OnShellSigmaV"},If[TrueQ[Lookup[job,"SelfPrepared",False]],{"MixedSelfEnergy"},{}]],qqFFInvalidQ[job[#]]&];
 bad=Join[bad,Select[Keys[job["Reduced"]],qqFFInvalidQ[job["Reduced"][#]]&]];
 If[bad=!={},Return[Failure["QQAmplitudeIncomplete",<|"Parts"->bad,"Action"->"RGZQQRepairIncomplete[600]; then RGZQQAmplitude[]"|>]]];
 If[Lookup[job,"Fingerprint",None]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],
  Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 If[!qqReducedPartsValidQ[job],Return[Failure["QQPartProvenanceRequired",<|"Action"->"Recompute the reduced parts with the current inputs."|>]]];
 If[!TrueQ[Lookup[job,"SelfPrepared",False]],Return[Failure["QQPrepareSelfFirst",<|"Action"->"RGZQQPrepareSelf[600]"|>]]];
 missing=Select[required,!KeyExistsQ[job["Reduced"],#]&];
 If[missing=!={},Return[Failure["QQPartsRequired",<|"Parts"->missing,"Action"->"RGZQQRunPart[name,seconds]"|>]]];
 measure=qqDeclaredLoopMeasure[job];
 If[!qqPhysicalLoopMeasureQ[measure],Return[Failure["QQMeasureConventionRequired",<||>]]];

 (* This is the complete conventional one-loop sum in the stored iM convention. *)
 self=measure job["MixedSelfEnergy"];
 av=job["Reduced"]["AV1"]+job["Reduced"]["AV2"];
 nav=job["Reduced"]["NAV1"]+job["Reduced"]["NAV2"];
 boxes=job["Reduced"]["Box1"]+job["Reduced"]["Box2"];
 residues=2 job["OnShellSigmaV"] job["Tree"];
 loop=self+av+nav+boxes+residues;
 packet=<|"Status"->"BareAmplitudeAssembled","Process"->job["Process"],"Fingerprint"->job["Fingerprint"],
 "ModelFingerprint"->Lookup[job,"ModelFingerprint",None],"ModelContent"->Lookup[job,"ModelContent",None],
 "JacobianGhostIncluded"->Lookup[job,"JacobianGhostIncluded",False],"ClosedQuarkFlavors"->Lookup[job,"ClosedQuarkFlavors",0],
 "Gauge"->job["Gauge"],"RegulatorMass"->job["RegulatorMass"],"Tree"->job["Tree"],"OneLoop"->loop,
 "iAmplitude"->job["Tree"]+loop,"Amplitude"->(job["Tree"]+loop)/I,"ExternalResidues"->residues,
 "PinchDoubleCounted"->False,"Renormalized"->False,"GaugeIndependenceVerified"->False,
 "OutstandingChecks"->{"Full amplitude Ward identities including vertex and box remainders","Local MSbar counterterm closure","UV and IR pole separation"},
 "AssemblyInputHash"->qqAssemblyInputHash[job],
 "AssemblyConvention"->qqAssemblyConvention,"MixedSelfEnergyLoopMeasure"->measure,
 "MixedSelfEnergySourceConvention"->"RawTID; source checkpoints unchanged"|>;
 saved=atomicWXF[FileNameJoin[{job["Directory"],"bare-amplitude.wxf"}],packet];If[FailureQ[saved],Return[saved]];
 invalidateQQDerived[];storeResult["QQAmplitude",packet];
 KeyTake[packet,{"Status","Process","Renormalized","GaugeIndependenceVerified","OutstandingChecks","AssemblyConvention"}]
]];
