(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Narrow, explicit migration for the audited -I g probe backend. The model
   itself uses FR$DSign=+1, hence D=partial-I g A and a +I g quark vertex.
   Even numbers of quark vertices are unchanged; NAV has three. *)
qqLegacyProbeBackendHash=FromDigits["4f9aa6a4058a13ee402e59ce6d8c8b77937bc30cbd7a5ad9d73e08108a96140c",16];
qqLegacyProbeFingerprint[gauge_,reg_] := Hash[{"RGZQQ/3",
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Scattering.wl"}],"SHA256"],$Version,modelKey[],navFormulaFingerprint[],qqLegacyProbeBackendHash,gauge,reg},"SHA256"];
Options[RGZQQRepairProbeConvention]={"File"->Automatic};
RGZQQRepairProbeConvention[OptionsPattern[]] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[
 {j,file=OptionValue["File"],r,old,new,dir,target,cached,red,names={"AV1","AV2","Box1","Box2","NAV1","NAV2"},p,source,changed={}},
 r=ready[];If[FailureQ[r],Return[r]];
 j=If[file===Automatic,RGZResult["QQ"],If[StringQ[file]&&FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed]];
 If[!AssociationQ[j]||!And@@(KeyExistsQ[j,#]& /@{"Gauge","RegulatorMass","Fingerprint","Directory"}),Return[Failure["QQRepairJobRequired",<|"Action"->"Restore the old QQ job or supply File -> old/job.wxf."|>]]];
 old=j["Fingerprint"];new=qqJobFingerprint[j["Gauge"],j["RegulatorMass"]];
 If[old===new,
 If[!TrueQ[qqSessionJobValidQ[j]],Return[Failure["QQRepairJobInvalid",<|"Action"->"Load a complete matching QQ job or start a fresh task."|>]]];
 invalidateQQDerived[];storeResult["QQ",j];Return[<|"Status"->"CurrentProbeConvention","Reused"->True|>]];
 If[old=!=qqLegacyProbeFingerprint[j["Gauge"],j["RegulatorMass"]],Return[Failure["QQUnknownLegacyProbe",<|"Explanation"->"The source/model/version fingerprint is not the audited legacy convention; no cache was altered."|>]]];
 If[!TrueQ[Lookup[j,"SelfPrepared",False]]||!KeyExistsQ[j,"MixedSelfEnergy"]||qqFFInvalidQ[j["MixedSelfEnergy"]],Return[Failure["QQCompleteLegacySelfRequired",<|"Explanation"->"Migration requires the completed unchanged mixed self energy."|>]]];
 If[AnyTrue[{Lookup[j,"Tree",$Failed],Lookup[j,"OnShellSigmaV",$Failed]},qqFFInvalidQ],Return[Failure["QQIncompleteLegacyProbe",<||>]]];
 If[!IntegerQ[new],Return[Failure["QQRepairJobInvalid",<||>]]];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_qq_"<>IntegerString[new,16]}];target=FileNameJoin[{dir,"job.wxf"}];
 If[!TrueQ[qqSessionJobValidQ[Join[j,<|"Fingerprint"->new,"Directory"->dir|>]]],
 Return[Failure["QQRepairJobInvalid",<|"Action"->"Supply a complete matching legacy QQ job; invalid tasks cannot be migrated."|>]]];
 If[FileExistsQ[target],cached=Quiet[Check[Import[target,"WXF"],$Failed]];
 If[AssociationQ[cached]&&Lookup[cached,"Fingerprint",None]===new&&Lookup[cached,"ProbeQuarkConvention",None]==="DminusIgA/1"&&TrueQ[qqSessionJobValidQ[cached]],$results=KeyDrop[$results,{"QQAmplitude","QQFormFactors","QQGaugeAmplitude","QQGaugeDirect","QQWardCoverage","QQGaugeEarlyPrepared","QQGaugeEarly"}];invalidateQQDerived[];storeResult["QQ",cached];Return[<|"Status"->"ProbeConventionRepaired","Reused"->True,"File"->target|>]];
 Return[Failure["QQRepairTargetExists",<|"File"->target|>]]];
 If[!DirectoryQ[dir],CreateDirectory[dir]];
 red=Lookup[j,"Reduced",<||>];If[!AssociationQ[red],red=<||>];
 Do[If[KeyExistsQ[red,name]&&!qqFFInvalidQ[red[name]],
   If[MemberQ[{"NAV1","NAV2"},name],red[name]=-red[name];AppendTo[changed,name]]];
 source=FileNameJoin[{j["Directory"],name<>"-reduced.wxf"}];
 If[FileExistsQ[source],p=Quiet[Check[Import[source,"WXF"],$Failed]];
 If[AssociationQ[p]&&Lookup[p,"Fingerprint",None]===old&&Lookup[p,"Part",None]===name&&!qqFFInvalidQ[Lookup[p,"Expression",$Failed]],
 p=Join[p,<|"Fingerprint"->new,"Expression"->If[MemberQ[{"NAV1","NAV2"},name],-p["Expression"],p["Expression"]],"ProbeQuarkConvention"->"DminusIgA/1"|>];
 r=atomicWXF[FileNameJoin[{dir,name<>"-reduced.wxf"}],p];If[FailureQ[r],Throw[r,rgzFunctionReturnTag]];
 red[name]=p["Expression"];If[MemberQ[{"NAV1","NAV2"},name],AppendTo[changed,name]]]],{name,names}];
 j=Join[j,<|"Fingerprint"->new,"Directory"->dir,"Reduced"->red,"ProbeQuarkConvention"->"DminusIgA/1",
 "MigratedFromFingerprint"->old,"GaugeIndependenceVerified"->False,"Status"->"ProbeConventionRepaired"|>];
 r=atomicWXF[target,j];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,{"QQAmplitude","QQFormFactors","QQGaugeAmplitude","QQGaugeDirect","QQWardCoverage","QQGaugeEarlyPrepared","QQGaugeEarly"}];
 invalidateQQDerived[];storeResult["QQ",j];
 <|"Status"->"ProbeConventionRepaired","Name"->"QQProbeConvention","SelfEnergyReused"->True,"NAVSignsReversed"->DeleteDuplicates[changed],"File"->target|>],rgzFunctionReturnTag]];
