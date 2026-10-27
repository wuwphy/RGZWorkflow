(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Optional low-point native PV reduction at the comparison boundary.
   Does not change physical integrands or general-gauge symbol identities. *)
$qqGaugeNativePV=False;
(* The PT cache can survive while Session.wxf lacks its NAV evidence packet.
   Hydrate only the current fingerprint's completed, validated disk packet;
   never start a NAV calculation as a side effect of a Ward check. *)
qqPVValidNAVProof[p_,key_] := navPacketQ[p,"RGZNAVPinch/1",{"Kernel"}]&&Lookup[p,"Fingerprint",None]===key;
qqPVRestoreNAVProof[] := Module[{key,file,p,current=RGZResult["NAVPinch"]},
 If[FailureQ[ready[]],Return[False]];
 If[!FileExistsQ[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}]],Return[False]];
 key=navFormulaFingerprint[];
 If[qqPVValidNAVProof[current,key],Return[True]];
 file=FileNameJoin[{cacheDirectory[],"RGZ_NAV_pinch_"<>IntegerString[key,16,64],"result.wxf"}];
 If[!FileExistsQ[file],Return[False]];
 p=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[p]||Lookup[p,"Schema",None]=!="RGZNAVPinch/1"||
   !MemberQ[{None,key},Lookup[p,"Fingerprint",None]],Return[False]];
 p=Join[p,<|"Fingerprint"->key|>];
 If[!qqPVValidNAVProof[p,key],Return[False]];
 storeResult["NAVPinch",p];True];
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[qqFFBuild[p_,False,t_,False]]:>qqFFBuild[p,False,t,$qqGaugeNativePV];
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[Hash[{h_,"QQCoefficient/1"},"SHA256"]]:>
   Hash[{h,"QQCoefficient/1",$qqGaugeNativePV},"SHA256"];
(* Lookup[{}, key] is Missing rather than an empty list in Wolfram 13.3. *)
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[Counts[Lookup[rows_,"Status"]]]:>If[rows==={},<||>,Counts[Lookup[rows,"Status"]]];

Options[qqPVGaugeCheckOriginal]=Options[RGZQQGaugeCheck];
DownValues[qqPVGaugeCheckOriginal]=DownValues[RGZQQGaugeCheck]/.
 RGZQQGaugeCheck->qqPVGaugeCheckOriginal;
Options[RGZQQGaugeCheck]=Append[Options[RGZQQGaugeCheck],"ReduceTensorIntegrals"->False];
RGZQQGaugeCheck[seconds_:1200,OptionsPattern[]] := Module[{reduce=OptionValue["ReduceTensorIntegrals"]},
 If[!MemberQ[{True,False},reduce],Return[Failure["QQPVReductionOption",<||>]]];
 Block[{$qqGaugeNativePV=reduce},qqPVGaugeCheckOriginal[seconds,
  "ReferenceGauge"->OptionValue["ReferenceGauge"],"CoefficientSeconds"->OptionValue["CoefficientSeconds"]]]];

Options[qqPVWardOriginal]=Options[RGZQQWardCoverage];
DownValues[qqPVWardOriginal]=DownValues[RGZQQWardCoverage]/.
 RGZQQWardCoverage->qqPVWardOriginal;
Options[RGZQQWardCoverage]=Append[Options[RGZQQWardCoverage],"ReduceTensorIntegrals"->False];
RGZQQWardCoverage[seconds_:1200,OptionsPattern[]] := Module[{reduce=OptionValue["ReduceTensorIntegrals"]},
 If[!MemberQ[{True,False},reduce],Return[Failure["QQPVReductionOption",<||>]]];
 qqPVRestoreNAVProof[];
 Block[{$qqGaugeNativePV=reduce},qqPVWardOriginal[seconds,
  "ReferenceGauge"->OptionValue["ReferenceGauge"],"CoefficientSeconds"->OptionValue["CoefficientSeconds"]]]];
