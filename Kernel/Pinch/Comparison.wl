(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Compare the separate PT candidate with the full conventional QQ gauge difference.

   Inputs:
   Completed candidate parts, the symbolic-gauge QQ task and saved NAV Ward evidence.

   Dependencies:
   Workflow.wl and Continuation.wl supply candidate data; Identities/GaugeDependence.wl
   supplies the conventional comparison; Renormalization/ provides the declared mass limit.
   This branch is used only by the explicitly requested RGZQQWardCoverage check.
*)

(* Reuse the established generic-D M0 prescription, but sum each Laurent
   order per master and restore finite masters separately. No global Together. *)
qqGaugePinchM0[expr_,key_,dir_,seconds_] := Module[{pairs,terms={},rows,values,r,finite,restored={},stop=Unique["qqGaugeM0"],require},Catch[
 require[x_]:=If[qqFFInvalidQ[x],Throw[If[FailureQ[x],x,Failure["QQPinchM0Incomplete",<||>]],stop],x];
 If[!DirectoryQ[dir],CreateDirectory[dir]];
 pairs=require[qqSelfCheckpoint[dir,"pairs",key,RGZWorkflow`Engine`RGZGaugePVPairs[expr],seconds]];
 Do[qqSelfProgress["PinchM0Coefficient",i,Length[pairs]];
 AppendTo[terms,require[qqSelfCheckpoint[dir,"co-"<>ToString[i],key,
 RGZWorkflow`Engine`rgzQQM0Term[pairs[[i]],seconds],seconds]]],{i,Length[pairs]}];
 Do[rows=require[qqSelfCheckpoint[dir,"assemble-pairs-"<>ToString[i],key,
 RGZWorkflow`Engine`RGZGaugePVPairs[RGZResidual39Diagnostic`Private`publicMaster[Total[terms[[All,i]]]]],seconds]];
 values={};Do[qqSelfProgress["PinchM0Assemble:"<>ToString[2 i-6],j,Length[rows]];
 r=require[qqSelfCheckpoint[dir,"assemble-"<>ToString[i]<>"-"<>ToString[j],key,qqSimplifyCoefficient[rows[[j,2]]],seconds]];
 If[i<3&&r=!=0,Throw[Failure["QQPinchM0Divergent",<|"Power"->2 i-6,"Master"->rows[[j,1]]|>],stop]];
 AppendTo[values,rows[[j,1]] r],{j,Length[rows]}];
 r=require[qqSelfCheckpoint[dir,"sum-"<>ToString[i],key,Total[values],seconds]];
 If[i===3,finite=r],{i,3}];
 rows=require[qqSelfCheckpoint[dir,"restore-pairs",key,RGZWorkflow`Engine`RGZGaugePVPairs[finite],seconds]];
 Do[AppendTo[restored,require[qqSelfCheckpoint[dir,"restore-"<>ToString[i],key,qqRestoreProgress[Times@@rows[[i]]],seconds]]],{i,Length[rows]}];
 Total[restored],stop]];
qqGaugePinchRawAmplitude[seconds_] := engineEvaluation@Module[{p,j=qqGaugeValidJob[],work,expected,scalar,r,file,key},
 If[FailureQ[j],Return[j]];r=loadPTEngine[];If[FailureQ[r],Return[r]];
 p=RGZPinchResult[];If[FailureQ[p],Return[p]];
 If[Lookup[p,"Mode",None]=!="FourPinch"||!fullNAVIncluded[],Return[Failure["QQFullMatrixPinchRequired",<|"Action"->"RGZPTStart[] and complete all four pinch parts"|>]]];
 (* Check the actual mixed matrix source; the old PT job did not retain its hash. *)
 work=RGZWorkflow`Engine`RGZGaugeStart[<|"T"->j["MixedTransverse"],"L"->ConstantArray[0,{8,8}],
 "TFieldOrder"->RGZWorkflow`Engine`RGZMixBasis["T"],"LFieldOrder"->RGZWorkflow`Engine`RGZMixBasis["L"]|>];If[FailureQ[work],Return[work]];
 expected=RGZWorkflow`Engine`RGZPTCommonSelf[work];
 If[Hash[expected,"SHA256"]=!=Hash[p["CommonConventionSelfEnergyTParts"],"SHA256"],Return[Failure["QQPinchMixedSourceMismatch",<||>]]];
 key=Hash[{Hash[p["CandidateSumT"],"SHA256"],j["Fingerprint"],pinchFormulaFingerprint[],navFormulaFingerprint[],qqGaugeIR},"SHA256"];
 file=FileNameJoin[{cacheDirectory[],"qq-gauge-pinch-m0-"<>IntegerString[key,16,64]<>".wxf"}];
 If[FileExistsQ[file],r=Import[file,"WXF"];If[AssociationQ[r]&&Lookup[r,"Key",None]===key&&!qqFFInvalidQ[Lookup[r,"Value",$Failed]],Return[r["Value"]]]];
 (* Match the existing mixed-self attachment exactly: scalar=-D Pi D,
    then -g_s^2 J P_T J; both conventional and PT inputs include the loop measure. *)
 scalar=qqGaugePinchM0[RGZWorkflow`Engine`RGZLoopPoleMasses[
 RGZWorkflow`Engine`RGZLoopColour[p["CandidateSumT"]]],key,file<>"-parts",seconds];If[qqFFInvalidQ[scalar],Return[Failure["QQPinchM0Incomplete",<|"Cause"->scalar|>]]];
 r=FeynCalc`Contract[FeynCalc`FCI[-FeynCalc`SMP["g_s"]^2/I
 RGZWorkflow`Engine`rgzQQBornCurrent[RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu]
 RGZWorkflow`Engine`RGZLoopPT[RGZWorkflow`Engine`p,RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu] scalar]];
 r=r/.{RGZWorkflow`Engine`p->RGZWorkflow`Engine`qqK3-RGZWorkflow`Engine`qqK1,RGZWorkflow`Engine`\[Alpha]->qqGaugeAlpha};
 r=qqFFSpinReduce[r];If[qqFFInvalidQ[r],Return[Failure["QQPinchAttachmentIncomplete",<||>]]];
 scalar=atomicWXF[file,<|"Key"->key,"Value"->r|>];If[FailureQ[scalar],scalar,r]];
(* Raw pinch checkpoints use the same single measure conversion as self energy. *)
qqGaugePinchAmplitude[seconds_] := Module[{job=qqGaugeValidJob[],measure,raw},
 If[FailureQ[job],Return[job]];measure=qqDeclaredLoopMeasure[job];
 If[qqFFInvalidQ[measure],Return[Failure["QQMeasureConventionRequired",<||>]]];
 raw=qqGaugePinchRawAmplitude[seconds];If[FailureQ[raw],raw,measure raw]];
Options[RGZQQWardCoverage]={"ReferenceGauge"->1,"CoefficientSeconds"->10,"ReduceTensorIntegrals"->False};
RGZQQWardCoverage[seconds_:1200,OptionsPattern[]] := Module[{p,r,evidence,pinch,bridge,zero,ref=OptionValue["ReferenceGauge"],co=OptionValue["CoefficientSeconds"],file,reduce=OptionValue["ReduceTensorIntegrals"]},
 If[!MemberQ[{True,False},reduce],Return[Failure["QQPVReductionOption",<||>]]];
 qqPVRestoreNAVProof[];
 Block[{$qqGaugeNativePV=reduce},
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[co]||!TrueQ[co>0],Return[Failure["TimeLimit",<||>]]];
 $results=KeyDrop[$results,{"QQWardCoverage"}];
 p=qqGaugePacket[];If[FailureQ[p],Return[p]];
 r=qqGaugeTimed["QQWardCoverage",Module[{},
 evidence=qqGaugeWardEvidence[];
 If[!AllTrue[Values[evidence],TrueQ],Return[Failure["QQWardPrerequisites",<|"ChecksPassed"->evidence,"Action"->"Run RGZCheckPropagators[] and prepare the current full-matrix NAV pinch; then repeat."|>]]];
 pinch=qqGaugePinchAmplitude[seconds];If[FailureQ[pinch],Return[pinch]];
 bridge=qqGaugeCompare[p["OneLoop"]/I-pinch,ref,seconds,co,"CoverageBridge"];If[FailureQ[bridge],Return[bridge]];
 zero=qqGaugeCompare[pinch,ref,seconds,co,"PinchGaugeDifference"];If[FailureQ[zero],Return[zero]];
 <|"Status"->Which[!bridge["AllCoefficientsZero"],"PinchCoverageNotEstablished",!zero["AllCoefficientsZero"],"PinchGaugeDifferenceNotZero",True,"OnShellPinchCoverageAndGaugeZeroVerified"],
 "Name"->"QQWardCoverage","WardEvidence"->evidence,"CoverageVerified"->bridge["AllCoefficientsZero"],"PinchDifferenceZero"->zero["AllCoefficientsZero"],
 "GaugeIndependenceVerified"->(bridge["AllCoefficientsZero"]&&zero["AllCoefficientsZero"]),"CoverageBridge"->bridge,"PinchCheck"->zero,
 "GaugeJobFingerprint"->p["GaugeJobFingerprint"],"IRPrescription"->qqGaugeIR,"ReferenceGauge"->ref,"PointTest"->False,
 "FunctionalOneLoopSTProved"->False,"Scope"->"This on-shell massless qqprime process with two dynamical quark flavours; not a proof of the full effective-action ST/Nielsen identities.","Renormalized"->False,"UVIRSeparated"->False|>],seconds];
 If[FailureQ[r],Return[r]];file=FileNameJoin[{cacheDirectory[],"qq-ward-coverage-"<>IntegerString[Hash[{p["GaugeJobFingerprint"],ref},"SHA256"],16,64]<>".wxf"}];
 r=Join[r,<|"File"->file|>];p=atomicWXF[file,r];If[FailureQ[p],Return[p]];storeResult["QQWardCoverage",r];
 Join[KeyTake[r,{"Status","Name","File"}],<|"Statistics"->KeyTake[r,{"CoverageVerified","PinchDifferenceZero","GaugeIndependenceVerified"}]|>]]];

(* Existing NAV Ward evidence *)
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

