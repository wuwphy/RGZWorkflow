(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Remove gauge-independent algebra before expensive PV/tensor reduction.
   This changes only comparison preparation, never physical diagram caches. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVScalarOrder.wl"}]];
$qqGaugePreparationVersion=Hash[{FileHash[$InputFileName,"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVScalarOrder.wl"}],"SHA256"]},"SHA256"];
qqGaugeScalarBasisQ[p_] := qqFFPacketQ[p]&&
 AllTrue[If[p["Rows"]==={}, {},Flatten[Lookup[p["Rows"],"MasterCoefficients",{}],1]],
  MatchQ[#,{1|_FeynCalc`A0|_FeynCalc`B0|_FeynCalc`C0|_FeynCalc`D0,_}]&]&&
 !qqFFInvalidQ[p];
qqGaugeLinearDifference[input_,alpha_,reference_] := Module[{delta},
 delta[x_] := Module[{f,free,dep,terms={},prefix=1,suffix},
  If[FreeQ[x,alpha],Return[0]];
  Switch[Head[x],
   Plus,Total[delta /@ List@@x],
   Times,f=List@@x;free=Select[f,FreeQ[#,alpha]&];dep=Select[f,!FreeQ[#,alpha]&];
    Do[suffix=Times@@Drop[dep,i];AppendTo[terms,prefix delta[dep[[i]]] suffix];
     prefix*=dep[[i]]/.alpha->reference,{i,Length[dep]}];Times@@free Total[terms],
   _,x-(x/.alpha->reference)]];
 delta[input]];
qqGaugeDifferenceBuild[packet_,simplify_,co_,reduce_,reference_] := Module[
 {input=packet["Amplitude"],alpha=packet["Gauge"],ints,delta,r,p,canonical,source,remaining,
  basisKey,basisFile,index,candidate,deltaHash},
 source=Hash[input,"SHA256"];
 qqProgressOperation["Check gauge dependence of PV arguments before reduction"];
 ints=DeleteDuplicates[Cases[{input},m:(_FeynCalc`PaVe|_FeynCalc`A0|_FeynCalc`A00|_FeynCalc`B0|_FeynCalc`B1|_FeynCalc`B00|_FeynCalc`B11|_FeynCalc`C0|_FeynCalc`D0|_FeynCalc`E0):>m,Infinity]];
 If[AnyTrue[ints,!FreeQ[#,alpha]&],Return[Failure["QQGaugePVArguments",<|"Explanation"->"Gauge-dependent PV arguments require a different comparison basis."|>]]];
 qqProgressOperation["Remove gauge-independent terms before PV reduction"];
 delta=qqGaugeLinearDifference[input,alpha,reference];
 canonical=Dispatch[(#->qqPVScalarCanonical[#])& /@ ints];
 delta=delta/.canonical;
 remaining=DeleteDuplicates[Cases[{delta},m:(_FeynCalc`PaVe|_FeynCalc`A0|_FeynCalc`A00|_FeynCalc`B0|_FeynCalc`B1|_FeynCalc`B00|_FeynCalc`B11|_FeynCalc`C0|_FeynCalc`D0|_FeynCalc`E0):>m,Infinity]];
 qqProgressUpdate[<|"GaugeDifferenceInputLeaves"->LeafCount[input],"GaugeDifferenceOutputLeaves"->LeafCount[delta],
  "GaugeDifferencePVBefore"->Length[ints],"GaugeDifferencePVAfter"->Length[remaining]|>];
 If[delta===0,
  storeResult["QQFormFactors",<|"Schema"->"RGZQQFormFactors/1","InputSHA256"->Hash[0,"SHA256"],
   "ComparisonSourceSHA256"->source,"GaugeDifferencePreparedBeforeReduction"->True,
   "Rows"->{},"SpinStructures"->{},"ColorBasis"->qqFFColors[],"ReconstructionVerified"->True,
   "Gauge"->alpha,"GaugeIndependenceVerified"->False,"Renormalized"->False|>];
  Return[<|"Status"->"ExactGaugeDifferenceZero"|>]];
 p=Join[packet,<|"Amplitude"->delta|>];
 deltaHash=Hash[delta,"SHA256"];
 basisKey=Hash[{"QQGaugeScalarBasis/1",deltaHash,reduce,simplify,$Version,FeynCalc`$FeynCalcVersion,$qqGaugePreparationVersion,
  FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringFormFactors.wl"}],"SHA256"],
  FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVRelations.wl"}],"SHA256"],
  FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVGram.wl"}],"SHA256"],
  DownValues[FeynCalc`PaVeReduce],DownValues[FeynCalc`PaVeOrder],Options[FeynCalc`PaVeReduce],Options[FeynCalc`PaVeOrder]},"SHA256"];
 basisFile=FileNameJoin[{cacheDirectory[],"qq-gauge-scalar-bases",IntegerString[basisKey,16,64]<>".wxf"}];
 If[FileExistsQ[basisFile],
  index=Quiet[Check[Import[basisFile,"WXF"],$Failed]];
  If[AssociationQ[index]&&Lookup[index,"Key",None]===basisKey&&StringQ[Lookup[index,"File",None]]&&FileExistsQ[index["File"]],
   qqProgressOperation["Restore completed scalar basis independent of coefficient time limit"];
   candidate=Quiet[Check[Import[index["File"],"WXF"],$Failed]];
   If[AssociationQ[candidate]&&Lookup[candidate,"InputSHA256",None]===deltaHash&&qqGaugeScalarBasisQ[candidate],
    storeResult["QQFormFactors",Join[candidate,<|"ComparisonSourceSHA256"->source,"GaugeDifferencePreparedBeforeReduction"->True|>]];
    qqProgressUpdate[<|"ScalarBasisReusedAcrossTimeLimits"->True|>];
    Return[<|"Status"->"CompletedScalarBasisReused"|>]]]];
 r=qqFFBuild[p,simplify,co,reduce];If[FailureQ[r],Return[r]];
 p=RGZResult["QQFormFactors"];
 If[!AssociationQ[p],Return[Failure["QQGaugeFormFactorsIncomplete",<||>]]];
 If[qqGaugeScalarBasisQ[p]&&StringQ[Lookup[p,"File",None]]&&FileExistsQ[p["File"]],
  If[!DirectoryQ[DirectoryName[basisFile]],CreateDirectory[DirectoryName[basisFile],CreateIntermediateDirectories->True]];
  index=atomicWXF[basisFile,<|"Key"->basisKey,"File"->p["File"]|>];If[FailureQ[index],Return[index]]];
 storeResult["QQFormFactors",Join[p,<|"ComparisonSourceSHA256"->source,"GaugeDifferencePreparedBeforeReduction"->True|>]];r];
(* Preserve the validation of the actual source amplitude, while ff's own
   InputSHA256 continues to describe the reduced gauge-difference expression. *)
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[qqFFBuild[p_,False,t_,reduce_]]:>qqGaugeDifferenceBuild[p,False,t,reduce,reference];
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[Lookup[r_,"InputSHA256",None]]:>Lookup[r,"ComparisonSourceSHA256",Lookup[r,"InputSHA256",None]];
(* The prepared form factors already represent M(alpha)-M(reference).
   Reduce that coefficient directly instead of constructing the same
   reference subtraction a second time. *)
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[qqCoefficientReduce[value_]]:>qqCoefficientReduce[
  If[TrueQ[Lookup[ff,"GaugeDifferencePreparedBeforeReduction",False]],co,value]];
