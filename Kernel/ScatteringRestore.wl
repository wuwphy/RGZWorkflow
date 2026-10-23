(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Exact linear M0 restoration. Excluded from physical diagram fingerprints. *)
If[!NumericQ[$qqRestoreCoefficientSeconds]||!TrueQ[$qqRestoreCoefficientSeconds>0],$qqRestoreCoefficientSeconds=0.25];
qqRestoreInvalidQ[x_] := !FreeQ[x,_Failure|_Missing|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity];
qqRestoreSubstitute[x_] := Module[{z=x},
 z=z/.{HoldPattern[RGZResidual39Diagnostic`Private`h^n_Integer]/;EvenQ[n]:>RGZWorkflow`Engine`\[Alpha]^(n/2)};
 z=z/.{RGZResidual39Diagnostic`Private`h->Sqrt[RGZWorkflow`Engine`\[Alpha]],RGZResidual39Diagnostic`Private`dim->System`D};
 RGZResidual39Diagnostic`Private`publicMaster[z]/.{
 RGZWorkflow`Engine`qqM0S->FeynCalc`SPD[RGZWorkflow`Engine`p,RGZWorkflow`Engine`p],
 RGZWorkflow`Engine`qqM0X->RGZWorkflow`Engine`m^2,RGZWorkflow`Engine`qqM0Y->RGZWorkflow`Engine`\[Mu]^2,
 RGZWorkflow`Engine`qqM0Z->RGZWorkflow`Engine`mG^4,RGZWorkflow`Engine`qqM0N->FeynCalc`SUNN}];
qqRestoreCompact[raw_] := Module[{z=raw,c},
 qqProgressOperation["Optional coefficient cancellation (bounded)"];
 c=TimeConstrained[Cancel[z],$qqRestoreCoefficientSeconds,$TimedOut];
 If[!qqRestoreInvalidQ[c]&&LeafCount[c]<=LeafCount[z],z=c];
 qqProgressOperation["Optional coefficient factorization (bounded)"];
 c=TimeConstrained[Factor[z],$qqRestoreCoefficientSeconds,$TimedOut];
 If[!qqRestoreInvalidQ[c]&&LeafCount[c]<=LeafCount[z],z=c];z];
qqRestoreProgress[finite_] := Module[{job=RGZResult["QQ"],base,source,key,dir,rows,values={},master,co,value,packet,file,r,stop=Unique["qqRestore"]},Catch[
 If[!AssociationQ[$qqProgress],$qqProgress=<||>];
 If[qqRestoreInvalidQ[finite],Throw[Failure["QQRestoreIncompleteInput",<||>],stop]];
 source=If[AssociationQ[job],{Lookup[job,"Fingerprint",None],Lookup[job,"MixedSourceHash",None]},None];
 base=If[AssociationQ[job]&&StringQ[Lookup[job,"Directory",None]],FileNameJoin[{job["Directory"],"self"}],cacheDirectory[]];
 key=Hash[{"QQLinearM0Restore/1",source,Hash[finite,"SHA256"],$Version},"SHA256"];
 dir=FileNameJoin[{base,"restore-linear-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
 qqSelfProgress["M0Restore:Split",0,1];qqProgressOperation["Collect finite M0 masters without a global denominator"];
 rows=RGZWorkflow`Engine`RGZGaugePVPairs[RGZResidual39Diagnostic`Private`publicMaster[finite]];
 If[qqRestoreInvalidQ[rows]||!ListQ[rows],Throw[Failure["QQRestoreMasterSplit",<|"Cause"->rows|>],stop]];
 Do[qqSelfProgress["M0Restore",i,Length[rows]];
 qqProgressUpdate[<|"Master"->rows[[i,1]],"InputLeaves"->LeafCount[rows[[i,2]]],"StepLimitSeconds"->"total; optional algebra "<>ToString[$qqRestoreCoefficientSeconds]|>];
 file=FileNameJoin[{dir,"master-"<>ToString[i]<>".wxf"}];qqProgressUpdate[<|"CheckpointFile"->file|>];
 If[FileExistsQ[file],
 qqProgressOperation["Read restored master checkpoint"];packet=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[packet]||Lookup[packet,"Key",None]=!=key||Lookup[packet,"Input",None]=!=rows[[i]]||qqRestoreInvalidQ[Lookup[packet,"Value",$Failed]],Throw[Failure["QQRestoreCheckpointInvalid",<|"File"->file|>],stop]];
 value=packet["Value"];qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>],
 qqProgressOperation["Restore this master and its scalar coefficient"];
 master=qqRestoreSubstitute[rows[[i,1]]];co=qqRestoreSubstitute[rows[[i,2]]];
 If[qqRestoreInvalidQ[{master,co}],Throw[Failure["QQRestoreSubstitutionIncomplete",<||>],stop]];
 co=qqRestoreCompact[co];value=master co;
 If[qqRestoreInvalidQ[value],Throw[Failure["QQRestoreCoefficientIncomplete",<||>],stop]];
 qqProgressOperation["Save restored master checkpoint"];
 r=atomicWXF[file,<|"Key"->key,"Input"->rows[[i]],"Value"->value,"Method"->"ExactSubstitutionWithBoundedOptionalAlgebra"|>];If[FailureQ[r],Throw[r,stop]];
 qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>]];
 AppendTo[values,value];qqProgressUpdate[<|"OutputLeaves"->LeafCount[value],"OperationFinished"->AbsoluteTime[]|>],{i,Length[rows]}];
 qqSelfProgress["M0Restore:Sum",Length[rows],Length[rows]];
 qqProgressOperation["Sum restored masters; no global Cancel or Factor"];
 value=FeynCalc`SMP["g_s"]^2 Total[values];
 qqProgressUpdate[<|"CacheState"->"RestoredMastersCheckpointed","OutputLeaves"->LeafCount[value],"OperationFinished"->AbsoluteTime[]|>];value,stop]];
