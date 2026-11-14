(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Native restoration and cache regressions in isolated temporary work. *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];
 If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
 RGZWorkflow`Private`loadQuarkEngine[];RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
checks=<||>;rec[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
Clear[m,mG,\[Mu],\[Alpha]];FCClearScalarProducts[];
input=RGZResidual39Diagnostic`Private`a0[qqM0X];
assigned=Block[{m=2},RGZWorkflow`Private`qqRestoreProgress[input]];
symbolic=RGZWorkflow`Private`qqRestoreProgress[input];
rec["AssignedMassRestoration",assigned===SMP["g_s"]^2 A0[4]];
rec["UnassignedMassDoesNotReuseNumericCache",symbolic===SMP["g_s"]^2 A0[m^2]];
file=RGZWorkflow`Private`$qqProgress["CheckpointFile"];
packet=Import[file,"WXF"];Export[file,Join[packet,<|"Value"->777|>],"WXF"];
rec["FiniteCorruptedValueRejected",FailureQ[RGZWorkflow`Private`qqRestoreProgress[input]]];
Export[file,packet,"WXF"];
reused=Lookup[RGZWorkflow`Private`$qqProgress,"ReusedCheckpoints",0];
rec["ValidCheckpointReused",RGZWorkflow`Private`qqRestoreProgress[input]===symbolic&&Lookup[RGZWorkflow`Private`$qqProgress,"ReusedCheckpoints",0]===reused+1];
Do[
 Internal`InheritedBlock[{RGZGaugePVPairs},Clear[RGZGaugePVPairs];RGZGaugePVPairs[___]:=bad;
  r=Quiet[RGZWorkflow`Private`qqRestoreProgress[input]];
  rec["MalformedRowsRejected"<>ToString[bad,InputForm],MatchQ[r,Failure["QQRestoreMasterSplit",_]]]],
 {bad,{{{A0[qqM0X]}},{{A0[qqM0X],1,2}},{{A0[qqM0X],{1,2}}},{{A0[qqM0X],A0[qqM0Y]}},{{unknownMaster[qqM0X],1}},{{A0[qqM0X],HoldForm[Part[unknownVector,1]]}}}}];
bubble=RGZResidual39Diagnostic`Private`b0[qqM0S,qqM0X,qqM0Y];
Internal`InheritedBlock[{Pair,ScalarProduct,SPD},
 SPD[p,p]=9;
 numericInvariant=RGZWorkflow`Private`qqRestoreProgress[bubble]];
symbolicInvariant=RGZWorkflow`Private`qqRestoreProgress[bubble];
rec["ChangedExternalInvariantDoesNotReuseCache",numericInvariant===SMP["g_s"]^2 B0[9,m^2,\[Mu]^2]&&symbolicInvariant===SMP["g_s"]^2 B0[SPD[p,p],m^2,\[Mu]^2]];
(* The linear physical restoration retains D and the principal root. *)
finite=(RGZResidual39Diagnostic`Private`h^2+RGZResidual39Diagnostic`Private`dim) RGZResidual39Diagnostic`Private`a0[qqM0X]+(qqM0X-qqM0Y) RGZResidual39Diagnostic`Private`b0[qqM0S,qqM0X,qqM0Y];
expected=SMP["g_s"]^2 ((\[Alpha]+D) A0[m^2]+(m^2-\[Mu]^2) B0[SPD[p,p],m^2,\[Mu]^2]);
rec["LinearPhysicalFormulaAndDimensionPreserved",Cancel[Together[RGZWorkflow`Private`qqRestoreProgress[finite]-expected]]===0];
rootResult=RGZWorkflow`Private`qqRestoreProgress[RGZResidual39Diagnostic`Private`h RGZResidual39Diagnostic`Private`a0[qqM0X]];
rec["PrincipalGaugeRootPreserved",(rootResult/.\[Alpha]->-1)===I SMP["g_s"]^2 A0[m^2]];
rec["RationalFiniteTermSupported",Cancel[Together[RGZWorkflow`Private`qqRestoreProgress[qqM0X-qqM0Y]-SMP["g_s"]^2 (m^2-\[Mu]^2)]]===0];
rec["ZeroFiniteTermSupported",RGZWorkflow`Private`qqRestoreProgress[0]===0];
Print["SUMMARY ",checks];Exit[If[And@@Values[checks],0,1]];
