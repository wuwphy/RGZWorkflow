(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Compare the full conventional gauge tasks and their protected integrand differences.

   Inputs:
   Tree identities or computed amplitudes and the source-linked reports being checked.

   Dependencies:
   Tree/, OneLoop/, Algebra/ and Renormalization/ for the result being checked.
*)


(* The full gauge difference *)
(* Independent gauge-verification jobs. Does not change physical QQ fingerprints. *)
(* The private gauge symbol carries the verification algorithm version in its
   context, so existing physical QQ fingerprints isolate all new IR jobs. *)
qqGaugeAlpha = Symbol["RGZWorkflow`Gauge`v"<>StringTake[IntegerString[
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Identities/GaugeDependence.wl"}],"SHA256"],16,64],12]<>"`alpha"];
qqGaugeIR = "DimensionalScalelessZero";
qqGaugeValidJob[] := Module[{j=RGZResult["QQ"],r=ready[]},
 If[FailureQ[r],Return[r]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 If[!AssociationQ[j]||Lookup[j,"Gauge",None]=!=qqGaugeAlpha||Lookup[j,"GaugeIRPrescription",None]=!=qqGaugeIR||Lookup[j,"RegulatorMass",None]=!=0||Lookup[j,"Fingerprint",None]=!=qqJobFingerprint[qqGaugeAlpha,0],
 Failure["QQSymbolicGaugeStartFirst",<|"Action"->"RGZQQGaugeStart[]"|>],j]];
qqGaugeSymbolicParametersQ[] := AllTrue[{"m","mG","M","\[Mu]","\[Alpha]","p","q","qqS","qqT","qqK1","qqK2","qqK3","qqK4"},
 Function[n,ToExpression["RGZWorkflow`Engine`"<>n,InputForm,Function[sym,OwnValues[sym]==={},HoldAll]]]];
Options[RGZQQGaugeStart]={"IRPrescription"->"DimensionalScalelessZero"};
RGZQQGaugeStart[seconds_:600,OptionsPattern[]] := Module[{answer,staged},
 (* Keep the active result snapshot until both task construction and IR tagging
    succeed. Completed source caches and attempt diagnostics remain reusable. *)
 answer=Block[{$results=$results},
  answer=qqGaugeStartTransaction[seconds,OptionValue["IRPrescription"]];
  If[!FailureQ[answer],staged=$results];answer];
 If[FailureQ[answer],Return[answer]];$results=staged;answer];
qqGaugeStartTransaction[seconds_,ir_] := Module[{r,j,old=RGZResult["QQ"],oldAmplitude=RGZResult["QQAmplitude"],oldFactors=RGZResult["QQFormFactors"],sigma},
 r=masslessQQReady[];If[FailureQ[r],Return[r]];
 If[!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["TimeLimit",<||>]]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 If[ir=!=qqGaugeIR,Return[Failure["QQIRPrescription",<|"Explanation"->"Only common dimensional regularization with scaleless integrals zero is implemented; UV/IR poles are not separated."|>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[OwnValues[Evaluate[qqGaugeAlpha]]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
 r=loadQuarkEngine[];If[FailureQ[r],Return[r]];
 r=engineGet["OneLoop/QuarkGraphs.wl",False];If[FailureQ[r],Return[r]];
 sigma=engineEvaluation[RGZWorkflow`Engine`rgzQQSigmaV0[0,0]];
 If[qqFFInvalidQ[sigma],Return[Failure["QQResidueIncomplete",<||>]]];
 (* This is a separately declared IR prescription, not a change to the default
    finite-M residue. The massless longitudinal on-shell integral is scaleless.
    Quark tools are already loaded before this scoped residue prescription. *)
 r=Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=False},
 Internal`InheritedBlock[{RGZWorkflow`Engine`rgzQQSigmaV0},
 RGZWorkflow`Engine`rgzQQSigmaV0[g_,0]:=sigma;
 RGZQQStart[seconds,"Gauge"->qqGaugeAlpha,"RegulatorMass"->0]]];
 If[FailureQ[r],Return[r]];
 If[AssociationQ[old]&&Lookup[old,"GaugeIRPrescription",None]=!=qqGaugeIR,
 storeResult["QQBeforeGauge",<|"QQ"->old,"QQAmplitude"->oldAmplitude,"QQFormFactors"->oldFactors|>]];
 j=RGZResult["QQ"];AssociateTo[j,"GaugeIRPrescription"->qqGaugeIR];
 r=atomicWXF[FileNameJoin[{j["Directory"],"job.wxf"}],j];If[FailureQ[r],Return[r]];
 storeResult["QQ",j];$results=KeyDrop[$results,{"QQAmplitude","QQFormFactors","QQGaugeAmplitude","QQGaugeDirect","QQWardCoverage"}];
 <|"Status"->"IntegrandsReady","Name"->"SymbolicGaugeOneLoop","Gauge"->qqGaugeAlpha,"RegulatorMass"->0,"IRPrescription"->qqGaugeIR,"File"->r["File"]|>];
qqGaugeReuseM0[job_] := Module[{previous=RGZResult["QQBeforeGauge"],old,src,dst,key,oldKey,p,target,count=0},
 If[!AssociationQ[previous]||!AssociationQ[Lookup[previous,"QQ",None]],Return[0]];old=previous["QQ"];
 If[Lookup[old,"RegulatorMass",None]=!=0||Lookup[old,"Fingerprint",None]=!=qqJobFingerprint[Lookup[old,"Gauge",None],0]||
 Lookup[old,"MixedSourceHash",None]=!=Lookup[job,"MixedSourceHash",Missing[]],Return[0]];
 src=FileNameJoin[{old["Directory"],"self"}];If[!DirectoryQ[src],Return[0]];
 dst=FileNameJoin[{job["Directory"],"self"}];If[!DirectoryQ[dst],CreateDirectory[dst]];
 oldKey=Hash[{old["Fingerprint"],old["MixedSourceHash"]},"SHA256"];key=Hash[{job["Fingerprint"],job["MixedSourceHash"]},"SHA256"];
 (* These checkpoints precede alpha evaluation; never copy gauge-* or connected. *)
 Do[target=FileNameJoin[{dst,FileNameTake[file]}];
 If[!FileExistsQ[target],p=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[AssociationQ[p]&&Lookup[p,"Key",None]===oldKey&&!qqFFInvalidQ[Lookup[p,"Value",$Failed]],
 If[!FailureQ[atomicWXF[target,<|"Key"->key,"Value"->p["Value"],"ReusedFrom"->file|>]],count++]]],
 {file,Join[FileNames["matrix-*.wxf",src],FileNames["pairs.wxf",src],FileNames["m0-*.wxf",src]]}];count];
Options[RGZQQGaugePrepareSelf]={"TotalSeconds"->3600};
RGZQQGaugePrepareSelf[seconds_:600,OptionsPattern[]] := Module[{j=qqGaugeValidJob[]},If[FailureQ[j],Return[j]];
 qqGaugeReuseM0[j];
 Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=False},RGZQQPrepareSelf[seconds,"TotalSeconds"->OptionValue["TotalSeconds"]]]];
RGZQQGaugeRunPart[name_String,seconds_:600] := Module[{j=qqGaugeValidJob[]},If[FailureQ[j],Return[j]];
 Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=False},RGZQQRunPart[name,seconds]]];
RGZQQGaugeAssemble[] := Module[{j=qqGaugeValidJob[],r,p},If[FailureQ[j],Return[j]];
 r=RGZQQAmplitude[];If[FailureQ[r],Return[r]];p=RGZResult["QQAmplitude"];
 p=Join[p,<|"IRPrescription"->qqGaugeIR,"ModelFingerprint"->modelKey[],"GaugeJobFingerprint"->j["Fingerprint"]|>];
 r=atomicWXF[FileNameJoin[{j["Directory"],"gauge-amplitude.wxf"}],p];If[FailureQ[r],Return[r]];
 storeResult["QQGaugeAmplitude",p];$results=KeyDrop[$results,{"QQGaugeDirect","QQWardCoverage"}];<|"Status"->"BareOneLoopAssembled","File"->r["File"]|>];
qqGaugeBackupValidQ[b_] := AssociationQ[b]&&TrueQ[qqSessionJobValidQ[Lookup[b,"QQ",None]]];
RGZQQGaugeRestore[] := Module[{r=ready[],b=RGZResult["QQBeforeGauge"]},
 If[FailureQ[r],Return[r]];
 If[MissingQ[b],Return[Failure["QQPreviousTaskMissing",<||>]]];
 If[!qqGaugeBackupValidQ[b],Return[Failure["QQPreviousTaskInvalid",<|"Action"->"Use a complete original QQ task matching the current workspace, model and sources; or start a fresh fixed-gauge task."|>]]];
 invalidateQQDerived[];
 Scan[Function[n,If[AssociationQ[b[n]],storeResult[n,b[n]],$results=KeyDrop[$results,{n}]]],{"QQ","QQAmplitude","QQFormFactors"}];
 $results=KeyDrop[$results,{"QQBeforeGauge"}];
 <|"Status"->"PreviousQQTaskRestored"|>];
qqGaugePacket[] := Module[{p=RGZResult["QQGaugeAmplitude"],j=qqGaugeValidJob[]},
 If[FailureQ[j],Return[j]];
 If[!AssociationQ[p]||Lookup[p,"Gauge",None]=!=qqGaugeAlpha||Lookup[p,"IRPrescription",None]=!=qqGaugeIR||Lookup[p,"ModelFingerprint",None]=!=modelKey[]||Lookup[p,"GaugeJobFingerprint",None]=!=j["Fingerprint"]||qqFFInvalidQ[Lookup[p,"OneLoop",$Failed]],
 Failure["QQGaugeAssembleFirst",<|"Action"->"RGZQQGaugeAssemble[]"|>],
 If[Lookup[p,"AssemblyConvention",None]=!=qqAssemblyConvention,
  Failure["QQGaugeAssembleFirst",<|"Action"->"RGZQQGaugeAssemble[]","Explanation"->"Reassemble with the physical loop measure; raw checkpoints remain valid."|>],p]]];
(* Form-factor slots need only span the amplitude: zero in every slot proves
   zero; nonzero slots in a redundant basis are not a proof of gauge dependence. *)
SetAttributes[qqGaugeTimed,HoldRest];
qqGaugeTimed[name_,expression_,seconds_] := Module[{r,now=AbsoluteTime[]},
 $qqProgress=<|"Stage"->name,"Status"->"Computing","RunStarted"->now,"ItemStarted"->now,"StepLimitSeconds"->seconds|>;
 r=If[TrueQ[$Notebooks],Monitor[Block[{$IterationLimit=Infinity,FeynCalc`$KeepLogDivergentScalelessIntegrals=False},timed[name,expression,seconds]],Dynamic[qqSelfProgressPanel[],UpdateInterval->1,TrackedSymbols:>{$qqProgress}]],
 Block[{$IterationLimit=Infinity,FeynCalc`$KeepLogDivergentScalelessIntegrals=False},timed[name,expression,seconds]]];
 qqProgressUpdate[<|"Status"->If[FailureQ[r],"Stopped","Complete"],"StoppedAt"->AbsoluteTime[]|>];r];
qqGaugeComparisonExit[x_] := Throw[x,qqGaugeComparisonStop];
qqGaugeCompare[expr_,reference_,seconds_,coSeconds_,tag_] := Catch[Module[{p,r,ff,rows={},pairs,co,d,den,file,key,itemKey,dir,saved,status,failed=0,alpha=qqGaugeAlpha},
 If[!MatchQ[reference,_Integer|_Rational]||!FreeQ[reference,alpha]||qqFFInvalidQ[reference],qqGaugeComparisonExit[Failure["QQGaugeReference",<||>]]];
 If[TrueQ[expr===0],qqGaugeComparisonExit[<|"Status"->"VerifiedSymbolicZero","Rows"->{},"FormFactors"-><||>,"ReferenceGauge"->reference,"GaugeSymbol"->alpha,"AllCoefficientsZero"->True,"PointTest"->False,"Statistics"-><|"Zero"->0|>,"File"->FileNameJoin[{cacheDirectory[],"qq-gauge-zero-"<>IntegerString[Hash[{tag,reference},"SHA256"],16]<>".wxf"}]|>]];
 p=<|"Amplitude"->expr,"Gauge"->alpha,"RegulatorMass"->0,"Fingerprint"->tag|>;
 r=Block[{$results=$results,FeynCalc`$KeepLogDivergentScalelessIntegrals=False},
   Module[{built=qqGaugeDifferenceBuild[p,False,coSeconds,$qqGaugeNativePV,reference]},If[FailureQ[built],built,RGZResult["QQFormFactors"]]]];
 If[!AssociationQ[r]||Lookup[r,"ComparisonSourceSHA256",Lookup[r,"InputSHA256",None]]=!=Hash[expr,"SHA256"],qqGaugeComparisonExit[Failure["QQGaugeFormFactorsIncomplete",<||>]]];ff=r;
 key=Hash[{tag,Hash[expr,"SHA256"],reference,Hash[{FileHash[FileNameJoin[{$packageRoot,"Kernel","Identities/GaugeDependence.wl"}],"SHA256"],"QQCoefficient/1",$qqGaugeNativePV},"SHA256"]},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_qq_gauge_"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 Do[pairs=ff["Rows"][[i]]["MasterCoefficients"];
 Do[qqProgressOperation[tag<>" coefficient "<>ToString[i]<>":"<>ToString[n]];
 If[!FreeQ[pairs[[n,1]],alpha],qqGaugeComparisonExit[Failure["QQGaugePVArguments",<|"Master"->pairs[[n,1]]|>]]];
 itemKey=Hash[{key,ff["Rows"][[i]]["ColorID"],ff["SpinStructures"][[ff["Rows"][[i]]["SpinID"]]],pairs[[n]]},"SHA256"];
 file=FileNameJoin[{dir,IntegerString[itemKey,16,64]<>".wxf"}];
 qqProgressUpdate[<|"Index"->n,"Total"->Length[pairs],"Master"->pairs[[n,1]],"CheckpointFile"->file|>];
 If[FileExistsQ[file],saved=Import[file,"WXF"];If[!AssociationQ[saved]||Lookup[saved,"Key",None]=!=itemKey||qqFFInvalidQ[Lookup[saved,"Value",$Failed]],qqGaugeComparisonExit[Failure["QQGaugeCheckpointInvalid",<|"File"->file|>]]];d=saved["Value"],
 co=pairs[[n,2]];
 d=TimeConstrained[qqCoefficientReduce[If[TrueQ[Lookup[ff,"GaugeDifferencePreparedBeforeReduction",False]],co,co-(co/.alpha->reference)]],coSeconds,$TimedOut];
 If[qqFFInvalidQ[d],qqGaugeComparisonExit[qqCoefficientStopDetails[d,tag,file,coSeconds]]];
 saved=atomicWXF[file,<|"Key"->itemKey,"Value"->d|>];If[FailureQ[saved],qqGaugeComparisonExit[saved]]];
 d=qqBranchCanonicalCoefficient[d];If[d=!=0,failed++];
 AppendTo[rows,<|"ColorID"->ff["Rows"][[i]]["ColorID"],"SpinID"->ff["Rows"][[i]]["SpinID"],"Master"->pairs[[n,1]],"Difference"->d,"Status"->If[d===0,"Zero","Uncancelled"]|>],{n,Length[pairs]}],{i,Length[ff["Rows"]]}];
 <|"Status"->If[failed===0,"VerifiedSymbolicZero","UncancelledCoefficients"],"Rows"->rows,"FormFactors"->ff,"ReferenceGauge"->reference,"GaugeSymbol"->alpha,"AllCoefficientsZero"->(failed===0),"PointTest"->False,"Statistics"->If[rows==={},<||>,Counts[Lookup[rows,"Status"]]],"File"->FileNameJoin[{dir,"result.wxf"}]|>],qqGaugeComparisonStop];
Options[RGZQQGaugeCheck]={"ReferenceGauge"->1,"CoefficientSeconds"->10,"ReduceTensorIntegrals"->False};
RGZQQGaugeCheck[seconds_:1200,OptionsPattern[]] := Module[{p,r,ref=OptionValue["ReferenceGauge"],co=OptionValue["CoefficientSeconds"],reduce=OptionValue["ReduceTensorIntegrals"]},
 If[!MemberQ[{True,False},reduce],Return[Failure["QQPVReductionOption",<||>]]];
 Block[{$qqGaugeNativePV=reduce},
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[co]||!TrueQ[co>0],Return[Failure["TimeLimit",<||>]]];
 $results=KeyDrop[$results,{"QQGaugeDirect"}];
 p=qqGaugePacket[];If[FailureQ[p],Return[p]];
 r=qqGaugeTimed["QQGaugeDirect",qqGaugeCompare[p["OneLoop"]/I,ref,seconds,co,"Direct"],seconds];
 If[FailureQ[r],Return[r]];r=Join[r,<|"Method"->"FullOneLoopSymbolicGauge","IRPrescription"->qqGaugeIR,"GaugeJobFingerprint"->p["GaugeJobFingerprint"],"Renormalized"->False,"GaugeIndependenceVerified"->r["AllCoefficientsZero"],"UVIRSeparated"->False|>];
 p=atomicWXF[r["File"],r];If[FailureQ[p],Return[p]];storeResult["QQGaugeDirect",r];
 <|"Status"->r["Status"],"Name"->"QQGaugeDirect","Rows"->Length[r["Rows"]],"Statistics"->r["Statistics"],"File"->r["File"]|>]];
qqGaugePropagatorDerivative[] := Module[{file,t,k,d,e,dk,dd,fail=<|"GaugeParameterOnlyBB"->False,"FullPropagatorGaugeDerivative"->False|>},
 file=FileNameJoin[{dataDirectory[],"Tree.wxf"}];If[!FileExistsQ[file],Return[fail]];
 t=Import[file,"WXF"];If[!AssociationQ[t]||Lookup[t,"Source",None]=!="FeynRulesQuadraticTerms"||Lookup[t,"ModelFingerprint",None]=!=modelKey[],Return[fail]];
 k=RGZWorkflow`Engine`rgzNormalizeKernel[t["KL"]];d=RGZWorkflow`Engine`rgzNormalizeKernel[t["DL"]];
 If[!MatrixQ[k]||!MatrixQ[d]||Dimensions[k]=!={8,8}||Dimensions[d]=!={8,8},Return[fail]];
 e=Normal[SparseArray[{{2,2}->1},{8,8}]];
 dk=Map[Cancel[Together[#]]&,D[k,RGZWorkflow`Engine`\[Alpha]]-e,{2}];
 dd=Map[Cancel[Together[#]]&,D[d,RGZWorkflow`Engine`\[Alpha]]-I d.e.d,{2}];
 <|"GaugeParameterOnlyBB"->(dk===ConstantArray[0,{8,8}]),
 "FullPropagatorGaugeDerivative"->(dd===ConstantArray[0,{8,8}])|>];
qqGaugeWardEvidence[] := Module[{derivative,props=RGZResult["Propagators"],contacts=RGZResult["PropagatorConnections"],nav=RGZResult["NAVPinch"],checks},
 derivative=TimeConstrained[qqGaugePropagatorDerivative[],60,<|"GaugeParameterOnlyBB"->False,"FullPropagatorGaugeDerivative"->False|>];
 checks=Join[derivative,<|"DdimensionalQuarkWard"->And@@qqBoxWardLineChecks[],
 "ActualPropagators"->(ListQ[props]&&Length[props]===69&&AllTrue[Join[Lookup[props,"ModelMinusMinkowski"],Lookup[props,"ModelMinusQuadraticKernel"]],TrueQ[#===0]&]),
 "MultiplierContacts"->(AssociationQ[contacts]&&Length[contacts]===8&&AllTrue[Values[contacts],TrueQ[#===0]&]),
 "FullNAVReconstruction"->TrueQ[qqPVValidNAVProof[nav,navFormulaFingerprint[]]]|>];checks];
(* Collecting the gauge difference in a common basis *)
(* Remove gauge-independent algebra before expensive PV/tensor reduction.
   This changes only comparison preparation, never physical diagram caches. *)
$qqGaugePreparationVersion=Hash[{FileHash[$InputFileName,"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/PassarinoVeltman.wl"}],"SHA256"]},"SHA256"];
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
  FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/ColorSpinBasis.wl"}],"SHA256"],
  FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/PassarinoVeltman.wl"}],"SHA256"],
  FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/PassarinoVeltman.wl"}],"SHA256"],
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

(* Ward reduction before integration *)
(* Experimental gauge difference BEFORE pole splitting and tensor integration.
   Existing physical jobs, reduced amplitudes and verification flags are untouched. *)
qqGaugeEarlyAlgorithmHash[] := Hash[FileHash[FileNameJoin[{$packageRoot,"Kernel",#}],"SHA256"]& /@
 {"Identities/GaugeDependence.wl","Algebra/FourPointMomenta.wl","Algebra/DiracChains.wl","Algebra/ColorSpinBasis.wl"},"SHA256"];
qqGaugeEarlyParts={"AV1","AV2","Box1","Box2","NAV1","NAV2"};
qqGaugeEarlyWard[input_,coSeconds_,iterate_:False] := Module[{prepared,x,parts,groups,rows={},co,r},
 prepared=qqFFSelectiveExpand[FeynCalc`FCI[input],qqFFColorHeads|qqFFSpinHeads|Dot,True];
 x=FeynCalc`SUNSimplify[FeynCalc`Contract[prepared["Masked"]],FeynCalc`SUNNToCACF->False];
 x=FeynCalc`DiracSimplify[x,FeynCalc`DiracEquation->True,FeynCalc`SirlinSimplify->False];
 x=If[TrueQ[iterate],qqFFSpinReduce[x],qqFFCanon[qqFFScalarKinematics[x]]];
 parts=qqFFSplit /@ If[Head[x]===Plus,List@@x,{x}];
 If[AnyTrue[parts,FailureQ],Return[First[Select[parts,FailureQ]]]];
 (* Contraction can move protected scalars into a color/spin factor. Restore
    the complete triple, not only its scalar coefficient. *)
 parts=parts/.prepared["Rules"];
 groups=GatherBy[parts,Take[#,2]&];
 Do[qqProgressOperation["Early Ward scalar group "<>ToString[i]<>" / "<>ToString[Length[groups]]];
 co=Total[groups[[i,All,3]]];
 r=TimeConstrained[Cancel[Together[co]],coSeconds,$TimedOut];
 If[qqFFInvalidQ[r],r=co];r=qqCoefficientCompact[co,r];If[r=!=0,AppendTo[rows,groups[[i,1,1]] groups[[i,1,2]] r]],{i,Length[groups]}];
 If[qqFFUnresolvedScalarQ[rows],Return[Failure["QQEarlyScalarRestorationIncomplete",<||>]]];
 <|"Expression"->Total[rows],"StructuralGroupsBefore"->Length[groups],"StructuralGroupsAfter"->Length[rows],
 "Identities"->{"D-dimensional Clifford algebra","Massless external Dirac equations","External scalar invariants"}|>];
qqGaugeEarlyWardInput[delta_,name_] := If[MemberQ[{"Box1","Box2"},name],
 qqBoxWardPrecondition[delta,name],delta];
Options[RGZQQGaugeEarlyPrepare]={"ReferenceGauge"->1,"Parts"->All,"CoefficientSeconds"->5};
RGZQQGaugeEarlyPrepare[seconds_:600,OptionsPattern[]] := Module[
 {j=qqGaugeValidJob[],ref=OptionValue["ReferenceGauge"],names=OptionValue["Parts"],co=OptionValue["CoefficientSeconds"],r,dir,key,file,p,raw,delta,wardInput,rows={},built},
 If[FailureQ[j],Return[j]];r=fcReady[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,{"QQGaugeEarlyPrepared","QQGaugeEarly"}];
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[co]||!TrueQ[co>0],Return[Failure["TimeLimit",<||>]]];
 If[!MatchQ[ref,_Integer|_Rational],Return[Failure["QQGaugeReference",<||>]]];
 If[names===All,names=qqGaugeEarlyParts];
 If[!ListQ[names]||names==={}||!DuplicateFreeQ[names]||!SubsetQ[qqGaugeEarlyParts,names],Return[Failure["QQEarlyParts",<|"Allowed"->qqGaugeEarlyParts|>]]];
 r=engineGet["OneLoop/QuarkGraphs.wl",False];If[FailureQ[r],Return[r]];
 key=Hash[{"QQGaugeEarly/2",j["Fingerprint"],Lookup[j,"MixedSourceHash",None],ref,qqGaugeEarlyAlgorithmHash[],$Version},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-gauge-early-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQGaugeEarlyPrepare",Catch[
 Do[file=FileNameJoin[{dir,name<>"-ward.wxf"}];
 If[FileExistsQ[file],p=Import[file,"WXF"];If[!AssociationQ[p]||Lookup[p,"Key",None]=!=key||qqFFInvalidQ[Lookup[p,"Expression",$Failed]],Throw[Failure["QQEarlyCacheInvalid",<|"File"->file|>]]],
 qqProgressOperation["Build raw gauge difference: "<>name];raw=j["Integrands"][name];
 If[MatchQ[raw,Missing["DeferredNAV",_Integer]],raw=-RGZWorkflow`Engine`rgzQQNAV[j["NAVSource"],raw[[2]]];raw=raw/.{RGZWorkflow`Engine`\[Alpha]->j["Gauge"],RGZWorkflow`Engine`M->j["RegulatorMass"]}];
 If[qqFFInvalidQ[raw],Throw[Failure["QQEarlyRawIncomplete",<|"Part"->name|>]]];
 delta=qqGaugeLinearDifference[raw,j["Gauge"],ref];
 wardInput=qqGaugeEarlyWardInput[delta,name];If[qqFFInvalidQ[wardInput],Throw[If[FailureQ[wardInput],wardInput,Failure["QQEarlyWardInputIncomplete",<|"Part"->name|>]]]];
 built=qqGaugeEarlyWard[wardInput,co,MemberQ[{"Box1","Box2"},name]];If[FailureQ[built],Throw[built]];
 built["Expression"]=qqGaugeEarlyCoordinateNormalize[built["Expression"],name,j["Gauge"],co];
 If[FailureQ[built["Expression"]],Throw[built["Expression"]]];
 p=Join[built,<|"Key"->key,"Part"->name,"RawLeaves"->LeafCount[raw],"DifferenceLeaves"->LeafCount[delta],"WardInputLeaves"->LeafCount[wardInput],"OutputLeaves"->LeafCount[built["Expression"]],"RawSHA256"->Hash[raw,"SHA256"]|>];
 built=atomicWXF[file,p];If[FailureQ[built],Throw[built]]];AppendTo[rows,p],{name,names}];True],seconds];
 If[FailureQ[r],Return[r]];If[r=!=True,Return[r]];
 p=<|"Status"->If[Sort[names]===Sort[qqGaugeEarlyParts],"IntegrandDifferencesPrepared","PartialIntegrandDifferencesPrepared"],"Parts"->names,"Rows"->rows,
 "Expression"->Total[Lookup[rows,"Expression"]],"Directory"->dir,"Fingerprint"->j["Fingerprint"],"MixedSourceHash"->Lookup[j,"MixedSourceHash",None],"AlgorithmHash"->qqGaugeEarlyAlgorithmHash[],"ReferenceGauge"->ref,"Gauge"->j["Gauge"],"GaugeIndependenceVerified"->False|>;
 file=FileNameJoin[{dir,"prepared.wxf"}];r=atomicWXF[file,p];If[FailureQ[r],Return[r]];storeResult["QQGaugeEarlyPrepared",p];
 <|"Status"->p["Status"],"Name"->"QQGaugeEarlyPrepared","Rows"->Length[rows],"Statistics"->(KeyTake[#,{"Part","RawLeaves","DifferenceLeaves","OutputLeaves","StructuralGroupsAfter"}]& /@rows),"File"->file|>];
(* Restore each distinct affine square once, rather than scanning all routes
   separately at every repeated denominator occurrence. *)
qqGaugeEarlyRestoreSquares[input_] := Module[{powers,rules},
 powers=DeleteDuplicates[Cases[input,Power[_Plus,_Integer?Negative],{0,Infinity}]];
 rules=Table[qqProgressOperation["Restore early denominator route "<>ToString[i]<>" / "<>ToString[Length[powers]]];
 powers[[i]]->qqRestoreAffineSquares[powers[[i]]],{i,Length[powers]}];
 input/.Dispatch[rules]];
(* Preserve the backend's exact pole reconstruction, but do not redo it for
   repeated denominators or merely renamed polynomial variables. *)
qqGaugeEarlyPoleSplit[input_,gauge_,reg_] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[
 {powers,rules={},q=RGZWorkflow`Engine`q,squares,var,route,poly,z,prepared,r},
 powers=DeleteDuplicates[Cases[FeynCalc`FCI[input],pow:Power[den_,n_Integer?Negative]/;
  !FreeQ[den,FeynCalc`Pair[FeynCalc`Momentum[v_,D],FeynCalc`Momentum[v_,D]]/;!FreeQ[v,q]]:>pow,{0,Infinity}]];
 Do[qqProgressOperation["Split unique early pole factor "<>ToString[i]<>" / "<>ToString[Length[powers]]];
 squares=DeleteDuplicates[Cases[powers[[i,1]],FeynCalc`Pair[FeynCalc`Momentum[v_,D],FeynCalc`Momentum[v_,D]]/;!FreeQ[v,q]:>v,{0,Infinity}]];
 If[Length[squares]=!=1,Throw[Failure["QuarkDenominatorRouting",<|"Denominator"->powers[[i,1]]|>],rgzFunctionReturnTag]];
 route=First[squares];var=Unique["RGZWorkflow`Private`earlyPole"];
 poly=powers[[i,1]]/.FeynCalc`FCI[FeynCalc`SPD[route,route]]->var;
 If[!PolynomialQ[poly,var],Throw[Failure["QuarkNonPolynomialDenominator",<||>],rgzFunctionReturnTag]];
 z=RGZWorkflow`Engine`rgzQQPoleFactor[poly^(-powers[[i,2]]),var,route,gauge,reg];If[qqFFInvalidQ[z],Throw[z,rgzFunctionReturnTag]];
 AppendTo[rules,powers[[i]]->z],{i,Length[powers]}];
 r=FeynCalc`FCI[input]/.Dispatch[rules];
 qqProgressOperation["Distribute pole denominators with scalar coefficients protected"];
 prepared=qqFFSelectiveExpand[r,_FeynCalc`FeynAmpDenominator];
 FeynCalc`FeynAmpDenominatorCombine[prepared["Masked"],FeynCalc`FCI->True]/.prepared["Rules"]],rgzFunctionReturnTag]];
(* Existing tensor stages are linear in loop-independent scalar weights.
   Protect those weights while their structural Expand calls execute. *)
qqGaugeEarlyProtectedApply[fn_,input_] := Module[{p},
 p=qqFFSelectiveExpand[input,RGZWorkflow`Engine`q|qqFFColorHeads|qqFFSpinHeads|_FeynCalc`FeynAmpDenominator];
 fn[p["Masked"]]/.p["Rules"]];
qqGaugeEarlyPieces[input_] := Module[{terms,parts,groups,out={},sum,r,q=RGZWorkflow`Engine`q},
 terms=If[Head[input]===Plus,List@@input,{input}];
 parts=Function[term,Module[{f=If[Head[term]===Times,List@@term,{term}],scalar,co,core,bound},
 bound=DeleteDuplicates[Cases[Select[f,!FreeQ[#,q]&],_FeynCalc`LorentzIndex,Infinity]];
 scalar=Select[f,FreeQ[#,q]&&Intersection[DeleteDuplicates[Cases[#,_FeynCalc`LorentzIndex,Infinity]],bound]==={}&];
 co=Times@@scalar;core=Times@@Complement[f,scalar];
 If[co core=!=term,Return[Failure["QQTensorFactorization",<||>]]];{co,core}]] /@terms;
 If[AnyTrue[parts,FailureQ],Return[First[Select[parts,FailureQ]]]];
 groups=GatherBy[parts,Last];
 Do[qqProgressOperation["Early tensor scalar group "<>ToString[i]<>" / "<>ToString[Length[groups]]];
 sum=Total[groups[[i,All,1]]];r=TimeConstrained[Cancel[Together[sum]],2,$TimedOut];
 If[qqFFInvalidQ[r],r=sum];r=qqCoefficientCompact[sum,r];
 If[r=!=0,AppendTo[out,{r,groups[[i,1,2]]}]],{i,Length[groups]}];out];
SetAttributes[qqGaugeEarlyWithProtectedAlgebra,HoldAll];
qqGaugeEarlyWithProtectedAlgebra[body_] := Module[{linear,apart,cancel},
 Internal`WithLocalSettings[
 DownValues[linear]=DownValues[qqLinearizeChains]/.qqLinearizeChains->linear;
 DownValues[apart]=DownValues[qqRouteApart]/.qqRouteApart->apart;
 DownValues[cancel]=DownValues[qqCancelRoutePowers]/.qqCancelRoutePowers->cancel,
 Internal`InheritedBlock[{qqLinearizeChains,qqRouteApart,qqCancelRoutePowers,qqTensorPieces},
 Clear[qqLinearizeChains,qqRouteApart,qqCancelRoutePowers,qqTensorPieces];
 qqLinearizeChains[x_]:=qqGaugeEarlyProtectedApply[linear,x];
 qqRouteApart[x_]:=qqGaugeEarlyProtectedApply[apart,x];
 qqCancelRoutePowers[x_]:=qqGaugeEarlyProtectedApply[cancel,x];
 qqTensorPieces[x_]:=qqGaugeEarlyPieces[x];body],
 Clear[linear,apart,cancel]]];
qqGaugeEarlyIntegratedRemainder[j_,ref_] := Module[{measure=qqDeclaredLoopMeasure[j]},
 If[qqFFInvalidQ[measure]||TrueQ[measure===0],Return[Failure["QQMeasureConventionRequired",<||>]]];
 qqGaugeLinearDifference[measure j["MixedSelfEnergy"]+2 j["OnShellSigmaV"] j["Tree"],j["Gauge"],ref]];
(* Shift complete tensor cores, including their numerators, before the second
   grouping pass. Scalar weights are loop independent by qqTensorPieces. *)
qqGaugeEarlyRouteInput[input_] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{r,pieces,anchored={},again},
 qqProgressOperation["Early common routes: linearize protected chains"];
 r=qqLinearizeChains[input];qqProgressUpdate[<|"OutputLeaves"->LeafCount[r]|>];
 qqProgressOperation["Early common routes: combine protected denominators"];
 r=qqGaugeEarlyProtectedApply[Function[x,FeynCalc`FeynAmpDenominatorCombine[x,FeynCalc`FCI->True]],r];
 qqProgressOperation["Early common routes: partial fractions and cancel route squares"];
 r=qqCancelRoutePowers[qqRouteApart[r]];
 pieces=qqTensorPieces[r];If[FailureQ[pieces],Return[pieces]];
 Do[r=qqTensorAnchor[piece[[2]]];If[FailureQ[r],Throw[r,rgzFunctionReturnTag]];AppendTo[anchored,piece[[1]] r],{piece,pieces}];
 again=qqTensorPieces[Total[anchored]];If[FailureQ[again],Return[again]];
 qqProgressUpdate[<|"EarlyRouteCoresBefore"->Length[pieces],"EarlyRouteCoresAfter"->Length[again]|>];
 Total[Times@@@again]],rgzFunctionReturnTag]];
(* Reuse the existing checkpointed TID driver, protecting its denominator
   combination too. This private copy changes no physical part definitions. *)
DownValues[qqGaugeEarlyReduceGroups]=DownValues[qqReduceTensorGroups]/.
 qqReduceTensorGroups->qqGaugeEarlyReduceGroups;
DownValues[qqGaugeEarlyReduceGroups]=DownValues[qqGaugeEarlyReduceGroups]/.
 HoldPattern[FeynCalc`FeynAmpDenominatorCombine[x_,FeynCalc`FCI->True]]:>
 qqGaugeEarlyProtectedApply[Function[z,FeynCalc`FeynAmpDenominatorCombine[z,FeynCalc`FCI->True]],x];
Options[RGZQQGaugeEarlyCheck]={"ReferenceGauge"->1,"CoefficientSeconds"->10,"TensorSeconds"->600,"ReduceTensorIntegrals"->True};
RGZQQGaugeEarlyCheck[seconds_:1200,OptionsPattern[]] := Module[
 {j=qqGaugeValidJob[],p=RGZResult["QQGaugeEarlyPrepared"],ref=OptionValue["ReferenceGauge"],co=OptionValue["CoefficientSeconds"],ts=OptionValue["TensorSeconds"],reduce=OptionValue["ReduceTensorIntegrals"],r,input,reduced,delta,job,file,key},
 If[FailureQ[j],Return[j]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[!MemberQ[{True,False},reduce],Return[Failure["QQPVReductionOption",<||>]]];
 If[!And@@(NumericQ[#]&&TrueQ[#>0]& /@{seconds,co,ts}),Return[Failure["TimeLimit",<||>]]];
  $results=KeyDrop[$results,{"QQGaugeEarly"}];
 If[!AssociationQ[p]||qqFFUnresolvedScalarQ[Lookup[p,"Expression",0]]||Lookup[p,"AlgorithmHash",None]=!=qqGaugeEarlyAlgorithmHash[]||Lookup[p,"Fingerprint",None]=!=j["Fingerprint"]||Lookup[p,"MixedSourceHash",None]=!=Lookup[j,"MixedSourceHash",None]||Lookup[p,"ReferenceGauge",None]=!=ref||Sort[Lookup[p,"Parts",{}]]=!=Sort[qqGaugeEarlyParts],Return[Failure["QQEarlyPrepareAllFirst",<|"Action"->"RGZQQGaugeEarlyPrepare[]"|>]]];
 If[!TrueQ[Lookup[j,"SelfPrepared",False]],Return[Failure["QQPrepareSelfFirst",<||>]]];
 r=engineGet["OneLoop/QuarkGraphs.wl",False];If[FailureQ[r],Return[r]];
 job=Join[j,<|"Directory"->p["Directory"]|>];
 r=qqGaugeTimed["QQGaugeEarlyCheck",Catch[
 qqProgressOperation["Split poles of combined raw gauge difference"];
 input=qqGaugeEarlyRestoreSquares[p["Expression"]];
 input=qqGaugeEarlyPoleSplit[input,j["Gauge"],0];If[qqFFInvalidQ[input],Throw[Failure["QQEarlySplitIncomplete",<|"Cause"->input|>]]];
 input=qqGaugeEarlyWithProtectedAlgebra[qqGaugeEarlyRouteInput[input]];If[qqFFInvalidQ[input],Throw[Failure["QQEarlyRoutesIncomplete",<||>]]];
 reduced=qqGaugeEarlyWithProtectedAlgebra[qqGaugeEarlyReduceGroups[input,job,"GaugeDifference",ts]];If[qqFFInvalidQ[reduced],Throw[If[FailureQ[reduced],reduced,Failure["QQEarlyTIDIncomplete",<||>]]]];
 reduced=RGZWorkflow`Engine`rgzQQRestorePoleMasses[reduced,j["Gauge"],0] j["LoopMeasureConversion"];
 delta=qqGaugeEarlyIntegratedRemainder[j,ref];If[qqFFInvalidQ[delta],Throw[Failure["QQEarlySelfIncomplete",<|"Cause"->delta|>]]];
 delta=(reduced+delta)/I;
 Block[{$qqGaugeNativePV=reduce},qqGaugeCompare[delta,ref,seconds,co,"EarlyIntegrandDirect"]]],seconds];
 If[FailureQ[r],Return[r]];If[!AssociationQ[r],Return[Failure["QQEarlyCheckIncomplete",<||>]]];
 r=Join[r,<|"Method"->"RawIntegrandGaugeDifferenceBeforeTID","GaugeJobFingerprint"->j["Fingerprint"],"MixedSourceHash"->Lookup[j,"MixedSourceHash",None],"ModelFingerprint"->Lookup[j,"ModelFingerprint",None],"ModelContent"->Lookup[j,"ModelContent",None],"IRPrescription"->qqGaugeIR,
 "GaugeIndependenceVerified"->TrueQ[r["AllCoefficientsZero"]],"Renormalized"->False,"UVIRSeparated"->False|>];
 file=FileNameJoin[{p["Directory"],"comparison.wxf"}];AssociateTo[r,"File"->file];key=atomicWXF[file,r];If[FailureQ[key],Return[key]];storeResult["QQGaugeEarly",r];
 <|"Status"->r["Status"],"Name"->"QQGaugeEarly","Rows"->Length[r["Rows"]],"Statistics"->r["Statistics"],"File"->file|>];
