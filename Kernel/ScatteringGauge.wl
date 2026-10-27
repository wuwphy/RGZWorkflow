(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Independent gauge-verification jobs. Does not change physical QQ fingerprints. *)
(* The private gauge symbol carries the verification algorithm version in its
   context, so existing physical QQ fingerprints isolate all new IR jobs. *)
qqGaugeAlpha = Symbol["RGZWorkflow`Gauge`v"<>StringTake[IntegerString[
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringGauge.wl"}],"SHA256"],16,64],12]<>"`alpha"];
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
 r=loadPTEngine[];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 sigma=engineEvaluation[RGZWorkflow`Engine`rgzQQSigmaV0[0,0]];
 If[qqFFInvalidQ[sigma],Return[Failure["QQResidueIncomplete",<||>]]];
 (* This is a separately declared IR prescription, not a change to the default
    finite-M residue. The massless longitudinal on-shell integral is scaleless.
    Avoid backend redefinition inside QQStart while the override is in scope. *)
 r=Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=False},
 Internal`InheritedBlock[{RGZWorkflow`Engine`rgzQQSigmaV0,engineGet},
 engineGet["RGZ_Quark_Amplitude.wl",False]:=Null;
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
 Failure["QQGaugeAssembleFirst",<|"Action"->"RGZQQGaugeAssemble[]"|>],p]];
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
   Module[{built=qqFFBuild[p,False,coSeconds,False]},If[FailureQ[built],built,RGZResult["QQFormFactors"]]]];
 If[!AssociationQ[r]||Lookup[r,"InputSHA256",None]=!=Hash[expr,"SHA256"],qqGaugeComparisonExit[Failure["QQGaugeFormFactorsIncomplete",<||>]]];ff=r;
 key=Hash[{tag,Hash[expr,"SHA256"],reference,FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringGauge.wl"}],"SHA256"]},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_qq_gauge_"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 Do[pairs=ff["Rows"][[i]]["MasterCoefficients"];
 Do[qqProgressOperation[tag<>" coefficient "<>ToString[i]<>":"<>ToString[n]];
 If[!FreeQ[pairs[[n,1]],alpha],qqGaugeComparisonExit[Failure["QQGaugePVArguments",<|"Master"->pairs[[n,1]]|>]]];
 itemKey=Hash[{key,ff["Rows"][[i]]["ColorID"],ff["SpinStructures"][[ff["Rows"][[i]]["SpinID"]]],pairs[[n]]},"SHA256"];
 file=FileNameJoin[{dir,IntegerString[itemKey,16,64]<>".wxf"}];
 qqProgressUpdate[<|"Index"->n,"Total"->Length[pairs],"Master"->pairs[[n,1]],"CheckpointFile"->file|>];
 If[FileExistsQ[file],saved=Import[file,"WXF"];If[!AssociationQ[saved]||Lookup[saved,"Key",None]=!=itemKey||qqFFInvalidQ[Lookup[saved,"Value",$Failed]],qqGaugeComparisonExit[Failure["QQGaugeCheckpointInvalid",<|"File"->file|>]]];d=saved["Value"],
 co=pairs[[n,2]];
 d=TimeConstrained[Cancel[Together[co-(co/.alpha->reference)]],coSeconds,$TimedOut];
 If[qqFFInvalidQ[d],qqGaugeComparisonExit[Failure["QQGaugeCoefficientStopped",<|"Stage"->tag,"Action"->"Repeat with the desired CoefficientSeconds; completed checkpoints are reused.","File"->file|>]]];
 saved=atomicWXF[file,<|"Key"->itemKey,"Value"->d|>];If[FailureQ[saved],qqGaugeComparisonExit[saved]]];
 If[d=!=0,failed++];
 AppendTo[rows,<|"ColorID"->ff["Rows"][[i]]["ColorID"],"SpinID"->ff["Rows"][[i]]["SpinID"],"Master"->pairs[[n,1]],"Difference"->d,"Status"->If[d===0,"Zero","Uncancelled"]|>],{n,Length[pairs]}],{i,Length[ff["Rows"]]}];
 <|"Status"->If[failed===0,"VerifiedSymbolicZero","UncancelledCoefficients"],"Rows"->rows,"FormFactors"->ff,"ReferenceGauge"->reference,"GaugeSymbol"->alpha,"AllCoefficientsZero"->(failed===0),"PointTest"->False,"Statistics"->Counts[Lookup[rows,"Status"]],"File"->FileNameJoin[{dir,"result.wxf"}]|>],qqGaugeComparisonStop];
Options[RGZQQGaugeCheck]={"ReferenceGauge"->1,"CoefficientSeconds"->10};
RGZQQGaugeCheck[seconds_:1200,OptionsPattern[]] := Module[{p,r,ref=OptionValue["ReferenceGauge"],co=OptionValue["CoefficientSeconds"]},
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[co]||!TrueQ[co>0],Return[Failure["TimeLimit",<||>]]];
 $results=KeyDrop[$results,{"QQGaugeDirect"}];
 p=qqGaugePacket[];If[FailureQ[p],Return[p]];
 r=qqGaugeTimed["QQGaugeDirect",qqGaugeCompare[p["OneLoop"]/I,ref,seconds,co,"Direct"],seconds];
 If[FailureQ[r],Return[r]];r=Join[r,<|"Method"->"FullOneLoopSymbolicGauge","IRPrescription"->qqGaugeIR,"GaugeJobFingerprint"->p["GaugeJobFingerprint"],"Renormalized"->False,"GaugeIndependenceVerified"->r["AllCoefficientsZero"],"UVIRSeparated"->False|>];
 p=atomicWXF[r["File"],r];If[FailureQ[p],Return[p]];storeResult["QQGaugeDirect",r];
 <|"Status"->r["Status"],"Name"->"QQGaugeDirect","Rows"->Length[r["Rows"]],"Statistics"->r["Statistics"],"File"->r["File"]|>];
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
 r=require[qqSelfCheckpoint[dir,"assemble-"<>ToString[i]<>"-"<>ToString[j],key,Factor[Cancel[Together[rows[[j,2]]]]],seconds]];
 If[i<3&&r=!=0,Throw[Failure["QQPinchM0Divergent",<|"Power"->2 i-6,"Master"->rows[[j,1]]|>],stop]];
 AppendTo[values,rows[[j,1]] r],{j,Length[rows]}];
 r=require[qqSelfCheckpoint[dir,"sum-"<>ToString[i],key,Total[values],seconds]];
 If[i===3,finite=r],{i,3}];
 rows=require[qqSelfCheckpoint[dir,"restore-pairs",key,RGZWorkflow`Engine`RGZGaugePVPairs[finite],seconds]];
 Do[AppendTo[restored,require[qqSelfCheckpoint[dir,"restore-"<>ToString[i],key,qqRestoreProgress[Times@@rows[[i]]],seconds]]],{i,Length[rows]}];
 Total[restored],stop]];
qqGaugePinchAmplitude[seconds_] := engineEvaluation@Module[{p,j=qqGaugeValidJob[],work,expected,scalar,r,file,key},
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
 RGZWorkflow`Engine`RGZPTColour[p["CandidateSumT"]]],key,file<>"-parts",seconds];If[qqFFInvalidQ[scalar],Return[Failure["QQPinchM0Incomplete",<|"Cause"->scalar|>]]];
 r=FeynCalc`Contract[FeynCalc`FCI[-FeynCalc`SMP["g_s"]^2/I
 RGZWorkflow`Engine`rgzQQBornCurrent[RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu]
 RGZWorkflow`Engine`RGZLoopPT[RGZWorkflow`Engine`p,RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu] scalar]];
 r=r/.{RGZWorkflow`Engine`p->RGZWorkflow`Engine`qqK3-RGZWorkflow`Engine`qqK1,RGZWorkflow`Engine`\[Alpha]->qqGaugeAlpha};
 r=qqFFSpinReduce[r];If[qqFFInvalidQ[r],Return[Failure["QQPinchAttachmentIncomplete",<||>]]];
 scalar=atomicWXF[file,<|"Key"->key,"Value"->r|>];If[FailureQ[scalar],scalar,r]];
Options[RGZQQWardCoverage]={"ReferenceGauge"->1,"CoefficientSeconds"->10};
RGZQQWardCoverage[seconds_:1200,OptionsPattern[]] := Module[{p,r,evidence,pinch,bridge,zero,ref=OptionValue["ReferenceGauge"],co=OptionValue["CoefficientSeconds"],file},
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
 Join[KeyTake[r,{"Status","Name","File"}],<|"Statistics"->KeyTake[r,{"CoverageVerified","PinchDifferenceZero","GaugeIndependenceVerified"}]|>]];
