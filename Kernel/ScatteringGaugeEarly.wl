(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Experimental gauge difference BEFORE pole splitting and tensor integration.
   Existing physical jobs, reduced amplitudes and verification flags are untouched. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringGaugeCoordinates.wl"}]];
qqGaugeEarlyAlgorithmHash[] := Hash[FileHash[FileNameJoin[{$packageRoot,"Kernel",#}],"SHA256"]& /@
 {"ScatteringGaugeEarly.wl","ScatteringGaugeCoordinates.wl","ScatteringStructuralExpand.wl","ScatteringFormFactors.wl"},"SHA256"];
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
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
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
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
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
