(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Express finite coefficients in compact pole coordinates and extract common factors.

   Inputs:
   Symbolic D-dimensional tensors, scalar masters and their collected coefficients.

   Dependencies:
   FeynCalc for tensor/PV expressions; Internal/ supplies progress and persistence.
*)


(* Finite coefficient coordinates *)
(* Separate exact coefficient presentation; source finite audits are immutable. *)
$scalarCompactSHA256=FileHash[$InputFileName,"SHA256"];
qqScalarCompactRational[input_] := Module[{z,nd},
 z=qqScalarAuditExact[input];
 (* Expanding the denominator separately exposes mG^4 hidden in products
    of factors containing mG^2. No square-root branch is changed. *)
 nd=qqScalarAuditCoordinates[{{},{Expand[Numerator[z]],Expand[Denominator[z]]}}][[2]];
 Factor[Cancel[Together[nd[[1]]/nd[[2]]]]]];
qqScalarCompactHorner[c_] := Module[{z=Together[c],h},
 h=HornerForm[Expand[Numerator[z]]]/HornerForm[Expand[Denominator[z]]];
 If[LeafCount[h]<LeafCount[c]&&TrueQ[Cancel[Together[h-c]]===0],h,c]];
qqScalarCompactFunction[f_] := Module[{z=qqScalarAuditCoordinates[{{},{f}}][[2,1]]},
 If[LeafCount[z]<LeafCount[f],z,f]];
qqScalarCompactPairs[f_] := Module[{atoms,vars,z,pairs},
 atoms=DeleteDuplicates[Cases[f,_RGZPVFinite|_Log,Infinity]];
 vars=Table[Unique["finiteFunction"],{Length[atoms]}];
 z=Expand[f/.Dispatch[Thread[atoms->vars]]];
 If[!PolynomialQ[z,vars],Return[Failure["ScalarFiniteNonlinearFunctions",<||>]]];
 pairs=Join[{{1,z/.Thread[vars->0]}},MapThread[List,{atoms,Coefficient[z,#]&/@vars}]];
 If[Expand[z-Total[MapThread[Times,{Prepend[vars,1],Last/@pairs}]]]=!=0,
 Return[Failure["ScalarFiniteNonlinearFunctions",<||>]]];
 Select[pairs,Last[#]=!=0&]];
Options[RGZQQScalarFiniteSimplify]={"File"->Automatic,"CoefficientSeconds"->30,"CompactFunctionArguments"->True,"HornerCoefficients"->True};
RGZQQScalarFiniteSimplify[seconds_:3600,OptionsPattern[]] := Module[
 {p,r,co=OptionValue["CoefficientSeconds"],args=OptionValue["CompactFunctionArguments"],horner=OptionValue["HornerCoefficients"],key,dir,rows={},pairs,compact,z,out,defs,stop=Unique["compact"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,co}),Return[Failure["TimeLimit",<||>]]];
 If[!MemberQ[{True,False},args]||!MemberQ[{True,False},horner],Return[Failure["ScalarCompactOptions",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 p=qqAuditRead[OptionValue["File"],"QQScalarFiniteAudit"];
 If[!AssociationQ[p]||!TrueQ[Lookup[p,"FiniteAmplitudeEquivalenceVerified",False]],Return[Failure["ScalarFiniteAuditRequired",<||>]]];
 key=Hash[{"ScalarFiniteCompact/2",FileHash[p["File"],"SHA256"],$scalarCompactSHA256,$scalarAuditSHA256,args,horner},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-scalar-finite-compact-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQScalarFiniteSimplify",Catch[
 Do[pairs=qqScalarCompactPairs[p["Rows"][[n]]["FiniteFormFactor"]];If[FailureQ[pairs],Throw[pairs,stop]];compact={};
 Do[qqProgressUpdate[<|"Stage"->"QQScalarFiniteSimplify","Index"->n,"Total"->Length[p["Rows"]],"Master"->pairs[[k,1]],"AlgebraGroupIndex"->k,"AlgebraGroupTotal"->Length[pairs],"InputLeaves"->LeafCount[pairs[[k,2]]],"StepLimitSeconds"->co|>];
 qqProgressOperation["Cancel finite coefficient in exact squared-pole coordinates"];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"row-"<>ToString[n]<>"-co-"<>ToString[k]<>".wxf"}],Hash[{key,n,k},"SHA256"],
 Module[{v=qqScalarCompactRational[pairs[[k,2]]],delta},
 If[TrueQ[horner],v=qqScalarCompactHorner[v]];
 delta=qqScalarCompactRational[pairs[[k,2]]-v];
 If[delta=!=0,Failure["ScalarCompactCoefficientMismatch",<|"Difference"->delta|>],<|"Coefficient"->v,"Verified"->True,"InputLeaves"->LeafCount[pairs[[k,2]]],"OutputLeaves"->LeafCount[v]|>]],co];
 If[FailureQ[z],Throw[z,stop]];AppendTo[compact,{If[TrueQ[args],qqScalarCompactFunction[pairs[[k,1]]],pairs[[k,1]]],z["Coefficient"]}],{k,Length[pairs]}];
 AppendTo[rows,Join[KeyTake[p["Rows"][[n]],{"ColorID","SpinID"}],<|"FiniteFunctionCoefficients"->Select[compact,Last[#]=!=0&],"FiniteFormFactor"->Total[Times@@#&/@compact],"ExactCoefficientEquivalenceVerified"->True,"InputLeaves"->LeafCount[p["Rows"][[n]]["FiniteFormFactor"]]|>]],{n,Length[p["Rows"]]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 defs={RGZWorkflow`ScalarAudit`rMinus->(RGZWorkflow`Engine`m^2+RGZWorkflow`Engine`\[Mu]^2)/2-I Sqrt[RGZWorkflow`Engine`mG^4-(RGZWorkflow`Engine`m^2-RGZWorkflow`Engine`\[Mu]^2)^2/4],
 RGZWorkflow`ScalarAudit`rPlus->(RGZWorkflow`Engine`m^2+RGZWorkflow`Engine`\[Mu]^2)/2+I Sqrt[RGZWorkflow`Engine`mG^4-(RGZWorkflow`Engine`m^2-RGZWorkflow`Engine`\[Mu]^2)^2/4],
 RGZWorkflow`ScalarAudit`mu2->RGZWorkflow`Engine`\[Mu]^2,RGZWorkflow`ScalarAudit`s->RGZWorkflow`Engine`qqS,RGZWorkflow`ScalarAudit`t->RGZWorkflow`Engine`qqT};
 out=Join[KeyTake[p,{"SourceFingerprint","InputSHA256","ColorBasis","SpinStructures","TreeAmplitude","RenormalizationScaleSquared","Conditions"}],
 <|"Status"->"ScalarFiniteCoefficientsSimplified","Key"->key,"Rows"->rows,"CoordinateDefinitions"->defs,
 "InputLeaves"->LeafCount[p["OneLoopFiniteAmplitude"]],"ExactCoefficientEquivalenceVerified"->True,
 "SourceFiniteAuditFile"->p["File"],"SourceFiniteAuditSHA256"->FileHash[p["File"],"SHA256"],
 "CompactFunctionArguments"->args,"HornerCoefficients"->horner,"AdditionalIntegralIdentitiesUsed"->False,
 "Scope"->"Exact rational coefficient simplification on the source audit's selected pole branch and domain. Optional function-argument coordinate rewriting preserves parameter values; no new integral identities or physical certificate.",
 "File"->FileNameJoin[{dir,"compact.wxf"}]|>];
 AssociateTo[out,"OneLoopFiniteAmplitude"->Total[(p["ColorBasis"][[#["ColorID"]]]p["SpinStructures"][[#["SpinID"]]]#["FiniteFormFactor"])&/@rows]];
 AssociateTo[out,{"Amplitude"->p["TreeAmplitude"]+out["OneLoopFiniteAmplitude"],"OutputLeaves"->LeafCount[out["OneLoopFiniteAmplitude"]]}];
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQScalarFiniteCompact",out];KeyTake[out,{"Status","InputLeaves","OutputLeaves","ExactCoefficientEquivalenceVerified","File"}]];

(* Search polynomial presentation orders without changing variables or integrals. *)
qqScalarCompactPolynomial[c_] := Module[{z=Together[c],num,den,candidates={c},v,h,best},
 num=Numerator[z];den=Factor[Denominator[z]];AppendTo[candidates,Factor[c]];
 Do[h=TimeConstrained[HornerForm[Expand[num],v]/den,1,$Failed];If[h=!=$Failed,AppendTo[candidates,h]];
 h=TimeConstrained[Collect[num,v,Factor]/den,1,$Failed];If[h=!=$Failed,AppendTo[candidates,h]],
 {v,{RGZWorkflow`ScalarAudit`s,RGZWorkflow`ScalarAudit`t,RGZWorkflow`ScalarAudit`rMinus,RGZWorkflow`ScalarAudit`rPlus,RGZWorkflow`ScalarAudit`mu2,FeynCalc`SUNN}}];
 best=First[MinimalBy[candidates,LeafCount]];
 If[TrueQ[Cancel[Together[best-c]]===0],best,Failure["ScalarPolynomialMismatch",<||>]]];
qqScalarSingletOctet[c1_,c2_] := Module[{sing,oct,checks},
 sing=qqScalarCompactPolynomial[qqScalarCompactRational[c1+c2/FeynCalc`SUNN]];
 oct=qqScalarCompactPolynomial[2c2];
 If[FailureQ[sing]||FailureQ[oct],Return[Failure["ScalarColorCoefficientMismatch",<||>]]];
 checks=qqScalarCompactRational/@{sing-oct/(2FeynCalc`SUNN)-c1,oct/2-c2};
 If[checks==={0,0},<|"Singlet"->sing,"Octet"->oct,"Verified"->True|>,Failure["ScalarColorCoefficientMismatch",<|"Differences"->checks|>]]];
Options[RGZQQScalarColorSimplify]={"File"->Automatic,"CoefficientSeconds"->30};
RGZQQScalarColorSimplify[seconds_:3600,OptionsPattern[]] := Module[
 {p,r,co=OptionValue["CoefficientSeconds"],key,dir,spins,rows={},r1,r2,p1,p2,masters,sing,oct,z,basis,out,n,counts={},stop=Unique["colorCompact"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,co}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 p=qqAuditRead[OptionValue["File"],"QQScalarFiniteCompact"];
 If[!AssociationQ[p]||!TrueQ[Lookup[p,"ExactCoefficientEquivalenceVerified",False]]||!ListQ[Lookup[p,"Rows",None]],Return[Failure["ScalarFiniteCompactRequired",<||>]]];
 If[p["ColorBasis"]=!=qqFFColors[],Return[Failure["ScalarColorBasisUnsupported",<||>]]];
 spins=Sort[DeleteDuplicates[Lookup[p["Rows"],"SpinID"]]];
 If[!AllTrue[spins,Function[j,Sort[Lookup[Select[p["Rows"],#["SpinID"]===j&],"ColorID"]]==={1,2}]],Return[Failure["ScalarColorRowsIncomplete",<||>]]];
 key=Hash[{"ScalarColorCompact/1",FileHash[p["File"],"SHA256"],$scalarCompactSHA256,$scalarAuditSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-scalar-color-compact-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQScalarColorSimplify",Catch[
 Do[n=j;r1=SelectFirst[p["Rows"],#["SpinID"]===j&&#["ColorID"]===1&];r2=SelectFirst[p["Rows"],#["SpinID"]===j&&#["ColorID"]===2&];
 p1=Merge[Association[Rule@@#]&/@r1["FiniteFunctionCoefficients"],Total];p2=Merge[Association[Rule@@#]&/@r2["FiniteFunctionCoefficients"],Total];
 masters=Union[Keys[p1],Keys[p2]];sing={};oct={};
 Do[qqProgressUpdate[<|"Stage"->"QQScalarColorSimplify","Index"->j,"Total"->Length[spins],"Master"->masters[[k]],"AlgebraGroupIndex"->k,"AlgebraGroupTotal"->Length[masters],"InputLeaves"->LeafCount[{Lookup[p1,masters[[k]],0],Lookup[p2,masters[[k]],0]}],"StepLimitSeconds"->co|>];
 qqProgressOperation["Recombine singlet/octet coefficients and search polynomial orders"];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"spin-"<>ToString[j]<>"-co-"<>ToString[k]<>".wxf"}],Hash[{key,j,k},"SHA256"],
 qqScalarSingletOctet[Lookup[p1,masters[[k]],0],Lookup[p2,masters[[k]],0]],co];
 If[FailureQ[z],Throw[z,stop]];
 If[z["Singlet"]=!=0,AppendTo[sing,{masters[[k]],z["Singlet"]}]];If[z["Octet"]=!=0,AppendTo[oct,{masters[[k]],z["Octet"]}]],{k,Length[masters]}];
 AppendTo[counts,<|"SpinID"->j,"InputEntries"->Length[p1]+Length[p2],"SingletEntries"->Length[sing],"OctetEntries"->Length[oct]|>];
 Do[AppendTo[rows,<|"ColorID"->i,"SpinID"->j,"FiniteFunctionCoefficients"->If[i===1,sing,oct],"FiniteFormFactor"->Total[Times@@#&/@If[i===1,sing,oct]],"ColorReconstructionVerified"->True|>],{i,2}],{j,spins}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 basis={p["ColorBasis"][[1]],(p["ColorBasis"][[2]]-p["ColorBasis"][[1]]/FeynCalc`SUNN)/2};
 out=Join[KeyTake[p,{"SourceFingerprint","InputSHA256","SpinStructures","TreeAmplitude","RenormalizationScaleSquared","Conditions","CoordinateDefinitions","SourceFiniteAuditFile"}],
 <|"Status"->"ScalarSingletOctetSimplified","Key"->key,"Rows"->rows,"ColorBasis"->basis,"ColorBasisLabels"->{"t-channel singlet","t-channel octet"},
 "FormFactorTransformation"->{{1,1/FeynCalc`SUNN},{0,2}},"ColorReconstructionVerified"->True,"ExactCoefficientEquivalenceVerified"->True,
 "InputLeaves"->LeafCount[p["OneLoopFiniteAmplitude"]],"EntryCounts"->counts,"SourceCompactFile"->p["File"],"SourceCompactSHA256"->FileHash[p["File"],"SHA256"],
 "Scope"->"Exact SU(N) color change Csinglet=Cdirect, Coctet=(Ccrossed-Cdirect/N)/2, with Tr(Ta Tb)=delta_ab/2. Rational coefficient and inverse-color reconstruction checked per finite function. No spin, dimension, integral or physical-domain assumptions added.",
 "File"->FileNameJoin[{dir,"color-compact.wxf"}]|>];
 AssociateTo[out,"OneLoopFiniteAmplitude"->Total[(basis[[#["ColorID"]]]p["SpinStructures"][[#["SpinID"]]]#["FiniteFormFactor"])&/@rows]];
 AssociateTo[out,{"Amplitude"->p["TreeAmplitude"]+out["OneLoopFiniteAmplitude"],"OutputLeaves"->LeafCount[out["OneLoopFiniteAmplitude"]]}];
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQScalarColorCompact",out];
 KeyTake[out,{"Status","InputLeaves","OutputLeaves","ColorReconstructionVerified","File"}]];

(* Common finite prefactors *)
(* Exact common scalar factors of existing singlet/octet finite rows.
   Never changes the spin basis or source finite/certification reports. *)
$scalarPrefactorsSHA256=FileHash[$InputFileName,"SHA256"];
qqScalarRowPrefactor[pairs_] := Module[{c,g,reduced,checks,input,candidate},
 If[!ListQ[pairs]||!AllTrue[pairs,MatchQ[#,{_,_}]&],Return[Failure["ScalarPrefactorPairs",<||>]]];
 If[pairs==={},Return[<|"ScalarPrefactor"->1,"ReducedFiniteFunctionCoefficients"->{},"FiniteFormFactor"->0,"PrefactorReconstructionVerified"->True|>]];
 c=Together/@pairs[[All,2]];
 g=TimeConstrained[Factor[(PolynomialGCD@@(Numerator/@c))/(PolynomialGCD@@(Denominator/@c))],5,$TimedOut];
 If[g===$TimedOut,g=1];
 If[qqFFInvalidQ[g]||g===0,Return[Failure["ScalarPrefactorInvalid",<||>]]];
 reduced=Map[{First[#],qqScalarCompactPolynomial[Cancel[Together[Last[#]/g]]]}&,pairs];
 If[qqFFInvalidQ[reduced],Return[Failure["ScalarPrefactorInvalid",<||>]]];
 checks=MapThread[TrueQ[Cancel[Together[g #1-#2]]===0]&,{Last/@reduced,Last/@pairs}];
 If[!And@@checks,Return[Failure["ScalarPrefactorReconstruction",<||>]]];
 input=Total[Times@@#&/@pairs];candidate=g Total[Times@@#&/@reduced];
 If[LeafCount[candidate]>=LeafCount[input],g=1;reduced=pairs;candidate=input];
 <|"ScalarPrefactor"->g,"ReducedFiniteFunctionCoefficients"->reduced,"FiniteFormFactor"->candidate,
 "PrefactorReconstructionVerified"->True,"CheckedCoefficients"->Length[pairs]|>];
Options[RGZQQScalarFactorRows]={"File"->Automatic,"RowSeconds"->120};
RGZQQScalarFactorRows[seconds_:1800,OptionsPattern[]] := Module[
 {p,r,limit=OptionValue["RowSeconds"],key,dir,rows={},z,out,stop=Unique["scalarPrefactor"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,limit}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 p=qqAuditRead[OptionValue["File"],"QQScalarColorCompact"];
 If[!AssociationQ[p]||!TrueQ[Lookup[p,"ColorReconstructionVerified",False]]||!TrueQ[Lookup[p,"ExactCoefficientEquivalenceVerified",False]],Return[Failure["ScalarColorCompactRequired",<||>]]];
 key=Hash[{"ScalarPrefactors/1",FileHash[p["File"],"SHA256"],$scalarPrefactorsSHA256,$scalarCompactSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-scalar-factored-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQScalarFactorRows",Catch[
 Do[qqProgressUpdate[<|"Stage"->"QQScalarFactorRows","Index"->n,"Total"->Length[p["Rows"]],"InputLeaves"->LeafCount[p["Rows"][[n]]["FiniteFormFactor"]],"StepLimitSeconds"->limit|>];
 qqProgressOperation["Extract common scalar factor and verify every coefficient"];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"row-"<>ToString[n]<>".wxf"}],Hash[{key,n},"SHA256"],qqScalarRowPrefactor[p["Rows"][[n]]["FiniteFunctionCoefficients"]],limit];
 If[FailureQ[z],Throw[z,stop]];AppendTo[rows,Join[p["Rows"][[n]],z]],{n,Length[p["Rows"]]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 out=Join[KeyTake[p,{"SourceFingerprint","InputSHA256","ColorBasis","ColorBasisLabels","SpinStructures","TreeAmplitude","RenormalizationScaleSquared","Conditions","CoordinateDefinitions","SourceFiniteAuditFile"}],
 <|"Status"->"ScalarRowPrefactorsVerified","Key"->key,"Rows"->rows,"ExactCoefficientEquivalenceVerified"->True,"PrefactorReconstructionVerified"->True,
 "InputLeaves"->LeafCount[p["OneLoopFiniteAmplitude"]],"CheckedCoefficients"->Total[Lookup[rows,"CheckedCoefficients",0]],
 "SourceColorFile"->p["File"],"SourceColorSHA256"->FileHash[p["File"],"SHA256"],
 "Scope"->"Exact common factors of scalar coefficients, with independent reconstruction for every finite function. Original integral values, color/spin basis, source domain and certification unchanged. No new spin-independence or global minimality claim.",
 "File"->FileNameJoin[{dir,"factored.wxf"}]|>];
 AssociateTo[out,"OneLoopFiniteAmplitude"->Total[(p["ColorBasis"][[#["ColorID"]]]p["SpinStructures"][[#["SpinID"]]]#["FiniteFormFactor"])&/@rows]];
 AssociateTo[out,{"Amplitude"->p["TreeAmplitude"]+out["OneLoopFiniteAmplitude"],"OutputLeaves"->LeafCount[out["OneLoopFiniteAmplitude"]]}];
 z=atomicWXF[out["File"],out];If[FailureQ[z],Return[z]];storeResult["QQScalarFactored",out];
 KeyTake[out,{"Status","InputLeaves","OutputLeaves","CheckedCoefficients","PrefactorReconstructionVerified","File"}]];
