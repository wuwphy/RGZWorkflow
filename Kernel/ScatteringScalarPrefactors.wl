(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
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
