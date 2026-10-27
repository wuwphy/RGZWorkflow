(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Independent raw-coefficient pole bounds. No M0-series checkpoint is read. *)
$rawMassAuditSHA256=FileHash[$InputFileName,"SHA256"];
qqRawMassValuation[term_,e_] := Module[{factors,base,power,n,den,low=0,stop=Unique["RGZWorkflow`Private`rawValuation"]},Catch[
 factors=If[Head[term]===Times,List@@term,{term}];
 Do[If[FreeQ[f,e],Continue[]];{base,power}=If[Head[f]===Power&&MatchQ[f[[2]],_Integer|_Rational],List@@f,{f,1}];
 If[PolynomialQ[base,e],n=base;den=1,base=Together[base];n=Numerator[base];den=Denominator[base]];
 If[!PolynomialQ[n,e]||!PolynomialQ[den,e],Throw[Failure["QQRawMassNonPolynomial",<||>],stop]];
 If[n===0,Throw[Infinity,stop]];
 low+=power(Exponent[n,e,Min]-Exponent[den,e,Min]),{f,factors}];
 If[!IntegerQ[low],Throw[Failure["QQRawMassFractionalValuation",<|"Power"->low|>],stop]];low,stop]];
qqRawMassBound[co_,dir_,key_,seconds_] := Module[
 {e=RGZResidual39Diagnostic`Private`e,input,terms,low,minimum=Infinity,parts={},z,groups,checks={},file,r,stop=Unique["RGZWorkflow`Private`rawBound"]},Catch[
 If[!qqAuditValidQ[co],Throw[Failure["QQRawMassInvalidCoefficient",<||>],stop]];
 qqProgressOperation["Convert the original coefficient to squared-mass coordinates"];
 input=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=qqAuditPoint[]},RGZResidual39Diagnostic`Private`pointExpression[co]];
 terms=If[Head[input]===Plus,List@@input,{input}];
 Block[{$qqMassSeriesSpecCache=<||>,$qqMassSeriesPowerCache=<||>,$qqMassSeriesSparse=True},
 Do[If[Mod[n,100]===1,qqProgressUpdate[<|"AlgebraGroupIndex"->n,"AlgebraGroupTotal"->Length[terms]|>]];
 low=qqRawMassValuation[terms[[n]],e];If[FailureQ[low],Throw[low,stop]];minimum=Min[minimum,low];
 If[low< -2,qqProgressOperation["Expand only raw terms below the allowed e^-2 bound"];
 z=qqAuditMassSeriesTerm[terms[[n]],e,-3];If[FailureQ[z],Throw[z,stop]];If[AssociationQ[z],AppendTo[parts,z]]],{n,Length[terms]}]];
 groups=Merge[parts,Total];
 Do[qqProgressOperation["Independently cancel raw coefficient below e^-2: power "<>ToString[power]];
 file=FileNameJoin[{dir,"below-"<>ToString[power]<>".wxf"}];
 r=qqAuditCheckpoint[file,Hash[{key,power},"SHA256"],qqMassRegionReduce[groups[power],"Grouped"],seconds];
 If[FailureQ[r],Throw[r,stop]];AppendTo[checks,<|"Power"->power,"Coefficient"->r,"Zero"->(r===0)|>],{power,Sort[Keys[groups]]}];
 <|"PoleBoundVerified"->AllTrue[checks,TrueQ[#["Zero"]]&],"TermCount"->Length[terms],"MinimumTermValuation"->If[minimum===Infinity,None,minimum],
 "AllowedMinimumPower"->-2,"BelowBoundChecks"->checks,"ReadsM0SeriesCheckpoints"->False|>,stop]];
(* Input-only shards are independent of the audit implementation. *)
qqRawMassIndex[j_,sourceSHA_] := Module[{key,dir,file,index,p,r,items={},stop=Unique["RGZWorkflow`Private`rawIndex"]},Catch[
 key=Hash[{"QQRawMassInputs/1",j["Fingerprint"],j["MixedSourceHash"],sourceSHA},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-raw-mass-inputs-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 file=FileNameJoin[{dir,"index.wxf"}];index=If[FileExistsQ[file],Import[file,"WXF"],None];
 If[AssociationQ[index]&&Lookup[index,"Key",None]===key&&ListQ[Lookup[index,"Items",None]]&&AllTrue[index["Items"],FileExistsQ[#["File"]]&],Return[index]];
 qqProgressOperation["Read original pre-limit pairs once and create independent input shards"];
 p=Import[FileNameJoin[{j["Directory"],"self","pairs.wxf"}],"WXF"];
 If[!AssociationQ[p]||Lookup[p,"Key",None]=!=Hash[{j["Fingerprint"],j["MixedSourceHash"]},"SHA256"]||!qqFFVectorPairsQ[Lookup[p,"Value",None]],Return[Failure["QQRawMassSourceMismatch",<||>]]];
 Do[file=FileNameJoin[{dir,"pair-"<>ToString[n]<>".wxf"}];r=atomicWXF[file,<|"Key"->Hash[{key,n},"SHA256"],"Pair"->p["Value"][[n]]|>];If[FailureQ[r],Throw[r,stop]];
 AppendTo[items,<|"Index"->n,"Master"->p["Value"][[n,1]],"File"->file,"SHA256"->FileHash[file,"SHA256"]|>],{n,Length[p["Value"]]}];
 index=<|"Key"->key,"SourceSHA256"->sourceSHA,"Items"->items|>;r=atomicWXF[FileNameJoin[{dir,"index.wxf"}],index];If[FailureQ[r],r,index],stop]];
Options[RGZQQRawMassPoleAudit]={"JobFile"->Automatic,"IDs"->All,"CoefficientSeconds"->180};
RGZQQRawMassPoleAudit[seconds_:3600,OptionsPattern[]] := Module[
 {j,r,index,sourceSHA,sourceFile,key,dir,ids=OptionValue["IDs"],sec=OptionValue["CoefficientSeconds"],rows={},z,packet,item,sub,out,stopped=None,stop=Unique["RGZWorkflow`Private`rawAudit"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 j=qqAuditRead[OptionValue["JobFile"],"QQ"];
 If[!AssociationQ[j]||!And@@(KeyExistsQ[j,#]&/@{"Fingerprint","MixedSourceHash","Directory","ModelFingerprint","ModelContent"})||
 j["ModelFingerprint"]=!=modelKey[]||j["ModelContent"]=!=modelContent[]||Lookup[j,"RegulatorMass",None]=!=0,Return[Failure["QQRawMassJobRequired",<||>]]];
 r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Return[r]];
 sourceFile=FileNameJoin[{j["Directory"],"self","pairs.wxf"}];If[!FileExistsQ[sourceFile],Return[Failure["QQSelfPairsRequired",<||>]]];sourceSHA=FileHash[sourceFile,"SHA256"];
 key=Hash[{"QQRawMassPoleAudit/1",j["Fingerprint"],j["MixedSourceHash"],sourceSHA,$rawMassAuditSHA256,$massRegionSourceSHA256,$certificationSourceSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-raw-mass-audit-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQRawMassPoleAudit",Catch[
 index=qqRawMassIndex[j,sourceSHA];If[FailureQ[index],Throw[index,stop]];
 If[ids===All,ids=Range[Length[index["Items"]]]];
 If[!ListQ[ids]||ids==={}||!AllTrue[ids,IntegerQ[#]&&1<=#<=Length[index["Items"]]&]||!DuplicateFreeQ[ids],Throw[Failure["QQRawMassIDs",<||>],stop]];
 Do[item=index["Items"][[n]];qqProgressUpdate[<|"Stage"->"QQRawMassPoleAudit","Index"->n,"Total"->Length[index["Items"]],"Master"->item["Master"],"StepLimitSeconds"->sec|>];
 sub=FileNameJoin[{dir,"pair-"<>ToString[n]}];If[!DirectoryQ[sub],CreateDirectory[sub]];
 z=qqAuditCheckpoint[FileNameJoin[{sub,"bound.wxf"}],Hash[{key,n,item["SHA256"]},"SHA256"],
 If[FileHash[item["File"],"SHA256"]=!=item["SHA256"],Failure["QQRawMassInputChanged",<||>],
 packet=Import[item["File"],"WXF"];If[Lookup[packet,"Key",None]=!=Hash[{index["Key"],n},"SHA256"],Failure["QQRawMassInputChanged",<||>],qqRawMassBound[packet["Pair"][[2]],sub,Hash[{key,n},"SHA256"],sec]]],sec];
 If[FailureQ[z],Throw[z,stop]];AppendTo[rows,Join[<|"Index"->n,"Master"->item["Master"]|>,z]],{n,ids}];True,stop],seconds];
 If[FailureQ[r],stopped=r];
 out=<|"Schema"->"RGZQQRawMassPoleAudit/1","Status"->Which[stopped=!=None,"RawMassPoleAuditStopped",AllTrue[rows,TrueQ[#["PoleBoundVerified"]]&],"RawMassPoleBoundVerified",True,"HigherRawMassPoleFound"],
 "Key"->key,"AlgorithmSHA256"->$rawMassAuditSHA256,"DependencySHA256"->{$massRegionSourceSHA256,$certificationSourceSHA256},"SourceFingerprint"->j["Fingerprint"],"MixedSourceHash"->j["MixedSourceHash"],"ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],"PreLimitPairsSHA256"->sourceSHA,
 "SupportedMassLimitMasters"->(AssociationQ[index]&&AllTrue[index["Items"],#["Master"]===1||MatchQ[#["Master"],FeynCalc`A0[_]|FeynCalc`B0[___]]&]),
 "RegulatorBubbleCount"->If[AssociationQ[index],Count[Lookup[index["Items"],"Master"],b_FeynCalc`B0/;!FreeQ[b,RGZWorkflow`Engine`M]],0],
 "ExpectedPairs"->If[AssociationQ[index],Length[index["Items"]],0],"PairsAudited"->Length[rows],"Rows"->rows,"PointTest"->False,"ReadsM0SeriesCheckpoints"->False,"RegulatorVariable"->"e=M^2 > 0; bound e^-2 is M^-4",
 "AllPairsWithinBound"->(stopped===None&&AssociationQ[index]&&Sort[ids]===Range[Length[index["Items"]]]&&AllTrue[rows,TrueQ[#["PoleBoundVerified"]]&]),
 "Stopped"->stopped,"File"->FileNameJoin[{dir,"raw-pole-audit.wxf"}]|>;
 z=atomicWXF[out["File"],out];If[FailureQ[z],Return[z]];storeResult["QQRawMassPoleAudit",out];
 If[FailureQ[stopped],Return[Failure[stopped[[1]],Join[stopped[[2]],<|"ReportFile"->out["File"],"Action"->"Repeat RGZQQRawMassPoleAudit; original-pair shards and completed bound checkpoints are reused."|>]]]];
 KeyTake[out,{"Status","PairsAudited","AllPairsWithinBound","File"}]];
