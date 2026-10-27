(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Separate D-dimensional scalar-PV presentation; source FF and certificates
   remain unchanged. Native tensor identities are applied before epsilon expansion. *)
$scalarFFSourceSHA256=FileHash[$InputFileName,"SHA256"];
qqScalarPVQ[m_] := m===1||MatchQ[m,_FeynCalc`A0|_FeynCalc`B0|_FeynCalc`C0|_FeynCalc`D0]||
 (MatchQ[m,_FeynCalc`PaVe]&&With[{v=qqPVParse[m]},v=!=$Failed&&v[[1]]==={0}&&Length[v[[3]]]<=4]);
qqScalarPVIdentity[m_] := Module[{z,ints,pairs,rules,v},
 If[m===1,Return[{{1,1}}]];
 z=If[qqScalarPVQ[m],m,qqPVNativeReduce[m]];
 If[qqFFInvalidQ[z],Return[Failure["QQScalarPVIdentityFailed",<|"Master"->m|>]]];
 (* Native reduction can leave exceptional B1/B00 at zero invariant.
    Lower these with exact generic-D vacuum identities before validation. *)
 Do[ints=DeleteDuplicates[Cases[z,x_ /; MatchQ[Head[x],qqFFMasterHeads] :> x,{0,Infinity}]];
 rules=Table[v=qqPVZeroInvariant[x];If[v===$Failed,v=If[qqScalarPVQ[x],x,qqPVNativeReduce[x]]];x->v,{x,ints}];
 z=z/.Dispatch[rules],{3}];
 ints=DeleteDuplicates[Cases[z,x_ /; MatchQ[Head[x],qqFFMasterHeads] :> x,{0,Infinity}]];
 If[!AllTrue[ints,qqScalarPVQ],Return[Failure["QQScalarPVUnreduced",<|"Master"->m|>]]];
 z=z/.Dispatch[(#->qqPVScalarCanonical[#])&/@ints];
 pairs=qqFFCollectMasters[z,False,1];
 If[FailureQ[pairs]||!AllTrue[pairs,qqScalarPVQ[First[#]]&],Failure["QQScalarPVIdentityFailed",<|"Master"->m|>],pairs]];
qqScalarCollectRow[pairs_,identities_] := Module[{terms={},id},
 If[!AllTrue[pairs,ListQ[Lookup[identities,First[#],Missing[]]]&],Return[Failure["QQScalarIdentityMissing",<||>]]];
 Do[id=Lookup[identities,First[pair]];
 terms=Join[terms,({First[#],Last[pair] Last[#]}&/@id)],{pair,pairs}];
 ({#[[1,1]],Total[#[[All,2]]]}&/@GatherBy[terms,First])];
Options[RGZQQScalarFormFactors]={"File"->Automatic,"IntegralSeconds"->30,"CoefficientSeconds"->30};
RGZQQScalarFormFactors[seconds_:3600,OptionsPattern[]] := Module[
 {r,p,file=OptionValue["File"],sec=OptionValue["IntegralSeconds"],co=OptionValue["CoefficientSeconds"],key,dir,sourceSHA,masters,identities=<||>,z,rows={},pairs,grouped,out,coeff,cache,ck,cf,n,idx,timeoutCount=0,after,before,stop=Unique["scalarFF"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec,co}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 p=qqUVIRReadFF[file];If[FailureQ[p],Return[p]];
 sourceSHA=FileHash[p["File"],"SHA256"];
 key=Hash[{"QQScalarFormFactors/1",sourceSHA,p["InputSHA256"],$scalarFFSourceSHA256,
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVRelations.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVScalarOrder.wl"}],"SHA256"],FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringFormFactors.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringCoefficient.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringCoefficientSpectral.wl"}],"SHA256"],
 DownValues[FeynCalc`PaVeReduce],Options[FeynCalc`PaVeReduce],$Version},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-scalar-ff-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQScalarFormFactors",Catch[
 masters=DeleteDuplicates[Flatten[(First/@#["MasterCoefficients"])&/@p["Rows"],1]];
 Do[qqProgressUpdate[<|"Stage"->"QQScalarFormFactors:Identities","Index"->n,"Total"->Length[masters],"Master"->masters[[n]],"StepLimitSeconds"->sec|>];
 qqProgressOperation["D-dimensional scalar PV identity "<>ToString[n]<>" / "<>ToString[Length[masters]]];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"identity-"<>IntegerString[Hash[masters[[n]],"SHA256"],16,64]<>".wxf"}],Hash[{key,masters[[n]]},"SHA256"],qqScalarPVIdentity[masters[[n]]],sec];
 If[FailureQ[z],Throw[z,stop]];AssociateTo[identities,masters[[n]]->z],{n,Length[masters]}];
 Do[qqProgressUpdate[<|"Stage"->"QQScalarFormFactors:Coefficients","Index"->n,"Total"->Length[p["Rows"]],"StepLimitSeconds"->co|>];
 qqProgressOperation["Collect scalar masters in color-spin row "<>ToString[n]];
 grouped=qqScalarCollectRow[p["Rows"][[n]]["MasterCoefficients"],identities];If[FailureQ[grouped],Throw[grouped,stop]];pairs={};
 Do[qqProgressUpdate[<|"Master"->grouped[[idx,1]],"AlgebraGroupIndex"->idx,"AlgebraGroupTotal"->Length[grouped],"InputLeaves"->LeafCount[grouped[[idx,2]]],"ItemStarted"->AbsoluteTime[]|>];
 qqProgressOperation["Reduce scalar coefficient "<>ToString[idx]<>" / "<>ToString[Length[grouped]]];
 cf=FileNameJoin[{dir,"co-"<>ToString[n]<>"-"<>ToString[idx]<>".wxf"}];ck=Hash[{key,n,grouped[[idx]],co},"SHA256"];
 cache=If[FileExistsQ[cf],Import[cf,"WXF"],None];
 If[AssociationQ[cache]&&Lookup[cache,"Key",None]===ck&&!qqFFInvalidQ[Lookup[cache,"Value",$Failed]],coeff=cache["Value"];qqProgressUpdate[<|"CacheState"->"Reused"|>],
 coeff=TimeConstrained[qqCoefficientRational[grouped[[idx,2]]],co,$TimedOut];
 If[qqFFInvalidQ[coeff],coeff=grouped[[idx,2]];timeoutCount++,coeff=qqCoefficientCompact[grouped[[idx,2]],coeff];
 z=atomicWXF[cf,<|"Key"->ck,"Value"->coeff|>];If[FailureQ[z],Throw[z,stop]]]];
 If[coeff=!=0,AppendTo[pairs,{grouped[[idx,1]],coeff}]],{idx,Length[grouped]}];
 AppendTo[rows,Join[KeyTake[p["Rows"][[n]],{"ColorID","SpinID"}],<|"MasterCoefficients"->pairs,"FormFactor"->Total[Times@@@pairs]|>]],{n,Length[p["Rows"]]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];If[r=!=True,Return[Failure["QQScalarFFIncomplete",<||>]]];
 before=DeleteDuplicates[DeleteCases[masters,1]];after=DeleteDuplicates[DeleteCases[Flatten[(First/@#["MasterCoefficients"])&/@rows,1],1]];
 out=Join[KeyTake[p,{"Schema","SourceFingerprint","InputSHA256","LoopOrder","SelectedComponent","AssemblyConvention","Gauge","RegulatorMass","ColorBasis","SpinStructures","SpinBasisIndependent","Renormalized","GaugeIndependenceVerified"}],
 <|"Key"->Hash[{key,co},"SHA256"],"Status"->If[timeoutCount===0,"ScalarPVCollected","ScalarPVCollectedExactCoefficientsRetained"],"Rows"->rows,"SourceFormFactorsFile"->p["File"],"SourceFormFactorsSHA256"->sourceSHA,
 "AlgorithmSHA256"->$scalarFFSourceSHA256,"PVIdentities"->Normal[identities],"ScalarIntegralInventory"->after,"ScalarIntegralCounts"->Counts[Head/@after],"InputIntegralCount"->Length[before],"OutputIntegralCount"->Length[after],
 "CoefficientTimeouts"->timeoutCount,"ReconstructionVerified"->True,"Verification"-><|"SourceReconstruction"->True,"NativePVIdentities"->"FeynCalc PaVeReduce, Dimension->D","ExactLinearCollection"->True,"IndependentIntegralIdentityProof"->False|>,
 "RequiresRepresentationUVIRLedger"->True,"SeparateUVIRPreservationVerified"->False,"GenericNondegenerateKinematicsRequired"->True,"MasterBasisIndependent"->False,"TensorPVReductionRequested"->True,"UVIRSeparated"->False,
 "Scope"->"D-dimensional scalar-PV presentation of the same pure-loop FF; not a new renormalized amplitude or certificate. Recompute UVIR/MSbar explicitly to use this basis downstream.",
 "File"->FileNameJoin[{dir,"scalar-form-factors-"<>IntegerString[Hash[co,"SHA256"],16,64]<>".wxf"}]|>];
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQScalarFormFactors",out];
 KeyTake[out,{"Status","InputIntegralCount","OutputIntegralCount","ScalarIntegralCounts","CoefficientTimeouts","File"}]];

Options[RGZQQScalarPoleAudit]={"ScalarFile"->Automatic,"ReferenceUVIRFile"->Automatic,"IntegralSeconds"->30,"CoefficientSeconds"->30};
RGZQQScalarPoleAudit[seconds_:1200,OptionsPattern[]] := Module[
 {p,old,r,u,sec=OptionValue["IntegralSeconds"],co=OptionValue["CoefficientSeconds"],rows={},prev,du,di,common,eps=Unique["scalarPoleEps"],key,file,out,stop=Unique["scalarPoles"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec,co}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 p=qqAuditRead[OptionValue["ScalarFile"],"QQScalarFormFactors"];old=qqAuditRead[OptionValue["ReferenceUVIRFile"],"QQUVIR"];
 If[!AllTrue[{p,old},AssociationQ]||!TrueQ[Lookup[p,"ReconstructionVerified",False]]||
 !TrueQ[Lookup[old,"CompleteRepresentedUV",False]]||!TrueQ[Lookup[old,"CompleteRepresentedIR",False]]||
 Lookup[p,"SourceFingerprint",None]=!=Lookup[old,"SourceFingerprint",Missing[]]||Lookup[p,"InputSHA256",None]=!=Lookup[old,"InputSHA256",Missing[]]||
 p["ColorBasis"]=!=old["ColorBasis"]||p["SpinStructures"]=!=old["SpinStructures"],Return[Failure["QQScalarPoleSourceMismatch",<||>]]];
 (* Do not replace the certified pipeline's active QQUVIR/renormalization packet. *)
 u=Block[{$results=$results},r=RGZQQUVIR[seconds,"File"->p["File"],"IntegralSeconds"->sec,"CoefficientSeconds"->co];If[FailureQ[r],r,RGZResult["QQUVIR"]]];
 If[FailureQ[u],Return[u]];
 If[Length[u["Rows"]]===0||Length[u["Rows"]]=!=Length[old["Rows"]]||!TrueQ[u["CompleteRepresentedUV"]]||!TrueQ[u["CompleteRepresentedIR"]],Return[Failure["QQScalarPoleUnsupported",<|"File"->u["File"]|>]]];
 r=qqGaugeTimed["QQScalarPoleAudit",Catch[
 Do[prev=SelectFirst[old["Rows"],#["ColorID"]===row["ColorID"]&&#["SpinID"]===row["SpinID"]&,Missing[]];
 If[!AssociationQ[prev],Throw[Failure["QQScalarPoleRowMismatch",<||>],stop]];
 qqProgressOperation["Compare scalar/tensor UV and IR split"];
 du=TimeConstrained[qqUVIRCompactPole[row["UVPole"]-prev["UVPole"]],co,$TimedOut];
 di=TimeConstrained[qqUVIRCompactPole[row["IRPole"]-prev["IRPole"]],co,$TimedOut];
 common=TimeConstrained[qqUVIRCompactPole[(du/.FeynCalc`EpsilonUV->eps)+(di/.FeynCalc`EpsilonIR->eps)],co,$TimedOut];
 If[qqFFInvalidQ[{du,di,common}],Throw[Failure["QQScalarPoleComparisonStopped",<||>],stop]];
 AppendTo[rows,Join[KeyTake[row,{"ColorID","SpinID"}],<|"UVRepresentationTransfer"->du,"IRRepresentationTransfer"->di,"CommonPoleDifference"->common,"CommonPoleZero"->(common===0)|>]],{row,u["Rows"]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 key=Hash[{"QQScalarPoleAudit/1",p["Key"],old["Key"],u["Key"],$scalarFFSourceSHA256},"SHA256"];
 file=FileNameJoin[{cacheDirectory[],"qq-scalar-pole-audit-"<>IntegerString[key,16,64]<>".wxf"}];
 out=<|"Status"->If[AllTrue[rows,TrueQ[#["CommonPoleZero"]]&],"CommonRegulatorPolesVerified","CommonRegulatorPoleMismatch"],"Key"->key,
 "SourceFingerprint"->p["SourceFingerprint"],"InputSHA256"->p["InputSHA256"],"Rows"->rows,
 "CommonRegulatorPolesVerified"->AllTrue[rows,TrueQ[#["CommonPoleZero"]]&],
 "SeparateUVIRUnchanged"->AllTrue[rows,#["UVRepresentationTransfer"]===0&&#["IRRepresentationTransfer"]===0&],
 "TransferRowCount"->Count[rows,x_/;x["UVRepresentationTransfer"]=!=0||x["IRRepresentationTransfer"]=!=0],
 "ScalarFormFactorsFile"->p["File"],"ReferenceUVIRFile"->old["File"],"ScalarUVIRFile"->u["File"],
 "FiniteAmplitudeEquivalenceVerified"->False,"FullSMatrixVerified"->False,
 "Scope"->"Pole-level representation transfer only. New basis needs its own consistent UV/IR ledger and finite audit before replacing a certified amplitude.","File"->file|>;
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQScalarPoleAudit",out];
 KeyTake[out,{"Status","CommonRegulatorPolesVerified","SeparateUVIRUnchanged","TransferRowCount","File"}]];
