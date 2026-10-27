(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Validate actual numerical/algebraic payloads as well as stable job IDs.
   Kept separate from physical algorithms so raw graph/source keys stay valid. *)
$evidenceValidationSHA256=FileHash[$InputFileName,"SHA256"];
qqCertificationPayloadChecks[a_,m_,g_,f_] := Module[{bare,tree,mrows,grows,frows,ids},
 If[!AllTrue[{a,m,g,f},AssociationQ],Return[<|"BareLoopPayloadMatchesMSbar"->False,"BareTreePayloadMatchesMSbar"->False,
 "FiniteAuditRowsConsistent"->False,"GaugeComparisonRowsConsistent"->False,"FinitePayloadsValid"->False,"CurrentBareMeasure"->False|>]];
 bare=Lookup[a,"OneLoop",Missing["OneLoop"]];tree=Lookup[a,"Tree",Missing["Tree"]];
 mrows=Lookup[m,"Rows",Missing["Rows"]];grows=Lookup[g,"Rows",Missing["Rows"]];frows=Lookup[f,"Rows",Missing["Rows"]];
 ids[rows_]:=Sort[({Lookup[#,"ColorID",Missing[]],Lookup[#,"SpinID",Missing[]]}& /@ rows)];
 <|"BareLoopPayloadMatchesMSbar"->(qqAuditValidQ[bare]&&Lookup[m,"InputSHA256",None]=!=None&&Hash[bare/I,"SHA256"]===m["InputSHA256"]),
 "BareTreePayloadMatchesMSbar"->(qqAuditValidQ[tree]&&KeyExistsQ[m,"TreeAmplitude"]&&SameQ[tree/I,m["TreeAmplitude"]]),
 "FiniteAuditRowsConsistent"->(ListQ[mrows]&&mrows=!={}&&AllTrue[mrows,AssociationQ]&&ListQ[frows]&&AllTrue[frows,AssociationQ]&&
 Length[mrows]===Length[frows]&&qqAuditValidQ[ids[mrows]]&&ids[mrows]===ids[frows]&&DuplicateFreeQ[ids[mrows]]&&
 AllTrue[frows,Lookup[#,"FiniteDifference",None]===0&&Lookup[#,"CountertermDifference",None]===0&]),
 "GaugeComparisonRowsConsistent"->(ListQ[grows]&&AllTrue[grows,AssociationQ[#]&&Lookup[#,"Difference",None]===0&&Lookup[#,"Status",None]==="Zero"&]),
 "FinitePayloadsValid"->qqAuditValidQ[{bare,tree,Lookup[m,"Amplitude",Missing[]],mrows,grows,frows}],
 "CurrentBareMeasure"->(Lookup[a,"AssemblyConvention",None]===qqAssemblyConvention&&Lookup[a,"MixedSelfEnergyLoopMeasure",None]===1/(2Pi)^4)|>];
qqCertificationFiniteAssemblyQ[m_] := Module[{rows,col,spin,loop},
 If[!AssociationQ[m]||!And@@(KeyExistsQ[m,#]&/@{"Rows","ColorBasis","SpinStructures","OneLoopFiniteAmplitude","TreeAmplitude","Amplitude"}),Return[False]];
 rows=m["Rows"];col=m["ColorBasis"];spin=m["SpinStructures"];
 If[!ListQ[rows]||!ListQ[col]||!ListQ[spin]||!AllTrue[rows,AssociationQ[#]&&KeyExistsQ[#,"FiniteFormFactor"]&&
 IntegerQ[Lookup[#,"ColorID",None]]&&IntegerQ[Lookup[#,"SpinID",None]]&&1<=#["ColorID"]<=Length[col]&&1<=#["SpinID"]<=Length[spin]&],Return[False]];
 loop=Total[(col[[#["ColorID"]]]spin[[#["SpinID"]]]#["FiniteFormFactor"])&/@rows];
 SameQ[loop,m["OneLoopFiniteAmplitude"]]&&SameQ[m["TreeAmplitude"]+loop,m["Amplitude"]]];
qqCertificationGaugeSourceQ[g_] := Module[{file,p},
 If[!AssociationQ[g]||Lookup[g,"Method",None]=!="RawIntegrandGaugeDifferenceBeforeTID"||!StringQ[Lookup[g,"File",None]],Return[False]];
 file=FileNameJoin[{DirectoryName[g["File"]],"prepared.wxf"}];If[!FileExistsQ[file],Return[False]];
 p=Quiet[Check[Import[file,"WXF"],$Failed]];
 AssociationQ[p]&&Lookup[p,"AlgorithmHash",None]===qqGaugeEarlyAlgorithmHash[]&&
 Lookup[p,"Fingerprint",None]===Lookup[g,"GaugeJobFingerprint",Missing[]]&&Lookup[p,"MixedSourceHash",None]===Lookup[g,"MixedSourceHash",Missing[]]&&
 Lookup[p,"ReferenceGauge",None]===Lookup[g,"ReferenceGauge",Missing[]]&&Lookup[p,"Status",None]==="IntegrandDifferencesPrepared"&&
 Sort[Lookup[p,"Parts",{}]]===Sort[qqGaugeEarlyParts]&&KeyExistsQ[p,"Expression"]&&qqAuditValidQ[p["Expression"]]];
qqCertificationMassEvidenceQ[l_] := Module[{files,hashes,j,pairs},
 If[!AssociationQ[l]||Lookup[l,"Schema",None]=!="RGZQQCompletedMassLedger/1"||
 Lookup[l,"AlgorithmSHA256",None]=!=$massCompletionSHA256||
 !AssociationQ[Lookup[l,"Checks",None]]||Sort[Keys[l["Checks"]]]=!=Sort[{"SameSource","SameMassInput","CurrentModel","SameMixedMatrix","CompletedTadpoleInventory","IndependentRawBounds","GeneralRegionMatch","MatchingRegionInventory","WeightedGraphAndPVCoverage","NoUnappliedMixedTransfer","CompleteProbeLedger","ProbeLedgerUsedByMSbar","ConditionalExternalResidueInventory"}]||!AllTrue[Values[l["Checks"]],TrueQ],Return[False]];
 files=Lookup[l,"EvidenceFiles",None];hashes=Lookup[l,"EvidenceSHA256",None];
 If[!ListQ[files]||Length[files]=!=6||!ListQ[hashes]||Length[hashes]=!=6||
 !AllTrue[files,StringQ[#]&&FileExistsQ[#]&]||!AllTrue[hashes,IntegerQ],Return[False]];
 j=RGZResult["QQ"];If[!AssociationQ[j]||!StringQ[Lookup[j,"Directory",None]],Return[False]];
 pairs=FileNameJoin[{j["Directory"],"self","pairs.wxf"}];
 FileExistsQ[pairs]&&Lookup[l,"PreLimitPairsSHA256",None]===FileHash[pairs,"SHA256"]&&
 Map[FileHash[#,"SHA256"]&,files]===hashes];
(* Copy the original aggregation, but suppress its write until the additional
   checks have been merged. A failed native call cannot expose an old packet. *)
DownValues[qqCertificateWithoutPayloadGuard]=DownValues[RGZQQCertification]/.RGZQQCertification->qqCertificateWithoutPayloadGuard;
Options[qqCertificateWithoutPayloadGuard]=Options[RGZQQCertification];
RGZQQCertification[OptionsPattern[]] := Module[{r,a,m,g,f,p,extra,checks,file,hashes},
 r=ready[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,{"QQCertification"}];
 a=qqAuditRead[OptionValue["AmplitudeFile"],"QQAmplitude"];m=qqAuditRead[OptionValue["MSbarFile"],"QQMSbar"];
 g=qqAuditRead[OptionValue["GaugeFile"],"QQGaugeEarly"];f=qqAuditRead[OptionValue["FiniteAuditFile"],"QQFiniteAudit"];
 extra=Join[qqCertificationPayloadChecks[a,m,g,f],<|
 "FiniteAmplitudeAssemblyConsistent"->TrueQ[qqCertificationFiniteAssemblyQ[m]],
 "BareJobSourceCurrent"->(AssociationQ[a]&&And@@(KeyExistsQ[a,#]&/@{"Fingerprint","Gauge","RegulatorMass"})&&IntegerQ[a["Fingerprint"]]&&a["Fingerprint"]===qqJobFingerprint[a["Gauge"],a["RegulatorMass"]]),
 "GaugePreparationSourceCurrent"->TrueQ[qqCertificationGaugeSourceQ[g]],
 "MassEvidenceCurrent"->TrueQ[qqCertificationMassEvidenceQ[qqAuditRead[OptionValue["MassLedgerFile"],"QQMassLimitLedger"]]]|>];
 r=Block[{$results=$results},Internal`InheritedBlock[{atomicWXF},
 Clear[atomicWXF];atomicWXF[_,_]:=<|"Status"->"DeferredCertificationWrite"|>;
 r=qqCertificateWithoutPayloadGuard["AmplitudeFile"->OptionValue["AmplitudeFile"],"MSbarFile"->OptionValue["MSbarFile"],
 "GaugeFile"->OptionValue["GaugeFile"],"MassLedgerFile"->OptionValue["MassLedgerFile"],"FiniteAuditFile"->OptionValue["FiniteAuditFile"]];
 If[!FailureQ[r],p=RGZResult["QQCertification"]];r]];
 If[FailureQ[r],Return[r]];If[!AssociationQ[p],Return[Failure["QQCertificationIncompletePacket",<||>]]];
 checks=Join[p["Checks"],extra];
 hashes=If[AllTrue[{a,m,g,f},AssociationQ],<|"BareTree"->Hash[Lookup[a,"Tree",Missing[]],"SHA256"],"BareOneLoop"->Hash[Lookup[a,"OneLoop",Missing[]],"SHA256"],
 "MSbarAmplitude"->Hash[Lookup[m,"Amplitude",Missing[]],"SHA256"],"GaugeRows"->Hash[Lookup[g,"Rows",Missing[]],"SHA256"],"FiniteAuditRows"->Hash[Lookup[f,"Rows",Missing[]],"SHA256"]|>,<||>];
 p=Join[KeyDrop[p,{"Amplitude","File"}],<|"Checks"->checks,"OutstandingChecks"->Keys[Select[checks,!TrueQ[#]&]],
 "FullSMatrixVerified"->AllTrue[Values[checks],TrueQ],"Status"->If[AllTrue[Values[checks],TrueQ],"CertifiedWithinDeclaredDomain","CertificationIncomplete"],
 "ValidationAlgorithmSHA256"->$evidenceValidationSHA256,"EvidencePayloadSHA256"->hashes,
 "FiniteAuditFile"->If[AssociationQ[f],Lookup[f,"File",Missing["InMemory"]],Missing["Absent"]],
 "BareAmplitudeFile"->If[StringQ[OptionValue["AmplitudeFile"]],OptionValue["AmplitudeFile"],Missing["InMemory"]]|>];
 If[TrueQ[p["FullSMatrixVerified"]],AssociateTo[p,"Amplitude"->m["Amplitude"]]];
 file=FileNameJoin[{cacheDirectory[],"qq-certification-"<>IntegerString[Hash[KeyDrop[p,"Amplitude"],"SHA256"],16,64]<>".wxf"}];AssociateTo[p,"File"->file];
 r=atomicWXF[file,p];If[FailureQ[r],Return[r]];storeResult["QQCertification",p];KeyTake[p,{"Status","Checks","OutstandingChecks","FullSMatrixVerified","File"}]];
