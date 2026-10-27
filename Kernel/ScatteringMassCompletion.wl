(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Link independently produced evidence into a NEW mass/scaleless ledger.
   No source result or boolean in an earlier artifact is edited. *)
$massCompletionSHA256=FileHash[$InputFileName,"SHA256"];
qqMassCompletionChecks[j_,l_,region_,raw_,weighted_,probe_,sigmaOK_,probeUsed_] := Module[{sameSource,sameMassSource,model},
 sameSource=AllTrue[{l,region,raw,weighted,probe},AssociationQ[#]&&Lookup[#,"SourceFingerprint",None]===Lookup[j,"Fingerprint",Missing[]]&];
 sameMassSource=AllTrue[{l,region,raw},Lookup[#,"PreLimitPairsSHA256",None]=!=None&&Lookup[#,"PreLimitPairsSHA256",None]===Lookup[l,"PreLimitPairsSHA256",Missing[]]&];
 model=AllTrue[{l,region,raw,weighted,probe},Lookup[#,"ModelFingerprint",None]===modelKey[]&&Lookup[#,"ModelContent",None]===modelContent[]&];
 <|"SameSource"->sameSource,"SameMassInput"->sameMassSource,"CurrentModel"->model,
 "SameMixedMatrix"->AllTrue[{l,region,raw,weighted},Lookup[#,"MixedSourceHash",None]===Lookup[j,"MixedSourceHash",Missing[]]&],
 "CompletedTadpoleInventory"->TrueQ[Lookup[l,"AuditComplete",False]]&&qqMassRegionLedgerCompleteQ[l],
 "IndependentRawBounds"->(TrueQ[Lookup[raw,"AllPairsWithinBound",False]]&&TrueQ[Lookup[raw,"SupportedMassLimitMasters",False]]&&Lookup[raw,"ReadsM0SeriesCheckpoints",None]===False&&Lookup[raw,"PointTest",None]===False&&
 Lookup[raw,"AlgorithmSHA256",None]===$rawMassAuditSHA256&&Lookup[raw,"DependencySHA256",None]==={$massRegionSourceSHA256,$certificationSourceSHA256}),
 "GeneralRegionMatch"->(TrueQ[Lookup[region,"GeneralParameterRegionCancellation",False]]&&Lookup[region,"PointTest",None]===False&&Lookup[region,"AlgorithmSHA256",None]===$massRegionSourceSHA256),
 "MatchingRegionInventory"->(Lookup[raw,"ExpectedPairs",None]===Lookup[l,"PairCount",Missing[]]&&Lookup[raw,"RegulatorBubbleCount",None]===Lookup[region,"BubblesExpected",Missing[]]&&Lookup[region,"BubblesExpected",None]===Lookup[region,"BubblesAudited",Missing[]]),
 "WeightedGraphAndPVCoverage"->(TrueQ[Lookup[weighted,"SourceGraphCoverageVerified",False]]&&TrueQ[Lookup[weighted,"PrePVCoverageVerified",False]]&&TrueQ[Lookup[weighted,"MatrixSourceMatchesQQ",False]]&&Lookup[weighted,"AlgorithmSHA256",None]===$weightedLedgerSHA256),
 "NoUnappliedMixedTransfer"->TrueQ[Lookup[weighted,"AllWeightedTransfersZero",False]],
 "CompleteProbeLedger"->(TrueQ[Lookup[probe,"CoreScanComplete",False]]&&Sort[Lookup[Lookup[probe,"Parts",{}],"Part",{}]]===Sort[{"AV1","AV2","Box1","Box2","NAV1","NAV2"}]),
 "ProbeLedgerUsedByMSbar"->TrueQ[probeUsed],"ConditionalExternalResidueInventory"->TrueQ[sigmaOK]|>];
Options[RGZQQCompleteMassLedger]={"JobFile"->Automatic,"MassLedgerFile"->Automatic,"RegionMatchFile"->Automatic,"RawPoleAuditFile"->Automatic,
 "WeightedLedgerFile"->Automatic,"ProbeLedgerFile"->Automatic,"MSbarFile"->Automatic,"UVIRFile"->Automatic};
RGZQQCompleteMassLedger[OptionsPattern[]] := Module[{r,j,l,region,raw,weighted,probe,m,u,checks,massOK,fullOK,out,key,file,sigma,sigmaExpected,residueConditions={},sigmaOK=False,probeUsed=False,missing},
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 j=qqAuditRead[OptionValue["JobFile"],"QQ"];l=qqMassRegionReadLedger[OptionValue["MassLedgerFile"]];
 region=qqAuditRead[OptionValue["RegionMatchFile"],"QQMassRegionMatch"];raw=qqAuditRead[OptionValue["RawPoleAuditFile"],"QQRawMassPoleAudit"];
 weighted=qqAuditRead[OptionValue["WeightedLedgerFile"],"QQWeightedUVIRLedger"];probe=qqAuditRead[OptionValue["ProbeLedgerFile"],"QQScalelessLedger"];
 m=qqAuditRead[OptionValue["MSbarFile"],"QQMSbar"];u=qqAuditRead[OptionValue["UVIRFile"],"QQUVIR"];
 If[!AssociationQ[weighted],weighted=<||>];If[!AssociationQ[probe],probe=<||>];If[!AssociationQ[m],m=<||>];If[!AssociationQ[u],u=<||>];
 If[!AllTrue[{j,l,region,raw},AssociationQ],Return[Failure["QQMassCompletionPrerequisites",<|"Action"->"Prepare the QQ task, completed tadpole ledger, general region match and independent raw-pole audit."|>]]];
 If[Lookup[j,"ModelContent",None]=!=modelContent[]||!AllTrue[{l,region,raw},Lookup[#,"ModelContent",None]===modelContent[]&],Return[Failure["QQMassCompletionModelContentMismatch",<||>]]];
 If[Lookup[j,"RegulatorMass",None]=!=0||Lookup[j,"Gauge",None]=!=0,Return[Failure["QQMassCompletionLandauM0Only",<||>]]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 If[AssociationQ[m]&&AssociationQ[u]&&AssociationQ[probe]&&And@@(KeyExistsQ[m,#]&/@{"Key","FormFactorsSHA256","RenormalizationScaleSquared"})&&KeyExistsQ[u,"Key"]&&KeyExistsQ[probe,"Key"],
 probeUsed=Lookup[m,"SourceFingerprint",None]===j["Fingerprint"]&&Lookup[u,"SourceFingerprint",None]===j["Fingerprint"]&&
 m["Key"]===Hash[{"QQMSbar/3",u["Key"],probe["Key"],j["Fingerprint"],m["FormFactorsSHA256"],m["RenormalizationScaleSquared"],True,
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringMSbar.wl"}],"SHA256"]},"SHA256"]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 sigma=Lookup[j,"OnShellSigmaV",Missing[]];sigmaExpected=RGZWorkflow`Engine`rgzQQSigmaV0[0,0];
 If[qqAuditValidQ[sigma]&&SameQ[sigma,sigmaExpected],
 residueConditions=DeleteDuplicates[Cases[sigma,FeynCalc`A0[a_]:>Unequal[a,0],Infinity]];
 sigmaOK=residueConditions=!={}&&FreeQ[sigma,FeynCalc`B0|FeynCalc`PaVe|FeynCalc`TID|FeynCalc`FeynAmpDenominator]];
 missing=Keys[Select[<|"WeightedLedger"->weighted,"ProbeLedger"->probe,"MSbar"->m,"UVIR"->u|>,Length[#]===0&]];If[!KeyExistsQ[j,"OnShellSigmaV"],AppendTo[missing,"OnShellResidue"]];
 checks=qqMassCompletionChecks[j,l,region,raw,weighted,probe,sigmaOK,probeUsed];
 (* Model/source compatibility for the mass proof does not depend on whether
    the separate weighted/probe ledgers are already present. *)
 massOK=AllTrue[{l,region,raw},Lookup[#,"ModelFingerprint",None]===modelKey[]&&Lookup[#,"SourceFingerprint",None]===j["Fingerprint"]&&Lookup[#,"MixedSourceHash",None]===j["MixedSourceHash"]&]&&
 And@@Lookup[checks,{"SameMassInput","CompletedTadpoleInventory","IndependentRawBounds","GeneralRegionMatch","MatchingRegionInventory"}];
 fullOK=massOK&&And@@Values[checks];
 key=Hash[{"QQCompleteMassLedger/1",j["Fingerprint"],j["MixedSourceHash"],KeyTake[l,{"SourceFingerprint","MixedSourceHash","PreLimitPairsSHA256","PairCount","PairsAudited","RegulatorTadpolesExpected","RegulatorTadpoles"}],Lookup[region,"Key",None],Lookup[raw,"Key",None],Lookup[weighted,"Key",None],Lookup[probe,"Key",None],checks,$massCompletionSHA256},"SHA256"];
 out=Join[l,<|"Schema"->"RGZQQCompletedMassLedger/1","AlgorithmSHA256"->$massCompletionSHA256,"Key"->key,"Status"->Which[fullOK,"MassAndScalelessCoverageVerified",massOK,"MassProofVerifiedScalelessPending",True,"MassCompletionIncomplete"],"Checks"->checks,
 "MassLimitUVIRCertified"->massOK,"ScalelessLedgerComplete"->fullOK,"MissingEvidence"->missing,"FullSMatrixVerified"->False,
 "ExternalResidueConditions"->residueConditions,"Scope"->"Conditional Landau/M0 two-flavor massless qqprime; source-bound completion ledger, not an inclusive observable or all-parameter certificate.",
 "EvidenceFiles"->(Lookup[#,"File",None]& /@ {region,raw,weighted,probe,m,u}),
 "EvidenceSHA256"->(With[{f=Lookup[#,"File",None]},If[StringQ[f]&&FileExistsQ[f],FileHash[f,"SHA256"],None]]& /@ {region,raw,weighted,probe,m,u}),"OutstandingChecks"->Keys[Select[checks,!TrueQ[#]&]]|>];
 file=FileNameJoin[{cacheDirectory[],"qq-completed-mass-ledger-"<>IntegerString[key,16,64]<>".wxf"}];AssociateTo[out,"File"->file];
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQMassLimitLedger",out];storeResult["QQMassCompletion",out];
 KeyTake[out,{"Status","MassLimitUVIRCertified","ScalelessLedgerComplete","MissingEvidence","Checks","OutstandingChecks","File"}]];
