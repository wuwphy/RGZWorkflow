(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* No production data; protect same-ID source swaps and invalid cache values. *)
rgzEvidenceRoot=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{rgzEvidenceRoot,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"rgz-evidence-"<>CreateUUID[]}]];
rgzEvidenceChecks=<||>;rgzEvidenceRecord[n_,v_]:=(AssociateTo[rgzEvidenceChecks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
rgzEvidenceA=<|"Fingerprint"->91,"Gauge"->0,"RegulatorMass"->0,"OneLoop"->I loop,"Tree"->I tree,"AssemblyConvention"->"QQPhysicalLoopMeasure/1","MixedSelfEnergyLoopMeasure"->1/(2Pi)^4|>;
rgzEvidenceM=<|"InputSHA256"->Hash[loop,"SHA256"],"TreeAmplitude"->tree,"Amplitude"->tree+finite,"OneLoopFiniteAmplitude"->finite,"ColorBasis"->{1},"SpinStructures"->{1},"Rows"->{<|"ColorID"->1,"SpinID"->1,"FiniteFormFactor"->finite,"RemainingUVPole"->0,"RemainingIRPole"->0|>}|>;
rgzEvidenceG=<|"Rows"->{<|"Difference"->0,"Status"->"Zero"|>}|>;
rgzEvidenceF=<|"Rows"->{<|"ColorID"->1,"SpinID"->1,"FiniteDifference"->0,"CountertermDifference"->0|>}|>;
rgzEvidencePayload[a_:rgzEvidenceA,m_:rgzEvidenceM,g_:rgzEvidenceG,f_:rgzEvidenceF]:=RGZWorkflow`Private`qqCertificationPayloadChecks[a,m,g,f];
rgzEvidenceRecord["MatchingPayloads",And@@Values[rgzEvidencePayload[]]];
rgzEvidenceRecord["ChangedLoopWithSameIDRejected",rgzEvidencePayload[Join[rgzEvidenceA,<|"OneLoop"->I (loop+1)|>]]["BareLoopPayloadMatchesMSbar"]===False];
rgzEvidenceRecord["ChangedTreeRejected",rgzEvidencePayload[Join[rgzEvidenceA,<|"Tree"->I (tree+1)|>]]["BareTreePayloadMatchesMSbar"]===False];
rgzEvidenceRecord["MissingLoopRejected",rgzEvidencePayload[KeyDrop[rgzEvidenceA,"OneLoop"]]["BareLoopPayloadMatchesMSbar"]===False];
rgzEvidenceRecord["ChangedMeasureRejected",rgzEvidencePayload[Join[rgzEvidenceA,<|"MixedSelfEnergyLoopMeasure"->1|>]]["CurrentBareMeasure"]===False];
rgzEvidenceRecord["NonzeroGaugeRowRejected",rgzEvidencePayload[rgzEvidenceA,rgzEvidenceM,<|"Rows"->{<|"Difference"->1,"Status"->"Zero"|>}|>]["GaugeComparisonRowsConsistent"]===False];
rgzEvidenceRecord["MissingFiniteRowsRejected",rgzEvidencePayload[rgzEvidenceA,rgzEvidenceM,rgzEvidenceG,<|"Rows"->{}|>]["FiniteAuditRowsConsistent"]===False];
rgzEvidenceRecord["FiniteMismatchRejected",rgzEvidencePayload[rgzEvidenceA,rgzEvidenceM,rgzEvidenceG,<|"Rows"->{<|"ColorID"->1,"SpinID"->1,"FiniteDifference"->1,"CountertermDifference"->0|>}|>]["FiniteAuditRowsConsistent"]===False];
rgzEvidenceRecord["InvalidFinitePayloadRejected",rgzEvidencePayload[rgzEvidenceA,Join[rgzEvidenceM,<|"Amplitude"->TerminatedEvaluation[IterationLimit]|>]]["FinitePayloadsValid"]===False];
rgzEvidenceMX=<|"T"->ConstantArray[0,{5,5}],"L"->ConstantArray[0,{8,8}],"FullColorTensorMatrix"->ConstantArray[0,{8,8}],"TFieldOrder"->{},"LFieldOrder"->{}|>;
rgzEvidenceRecord["RegularMixedMatrixAccepted",TrueQ[RGZWorkflow`Private`validMixed[rgzEvidenceMX]]];
Do[rgzEvidenceBad=Join[rgzEvidenceMX,<|"T"->ReplacePart[rgzEvidenceMX["T"],{1,1}->v]|>];
 rgzEvidenceRecord["InvalidMixed"<>ToString[i],!TrueQ[RGZWorkflow`Private`validMixed[rgzEvidenceBad]]],{i,1,7},{v,Take[{$TimedOut,TerminatedEvaluation[IterationLimit],Indeterminate,ComplexInfinity,Infinity,$Aborted,$Failed},{i}]}];
rgzEvidencePrepDir=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"gauge-preparation"}];CreateDirectory[rgzEvidencePrepDir];
rgzEvidencePrep=<|"AlgorithmHash"->RGZWorkflow`Private`qqGaugeEarlyAlgorithmHash[],"Fingerprint"->99,"MixedSourceHash"->88,"ReferenceGauge"->1,"Status"->"IntegrandDifferencesPrepared","Parts"->RGZWorkflow`Private`qqGaugeEarlyParts,"Expression"->0|>;
rgzEvidencePrepG=<|"Method"->"RawIntegrandGaugeDifferenceBeforeTID","File"->FileNameJoin[{rgzEvidencePrepDir,"comparison.wxf"}],"GaugeJobFingerprint"->99,"MixedSourceHash"->88,"ReferenceGauge"->1|>;
Export[FileNameJoin[{rgzEvidencePrepDir,"prepared.wxf"}],rgzEvidencePrep,"WXF"];
rgzEvidenceRecord["CurrentGaugePreparationAccepted",TrueQ[RGZWorkflow`Private`qqCertificationGaugeSourceQ[rgzEvidencePrepG]]];
Export[FileNameJoin[{rgzEvidencePrepDir,"prepared.wxf"}],Join[rgzEvidencePrep,<|"AlgorithmHash"->0|>],"WXF"];
rgzEvidenceRecord["StaleGaugePreparationRejected",!TrueQ[RGZWorkflow`Private`qqCertificationGaugeSourceQ[rgzEvidencePrepG]]];
rgzEvidenceRecord["MissingGaugePreparationRejected",!TrueQ[RGZWorkflow`Private`qqCertificationGaugeSourceQ[<||>]]];
rgzEvidenceRecord["FiniteAssemblyExact",TrueQ[RGZWorkflow`Private`qqCertificationFiniteAssemblyQ[rgzEvidenceM]]];
rgzEvidenceRecord["ChangedFiniteAssemblyRejected",!TrueQ[RGZWorkflow`Private`qqCertificationFiniteAssemblyQ[Join[rgzEvidenceM,<|"Amplitude"->tree+finite+1|>]]]];
(* Exercise the public aggregate against a native-aggregator fixture. *)
Internal`InheritedBlock[{RGZWorkflow`Private`qqCertificateWithoutPayloadGuard,RGZWorkflow`Private`qqJobFingerprint,RGZWorkflow`Private`qqCertificationGaugeSourceQ,RGZWorkflow`Private`qqCertificationMassEvidenceQ},
 Clear[RGZWorkflow`Private`qqJobFingerprint,RGZWorkflow`Private`qqCertificationGaugeSourceQ,RGZWorkflow`Private`qqCertificationMassEvidenceQ];
 RGZWorkflow`Private`qqCertificationMassEvidenceQ[_]:=True;
 RGZWorkflow`Private`qqJobFingerprint[___]:=91;RGZWorkflow`Private`qqCertificationGaugeSourceQ[_]:=True;
 Clear[RGZWorkflow`Private`qqCertificateWithoutPayloadGuard];
 Options[RGZWorkflow`Private`qqCertificateWithoutPayloadGuard]=Options[RGZQQCertification];
 RGZWorkflow`Private`qqCertificateWithoutPayloadGuard[OptionsPattern[]]:=(RGZWorkflow`Private`storeResult["QQCertification",<|"Checks"-><|"Native"->True|>,"FullSMatrixVerified"->True,"Amplitude"->tree+finite|>];<|"Status"->"CertifiedWithinDeclaredDomain"|>);
 RGZWorkflow`Private`$results=<|"QQAmplitude"->rgzEvidenceA,"QQMSbar"->rgzEvidenceM,"QQGaugeEarly"->rgzEvidenceG,"QQFiniteAudit"->rgzEvidenceF|>;
 rgzEvidenceR=RGZQQCertification[];rgzEvidenceRecord["PublicMatchingCertificate",AssociationQ[rgzEvidenceR]&&TrueQ[rgzEvidenceR["FullSMatrixVerified"]]];
 rgzEvidenceBefore=RGZResult["QQCertification"];rgzEvidenceRecord["PayloadHashesRecorded",Length[rgzEvidenceBefore["EvidencePayloadSHA256"]]===5];
 RGZWorkflow`Private`storeResult["QQAmplitude",Join[rgzEvidenceA,<|"OneLoop"->I (loop+1)|>]];
 rgzEvidenceR=RGZQQCertification[];rgzEvidenceRecord["PublicMismatchCannotCertify",rgzEvidenceR["Status"]==="CertificationIncomplete"&&MemberQ[rgzEvidenceR["OutstandingChecks"],"BareLoopPayloadMatchesMSbar"]];
 rgzEvidenceRecord["IncompleteCertificateHasNoAmplitude",!KeyExistsQ[RGZResult["QQCertification"],"Amplitude"]];
];
Internal`InheritedBlock[{RGZWorkflow`Private`engineGet,RGZWorkflow`Private`qqJobFingerprint},
 Clear[RGZWorkflow`Private`engineGet,RGZWorkflow`Private`qqJobFingerprint];
 RGZWorkflow`Private`engineGet[___]:=Null;RGZWorkflow`Private`qqJobFingerprint[___]:=91;
 rgzEvidenceJobDir=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"cached-part"}];CreateDirectory[rgzEvidenceJobDir];
 Do[RGZWorkflow`Private`storeResult["QQ",<|"Fingerprint"->91,"Gauge"->0,"RegulatorMass"->0,"Integrands"-><|"AV1"->0|>,"Directory"->rgzEvidenceJobDir,"Reduced"-><||>|>];
 Export[FileNameJoin[{rgzEvidenceJobDir,"AV1-reduced.wxf"}],<|"Fingerprint"->91,"Part"->"AV1","Expression"->v|>,"WXF"];
 rgzEvidenceRecord["CachedPartInvalid"<>ToString[i],FailureQ[RGZQQRunPart["AV1",1]]&&RGZResult["QQ"]["Reduced"]===<||>],
 {i,1,3},{v,Take[{Indeterminate,ComplexInfinity,Infinity},{i}]}];
];
Print[<|"Passed"->Count[Values[rgzEvidenceChecks],True],"Failed"->Count[Values[rgzEvidenceChecks],False]|>];Exit[If[And@@Values[rgzEvidenceChecks],0,1]];
