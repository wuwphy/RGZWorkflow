(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZTreeBenchmark[60];RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`Private`loadLoopEngine[];Scan[RGZWorkflow`Private`engineGet,{"Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","Algebra/CommonDenominator.wl","Pinch/Kernels.wl","Pinch/Continuation.wl"}];RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[]];
checks=<||>;rec[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
RGZMXSetDisplay["Quiet"];
unit=<|"ID"->1,"MasterIntegral"->A0[rgzFDTestMass],"AlphaShortCoefficient"->1,"ReferenceShortCoefficient"->0,"ReferenceGauge"->1|>;
Do[
rec["Finish"<>ToString[bad,InputForm],FailureQ[RGZGaugeFixedFinish[unit,bad,<|"Source"->"Test"|>]]];
rec["Coordinates"<>ToString[bad,InputForm],FailureQ[RGZGaugeFixedCoordinates[bad]]];
rec["Compare"<>ToString[bad,InputForm],FailureQ[RGZGaugeFixedCompare[bad,1,10,"CacheDirectory"->None]]];
rec["Write"<>ToString[bad,InputForm],FailureQ[RGZGaugeFixedCacheWrite[bad,None,1]]],
{bad,{TerminatedEvaluation["IterationLimit"],Indeterminate,ComplexInfinity,$TimedOut,Global`\[Alpha]}}];
r=RGZGaugeFixedPair[unit,10,"CacheDirectory"->None];
rec["ValidFinish",AssociationQ[r]&&r["CoefficientStatus"]==="Nonzero"&&RGZGaugeRowQ[rgzGaugeFixedCoefficient]];
rec["IndependentZero",AssociationQ[RGZGaugeFixedCompare[A0[rgzFDTestMass],1,10,"CacheDirectory"->None]]&&rgzGaugeFixedReport["Statistics"]["Zero"]===1];
e=\[Alpha] A0[rgzFDTestMass];ref=1;
k=Hash[{"RGZFixedPrepared-v2",e,ref,DownValues[RGZGaugePrepare],DownValues[RGZGaugePVPairs],DownValues[RGZGaugeCanonicalPV],DownValues[RGZLoopPoleMasses]},"SHA256"];
Block[{rgzGaugeFixedPrepared=<|"Identity"->k,"ReferenceGauge"->ref,"CoefficientList"->{},"ContentHash"->123|>},
r=RGZGaugeFixedCompare[e,ref,10,"CacheDirectory"->None];rec["CorruptPreparedRecomputed",AssociationQ[r]&&TrueQ[r["Statistics"]["Nonzero"]>0]]];
tmp=FileNameJoin[{CreateDirectory[],"cache.wl"}];
rec["CacheWrite",TrueQ[RGZGaugeFixedCacheWrite[<|"ResidualCoefficient"->2|>,tmp,42]]];
rec["CacheRead",RGZGaugeFixedCacheRead[tmp,42]===<|"ResidualCoefficient"->2|>];
pkt=Import[tmp<>".wxf","WXF"];rec["ValidPacket",RGZGaugeFixedValidPacketQ[pkt,42]];
rec["AlteredPacketRejected",!RGZGaugeFixedValidPacketQ[Join[pkt,<|"Result"-><|"ResidualCoefficient"->0|>|>],42]];
rec["LegacyRejected",!RGZGaugeFixedValidPacketQ[<|"Identity"->42,"Result"->0|>,42]];
r=RGZGaugeFixedPair[Join[unit,<|"AlphaShortCoefficient"->Pi^2 SMP["g_s"]^2 \[Alpha]|>],10,"DirectThreshold"->0,"CacheDirectory"->None];
rec["FixedPath",AssociationQ[r]&&r["CoefficientStatus"]==="Nonzero"];
rgzFDCache=CreateDirectory[];
r=RGZGaugeFixedCompare[A0[rgzFDTestMass],1,10,"CacheDirectory"->rgzFDCache];
rec["CurrentCoefficientCacheSaved",AssociationQ[r]&&Length[FileNames["coefficient_*.wl",rgzFDCache,Infinity]]>0];
r=RGZGaugeFixedCompare[A0[rgzFDTestMass],1,10,"CacheDirectory"->rgzFDCache];
rec["CurrentCoefficientCacheReused",AssociationQ[r]&&AllTrue[r["ResidualParts"],#["Source"]==="OriginalCoefficientCacheRestored"&]];
(* Synthetic dispatch fixtures isolate report binding; these are not certificates. *)
Internal`InheritedBlock[{RGZPTJobCheck,RGZLoopColour,RGZGaugeFixedCompare},
 Clear[RGZPTJobCheck,RGZLoopColour,RGZGaugeFixedCompare];
 RGZPTJobCheck[]:=True;RGZLoopColour[x_]:=x;
 Block[{rgzPTJob=<|"Environment"->1,"Mode"->"FourPinch","NAVMode"->"AAReplacement","NAVFingerprint"->None,
   "SelfEnergyParts"->{0},"PartsOrder"->{"box"},"Parts"-><|"box"-><|"OriginalExpression"->1,"Result"-><|"Scalar"->1|>,"StageProvenance"-><||>|>|>|>},
 RGZGaugeFixedCompare[___]:=<|"Status"->"Fixture"|>;
 r=RGZPTFixedCompare[1,10,"CacheDirectory"->None];
 rec["PTReportBound",AssociationQ[r]&&r["PTSourceIdentity"]===RGZWorkflow`Private`ptComparisonIdentity[rgzPTJob]&&r["FullNAVIncluded"]===False];
 Clear[RGZGaugeFixedCompare];RGZGaugeFixedCompare[___]:=(AssociateTo[rgzPTJob,"SelfEnergyParts"->{1}];<|"Status"->"Fixture"|>);
 rec["PTMutationRejected",MatchQ[RGZPTFixedCompare[1,10,"CacheDirectory"->None],Failure["PTComparisonSourceChanged",_]]]
 ]];
Print["SUMMARY ",checks];Exit[If[And@@Values[checks],0,1]];
