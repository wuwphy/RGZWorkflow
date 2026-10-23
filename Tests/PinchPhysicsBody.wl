(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
checks=<||>;
record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",checks[n]]);
fastProject[e_]:=Block[{FullSimplify=Identity},RGZQCDProject[Expand[e]]];
zero[e_]:=TrueQ[Cancel[Together[ExpandScalarProduct[e]]]===0];
parts=FourPiecesGF[p,k,\[Mu],\[Nu],alpha,alpha0];
weighted=parts["Box"]+2parts["NAV_one"]+2parts["AV_one"]+4parts["QSE_one"];
total=TotalGF[p,k,\[Mu],\[Nu],alpha,alpha0];
firstDifference=fastProject[weighted-total];record["AppendixWeightedSumEqualsTotal",zero[firstDifference]];
secondDifference=fastProject[FourPinch[p,k,\[Mu],\[Nu],alpha]-FourPinch[p,k,\[Mu],\[Nu],alpha0]-total];record["FourPinchDifferenceEqualsGaugeDifference",zero[secondDifference]];
record["CFExactlyCancels",zero[Coefficient[fastProject[weighted],CF]]];
record["SameGaugeZero",zero[fastProject[total/.alpha0->alpha]]];
record["ReferenceProductCrossTerms",zero[dBBTrgz[k,k+p,alpha,alpha0]-(BTrgz[k,alpha0]dBTrgz[k+p,alpha,alpha0]+dBTrgz[k,alpha,alpha0]BTrgz[k+p,alpha0]+dBTrgz[k,alpha,alpha0]dBTrgz[k+p,alpha,alpha0])]];
corrected=RGZCandidatePinchesRational[];
paramRules={m2->m^2,mu2->\[Mu]^2,mg4->mG^4,reg4->M^4,alpha->\[Alpha],alpha0->0};
matched=Map[(#/.paramRules/.k->-q)&,parts];
record["LandauNAVEqualsIndependentCorrection",zero[fastProject[2matched["NAV_one"]-corrected["nav全部"]]]];
record["LandauAVEqualsIndependentKernel",zero[fastProject[2matched["AV_one"]-corrected["av全部"]]]];
record["LandauQSEEqualsIndependentKernel",zero[fastProject[4matched["QSE_one"]-corrected["qse全部"]]]];
(* Box equality uses the allowed k <-> l integral routing. *)
record["LandauBoxMatchesReviewedRoute",zero[fastProject[matched["Box"]-corrected["box"]]]];
rq=total/.{m2->0,mu2->0,mg4->0,reg4->0,alpha0->1,alpha->1-lambda};
s=SPD[p,p];r=SPD[k,k]-SPD[p,k]^2/s;
expected=SMP["g_s"]^2 CA(-lambda^2 r/(2(D-1)SPD[k,k]^2 SPD[k+p,k+p]^2)+lambda/(SPD[k,k]^2 SPD[k+p,k+p])+2lambda r/(s(D-1)SPD[k,k]^2 SPD[k+p,k+p])-lambda/(s SPD[k,k]^2));
record["QCDTotalMatchesEquation8Point2",zero[fastProject[rq]-expected]];
gp=GammaPPair[p,k,\[Mu],\[Nu]]/.{m2->0,mu2->0,mg4->0};
record["GammaPMirrorNormalizationQCD",zero[s^2 fastProject[gp]-2SMP["g_s"]^2 CA s/(SPD[k,k]SPD[k+p,k+p])]];
record["NAVWeightIdentity",zero[(SPD[k+p,k+p]ATrgz[k+p]-1)-(m2(SPD[k+p,k+p]-mu2)-mg4)/((SPD[k+p,k+p]-m2)(SPD[k+p,k+p]-mu2)+mg4)]];
record["WeightedSumFullTensorEquality",zero[Contract[Expand[weighted-total]]]];
record["GaugeDifferenceFullTensorEquality",zero[Contract[Expand[FourPinch[p,k,\[Mu],\[Nu],alpha]-FourPinch[p,k,\[Mu],\[Nu],alpha0]-total]]]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
full=RGZBuildPinches["Mode"->"FourPinch","Representation"->"Rational"];
diff1=RGZBuildPinches["Mode"->"GaugeDifference","ReferenceGauge"->1,"Representation"->"Rational"];
mapPDF={m2->m^2,mu2->\[Mu]^2,mg4->mG^4,reg4->M^4,alpha->\[Alpha],alpha0->1};
fullPDF=FourPinch[p,k,\[Mu],\[Nu],alpha]/.mapPDF/.k->-q;
diffPDF=TotalGF[p,k,\[Mu],\[Nu],alpha,alpha0]/.mapPDF/.k->-q;
record["FullTotalMatchesReviewedPDF",zero[Contract[Expand[full["Total"]-fullPDF]]]];
record["ReferenceOneMatchesReviewedPDF",zero[Contract[Expand[diff1["Total"]-diffPDF]]]];
record["GammaPExplicit",!TrueQ[full["GammaP"]===0]];
record["ReferenceOneRetainsFiniteBaseline",!zero[diff1["Total"]-RGZBuildPinches["Mode"->"GaugeDifference","Representation"->"Rational"]["Total"]]];
record["InvalidModeIsFailure",FailureQ[RGZBuildPinches["Mode"->"bad"]]];
record["GaugeParameterIsNotFixedReference",FailureQ[RGZBuildPinches["Mode"->"GaugeDifference","ReferenceGauge"->\[Alpha]]]];
Do[
 ff=RGZBuildPinches["Mode"->auditMode,"ReferenceGauge"->1,"Representation"->"FAD"];
 rt=RGZBuildPinches["Mode"->auditMode,"ReferenceGauge"->1,"Representation"->"Rational"];
 Do[
  fd=RGZLoopPoleMasses[fastProject[FeynAmpDenominatorExplicit[ff[name]]]]/.{m->1,\[Mu]->2,mG->2,M->1,\[Alpha]->3};
  rd=fastProject[rt[name]]/.{m->1,\[Mu]->2,mG->2,M->1,\[Alpha]->3};
  record["FiniteFADSample:"<>auditMode<>name,zero[fd-rd]],
 {name,{"box","nav全部","av全部","qse全部"}}],{auditMode,{"FourPinch","GaugeDifference"}}];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Exit[If[And@@Values[checks],0,1]];
