(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Generate the quark probe graphs, attach the full NAV and reduce open-spin-chain integrands.

   Inputs:
   The checked tree/model data and the graph or task data named by the called function.

   Dependencies:
   Tree/ definitions and Algebra/ tools; Internal/ supplies configuration and persistence.
*)

(* Complete conventional probe graphs, not the four pinch kernels.
   Distinct massless quarks; closed quark loops are supplied by the base-model mixed self energy.
   The auxiliary NAV is supplied by the full field-matrix engine. *)
rgzQQMomenta={qqK1,qqK2,qqK3,qqK4};
rgzQQInvariantRules={SPD[qqK1,qqK1]->0,SPD[qqK2,qqK2]->0,SPD[qqK3,qqK3]->0,SPD[qqK4,qqK4]->0,
 SPD[qqK1,qqK2]->qqS/2,SPD[qqK3,qqK4]->qqS/2,SPD[qqK1,qqK3]->-qqT/2,SPD[qqK2,qqK4]->-qqT/2,
 SPD[qqK1,qqK4]->(qqS+qqT)/2,SPD[qqK2,qqK3]->(qqS+qqT)/2};
rgzQQGeneratorSource[] := Module[{s,old,new},
 s=Import[RGZWorkflow`Private`modelBase[]<>".gen","Text"];
 old="  AnalyticalPropagator[Internal][\n    s1 V[j1, mom, {li1} -> {li2}]\n  ] ==\n    Global`FAMetricTensor[li1, li2],";
 new="  AnalyticalPropagator[Internal][\n    s1 V[j1, mom, {li1} -> {li2}]\n  ] == -I ((Global`FAScalarProduct[mom,mom]-RGZWorkflow`Engine`\\[Mu]^2)/((Global`FAScalarProduct[mom,mom]-RGZWorkflow`Engine`m^2)(Global`FAScalarProduct[mom,mom]-RGZWorkflow`Engine`\\[Mu]^2)+RGZWorkflow`Engine`mG^4) (Global`FAMetricTensor[li1,li2]-Global`FAFourVector[mom,li1] Global`FAFourVector[mom,li2]/Global`FAScalarProduct[mom,mom])+RGZWorkflow`Engine`\\[Alpha] Global`FAFourVector[mom,li1] Global`FAFourVector[mom,li2]/(Global`FAScalarProduct[mom,mom]^2+RGZWorkflow`Engine`\\[Alpha] RGZWorkflow`Engine`M^4)),";
 If[!StringContainsQ[s,old],Return[Failure["ProbePropagatorTemplateChanged",<||>]]];StringReplace[s,old->new]];
(* FR$DSign=+1: D=partial-I g A, so i bar(psi) gamma.D psi gives +I g gamma T. *)
rgzQQModelSource="IndexRange[Index[Global`Colour]]=NoUnfold[Range[3]];\nIndexRange[Index[Global`Gluon]]=NoUnfold[Range[8]];\nM$ClassesDescription={F[1]=={SelfConjugate->False,Indices->{Index[Global`Colour]},Mass->0,PropagatorType->Straight,PropagatorArrow->Forward},F[2]=={SelfConjugate->False,Indices->{Index[Global`Colour]},Mass->0,PropagatorType->Straight,PropagatorArrow->Forward},V[1]=={SelfConjugate->True,Indices->{Index[Global`Gluon]},Mass->0,PropagatorType->Cycles,PropagatorArrow->None}};\nM$CouplingMatrices={C[-F[1,{c1}],F[1,{c2}],V[1,{a}]]==I Global`FAGS Global`FASUNT[a,c1,c2] {{1,0},{1,0}},C[-F[2,{c1}],F[2,{c2}],V[1,{a}]]==I Global`FAGS Global`FASUNT[a,c1,c2] {{1,0},{1,0}}};\n";
rgzQQStripSums[expr_] := Module[{sums,body},
 sums=DeleteDuplicates@Cases[expr,_FeynArts`SumOver,Infinity];body=expr/._FeynArts`SumOver->1;
 If[!AllTrue[sums,Function[so,MemberQ[Rest[List@@so],FeynArts`External] || Count[body,First[so],Infinity]===2]],
 Return[Failure["QuarkColourSumAudit",<|"Sums"->sums|>]]];body];
(* Installed FA 3.12 has Index[i_]:>i_ in coeffDef. Fix only the
   scoped probe initialization, constructing the intended index pattern
   dynamically; retain the library's definitions afterward. *)
SetAttributes[rgzQQFAInitialize,HoldAll];
rgzQQFAInitialize[body_] := Internal`InheritedBlock[{FeynArts`Initialize`coeffDef},
 Clear[FeynArts`Initialize`coeffDef];
 FeynArts`Initialize`coeffDef[_[_[rgzQQFj_]],rgzQQFi_] :=
  Block[{rgzQQIj=FeynArts`Indices[rgzQQFj]},
   (FeynArts`TheCoeff[#1]=#2)&[
    rgzQQFj/.FeynArts`Index[rgzQQIndex_]:>Apply[Pattern,{rgzQQIndex,Blank[]}],
    Flatten[rgzQQFi]/.Thread[rgzQQIj->Level[rgzQQIj,{2}]]]];
 body];
rgzQQProbeGraphs[dir_,seconds_]:=rgzQQFAInitialize[Module[{base=FileNameJoin[{dir,"Probe"}],source,top,ins,fa,converted,tree,se,convert},
 source=rgzQQGeneratorSource[];If[FailureQ[source],Return[source]];
 Export[base<>".gen",source,"Text"];Export[base<>".mod",rgzQQModelSource,"Text"];
 convert[amp_,in_,out_]:=FCFAConvert[amp,IncomingMomenta->in,OutgoingMomenta->out,LoopMomenta->{q},ChangeDimension->D,List->True,Contract->False,DropSumOver->False,UndoChiralSplittings->True];
 top=CreateTopologies[1,2->2,ExcludeTopologies->{SelfEnergies,WFCorrections,Tadpoles}];
 ins=InsertFields[top,{F[1],F[2]}->{F[1],F[2]},Model->base,GenericModel->base,InsertionLevel->{Classes}];
 fa=CreateFeynAmp[ins,Truncated->False,PreFactor->1,GaugeRules->{}];
 If[Length[fa]=!=4,Return[Failure["ProbeGraphCount",<|"Expected"->4,"Actual"->Length[fa]|>]]];
 converted=rgzQQStripSums /@ convert[fa,Take[rgzQQMomenta,2],Take[rgzQQMomenta,-2]];If[AnyTrue[converted,FailureQ],Return[First[Select[converted,FailureQ]]]];
 top=CreateTopologies[0,2->2];ins=InsertFields[top,{F[1],F[2]}->{F[1],F[2]},Model->base,GenericModel->base,InsertionLevel->{Classes}];
 tree=convert[CreateFeynAmp[ins,Truncated->False,PreFactor->1,GaugeRules->{}],Take[rgzQQMomenta,2],Take[rgzQQMomenta,-2]];
 top=CreateTopologies[1,1->1,ExcludeTopologies->{Tadpoles}];ins=InsertFields[top,F[1]->F[1],Model->base,GenericModel->base,InsertionLevel->{Classes}];
 se=convert[CreateFeynAmp[ins,Truncated->True,PreFactor->1,GaugeRules->{}],{qqR},{qqR}];
 If[Length[tree]=!=1||Length[se]=!=1,Return[Failure["ProbeTreeOrSelfEnergyCount",<|"TreeCount"->Length[tree],"SelfEnergyCount"->Length[se],"Tree"->tree,"SelfEnergy"->se|>]]];
 <|"Tree"->rgzQQStripSums[First[tree]],"AVAndBoxes"->converted,"QuarkSelfEnergy"->rgzQQStripSums[First[se]],"RawFeynArts"->fa,
 "Source"->"Independent FeynArts four-quark graphs with physical full AA propagator; no pinch extraction", "Measure"->"d^D q/(2 Pi)^D; PreFactor=1", "ContainsClosedSelfEnergyLoops"->False,"SelfEnergySource"->"BaseModelMixedMatrices"|>]];
(* Only fundamental-colour indices are renamed. Never traverse arbitrary
   backend symbols: some have OwnValues containing parameter associations. *)
rgzQQNormalizeColours[expr_] := expr /. HoldPattern[SUNFIndex[rgzQQColourIndex_Symbol]] :>
 With[{rgzQQColourName=SymbolName[Unevaluated[rgzQQColourIndex]]},
  If[StringMatchQ[rgzQQColourName,"Col" ~~ (DigitCharacter ..)],
   SUNFIndex[Symbol["RGZWorkflow`Engine`qqC"<>StringDrop[rgzQQColourName,3]]],
   SUNFIndex[rgzQQColourIndex]]];
rgzQQColour[expr_,left_,right_]:=expr/.HoldPattern[SUNT[a__]]:>SUNTF[{a},left,right];
rgzQQCurrent[mu_,left_,right_]:=SpinorUBarD[left,0].GAD[mu].SpinorUD[right,0];
rgzQQBornCurrent[mu_,nu_]:=rgzQQCurrent[mu,qqK3,qqK1] rgzQQCurrent[nu,qqK4,qqK2] SUNTF[{qqA},qqC3,qqC1] SUNTF[{qqA},qqC4,qqC2];
rgzQQNAV[nav_,line_Integer]:=Module[{rout,rin,coOut,coIn,otherOut,otherIn,otherCoOut,otherCoIn,tensor,transfer,chain},
 {rout,rin,coOut,coIn,otherOut,otherIn,otherCoOut,otherCoIn}=If[line===1,{qqK3,qqK1,qqC3,qqC1,qqK4,qqK2,qqC4,qqC2},{qqK4,qqK2,qqC4,qqC2,qqK3,qqK1,qqC3,qqC1}];
 transfer=rout-rin;
 tensor=nav["TransverseIntegrand"]/.{rgzNAVIncoming->rin,p->transfer};
 tensor=rgzQQColour[tensor,coOut,coIn];
 chain=SpinorUBarD[rout,0].tensor.SpinorUD[rin,0];
 I SMP["g_s"] SUNTF[{rgzNAVA},otherCoOut,otherCoIn] rgzQQCurrent[rgzNAVNu,otherOut,otherIn] chain];
(* Split physical boson denominators route by route, keeping all independent
   four-point momenta. The old two-point-only RGZMXSplit is NOT used. *)
rgzQQPoleFactor[den_,var_,route_,gauge_,reg_]:=Block[{$Assumptions=True},Module[
 {r3,r4,roots,out=0,back=0,degree,r,order,quot,c,mass,emitted,probe=Unique["qqPoleProbe$"]},
 If[Or@@{OwnValues[qqPVm3]=!={},OwnValues[qqPVm4]=!={},OwnValues[qqPVl1]=!={},OwnValues[qqPVl2]=!={}},
  Return[Failure["AssignedQuarkPoleLabel",<||>]]];
 If[!RGZLoopValidValueQ[{den,var,route,gauge,reg}] || !PolynomialQ[den,var],Return[Failure["InvalidQuarkPoleInput",<||>]]];
 r3=(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4];r4=(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4];
 roots=DeleteDuplicatesBy[{{0,0},{\[Mu]^2,\[Mu]},{r3,Sqrt[r3]},{r4,Sqrt[r4]},
 {-I Sqrt[gauge reg^4],Sqrt[-I Sqrt[gauge reg^4]]},{I Sqrt[gauge reg^4],Sqrt[I Sqrt[gauge reg^4]]}},First];
 degree=Exponent[den,var];
 Do[{r,mass}=pole;If[TrueQ[Cancel[Expand[den/.var->r]]===0],
 order=1;While[order<degree&&TrueQ[Cancel[Expand[D[den,{var,order}]/.var->r]]===0],order++];
 quot=PolynomialQuotient[den,(var-r)^order,var];
 Do[c=Cancel[Together[(D[1/quot,{var,order-j}]/.var->r)/Factorial[order-j]]];back+=c/(var-r)^j;
 out+=c FCI[FAD@@ConstantArray[{route,Which[SameQ[r,r3],qqPVm3,SameQ[r,r4],qqPVm4,SameQ[r,-I Sqrt[gauge reg^4]]&&!TrueQ[r===0],qqPVl1,SameQ[r,I Sqrt[gauge reg^4]]&&!TrueQ[r===0],qqPVl2,True,mass]},j]],{j,order}]],{pole,roots}];
 If[!RGZLoopValidValueQ[back] || !TrueQ[Cancel[Together[1/den-back]]===0],Return[Failure["QuarkPoleReconstruction",<|"Denominator"->den|>]]];
 With[{momentum=MomentumExpand[FCI[Momentum[route,D]]]},
  emitted=out/.HoldPattern[PropagatorDenominator[momentum,label_]]:>PropagatorDenominator[Momentum[probe,D],label]];
 emitted=FeynAmpDenominatorExplicit[rgzQQRestorePoleMasses[emitted,gauge,reg]]/.
  Pair[Momentum[probe,D],Momentum[probe,D]]->var;
 If[!RGZLoopValidValueQ[{out,emitted}] || !TrueQ[Cancel[Together[1/den-emitted]]===0],
  Return[Failure["QuarkEmittedPoleReconstruction",<|"Denominator"->den|>]]];out]];
rgzQQSplit[expr_,gauge_,reg_]:=Module[{convert,stop=Unique["qqSplitStop"],result},Catch[
 If[!RGZLoopValidValueQ[{expr,gauge,reg}],Return[Failure["InvalidQuarkSplitInput",<||>]]];
 convert[den_,n_]:=Module[{squares,var,route,poly,z},
 squares=DeleteDuplicates@Cases[den,HoldPattern[Pair[Momentum[v_,D],Momentum[v_,D]]]/;!FreeQ[v,q]:>v,{0,Infinity}];
 If[Length[squares]=!=1,Throw[Failure["QuarkDenominatorRouting",<|"Denominator"->den|>],stop]];
 route=First[squares];var=Unique["qqPole"];
 poly=den/.FCI[SPD[route,route]]->var;
 If[!PolynomialQ[poly,var],Throw[Failure["QuarkNonPolynomialDenominator",<||>],stop]];
 z=rgzQQPoleFactor[poly^n,var,route,gauge,reg];If[FailureQ[z],Throw[z,stop]];z];
 result=FCI[expr]/.HoldPattern[Power[den_,n_Integer]]/;n<0&&!FreeQ[den,Pair[Momentum[v_,D],Momentum[v_,D]]/;!FreeQ[v,q]]:>convert[den,-n];
 FeynAmpDenominatorCombine[Expand[result]],stop]];
(* Momentum conservation must be applied separately to each open chain.
   A global replacement of a spinor momentum destroys its on-shell identity. *)
rgzQQChainConservation[d_Dot]:=Module[{first=First[d],last=Last[d],v=d},
 If[MatchQ[first,Spinor[Momentum[qqK3,D],0,1]]&&MatchQ[last,Spinor[Momentum[qqK1,D],0,1]],
 v=v/.DiracGamma[Momentum[qqK4,D],D]->GSD[qqK1]+GSD[qqK2]-GSD[qqK3]];
 If[MatchQ[first,Spinor[Momentum[qqK4,D],0,1]]&&MatchQ[last,Spinor[Momentum[qqK2,D],0,1]],
 v=v/.DiracGamma[Momentum[qqK3,D],D]->GSD[qqK1]+GSD[qqK2]-GSD[qqK4]];v];
rgzQQWardSimplify[x_]:=Module[{z},
 z=DiracSimplify[SUNSimplify[Contract[FCI[x]],SUNNToCACF->False]];
 z=ExpandScalarProduct[z]/.FCI[rgzQQInvariantRules];
 z=z/.Pair[Momentum[q,D],Momentum[qqK4,D]]->SPD[q,qqK1]+SPD[q,qqK2]-SPD[q,qqK3];
 z=z/.d_Dot:>rgzQQChainConservation[d];
 Cancel[Together[FCCanonicalizeDummyIndices[DiracSimplify[z],LorentzIndexNames->{Unique["qqWardLor"]},SUNIndexNames->{Unique["qqWardColour"]}]]]];
rgzQQClassify[graphs_List]:=Module[{names,result,chains,counts,line},
 names=Map[Function[g,
 chains=Cases[g,d_Dot/;!FreeQ[d,Spinor]:>d,Infinity];
 counts=Count[#, _DiracGamma,Infinity]& /@ chains;
 If[Length[chains]=!=2,Return["Unsupported"]];
 If[Sort[counts]==={1,5},
 line=First[Pick[chains,counts,5]];
 If[!FreeQ[First[line],qqK3],"AV1","AV2"],
 If[Sort[counts]==={3,3},"Box","Unsupported"]]],graphs];
 If[Count[names,"AV1"]=!=1||Count[names,"AV2"]=!=1||Count[names,"Box"]=!=2,
 Return[Failure["QQGraphClassification",<|"Classes"->names|>]]];
 (* The two boxes are named by their FeynArts insertion order; both are always included. *)
 names=ReplacePart[names,Thread[Flatten[Position[names,"Box"]]->{"Box1","Box2"}]];
 AssociationThread[names,graphs]];

rgzQQRestorePoleMasses[expr_,gauge_,reg_]:=expr/.{
 qqPVm3->Sqrt[(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4]],
 qqPVm4->Sqrt[(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4]],
 qqPVl1->Sqrt[-I Sqrt[gauge reg^4]],qqPVl2->Sqrt[I Sqrt[gauge reg^4]]};
(* On-shell kinematics are scoped: never change the notebook's scalar products. *)
rgzQQReduce[expr_] := Internal`InheritedBlock[{Pair,ScalarProduct,SP,SPD,SPE,SPLR,SPLRD},
 SPD[qqK1,qqK1]=0;SPD[qqK2,qqK2]=0;SPD[qqK3,qqK3]=0;SPD[qqK4,qqK4]=0;
 SPD[qqK1,qqK2]=qqS/2;SPD[qqK3,qqK4]=qqS/2;
 SPD[qqK1,qqK3]=-qqT/2;SPD[qqK2,qqK4]=-qqT/2;
 SPD[qqK1,qqK4]=(qqS+qqT)/2;SPD[qqK2,qqK3]=(qqS+qqT)/2;
 If[!RGZLoopValidValueQ[expr],Return[Failure["InvalidQuarkReductionInput",<||>]]];
 TID[expr,q,ToPaVe->True,UsePaVeBasis->True,ApartFF->False,PaVeAutoReduce->False]];

(* Gauge limits act on collected PV coefficients, after coalescing masses have
   been treated. Direct alpha->0 in a pole decomposition creates spurious 0/0. *)
rgzQQGaugeValue[expr_,gauge_,seconds_] := Module[{pairs,result,terms={},co,stop=Unique["qqGaugeStop"]},Catch[
 pairs=RGZGaugePVPairs[expr];If[FailureQ[pairs],Return[pairs]];
 Do[If[!FreeQ[pair[[1]],\[Alpha]],Throw[Failure["QQGaugeMassLimitRequired",<|"Master"->pair[[1]]|>],stop]];
 co=TimeConstrained[If[FreeQ[pair[[2]],\[Alpha]],pair[[2]],If[TrueQ[gauge===0],rgzQQGaugeZeroCoefficient[pair[[2]]],pair[[2]]/.\[Alpha]->gauge]],seconds,$TimedOut];
 If[!FreeQ[co,Indeterminate|ComplexInfinity|_DirectedInfinity|$TimedOut|_Limit],Throw[Failure["QQGaugeLimitIncomplete",<||>],stop]];
 AppendTo[terms,pair[[1]] co],{pair,pairs}];Total[terms],stop]];

