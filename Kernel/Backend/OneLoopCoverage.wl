(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Action-side inventory, independent of the mixed-loop graph enumerator.
   This does not assert a bijection with saved loop graphs. *)
ClearAll[rgzCoverageFieldsIn,rgzCoverageSignature];
rgzCoverageFields=DeleteDuplicates[Flatten[({ClassName/.#[[2]],anti[ClassName/.#[[2]]]}& /@ M$ClassesDescription)]];
rgzCoverageName[f_Symbol]:=StringReplace[SymbolName[Unevaluated[f]],{"\[Phi]"->"ϕ"}];
rgzCoverageFieldsIn[x_] := Which[
 MemberQ[rgzCoverageFields,Head[x]],{rgzCoverageName[Head[x]]},
 MatchQ[x,Power[_,_Integer?Positive]],Flatten[ConstantArray[rgzCoverageFieldsIn[x[[1]]],x[[2]]]],
 AtomQ[x],{},True,Flatten[rgzCoverageFieldsIn /@ List@@x]];
rgzCoverageSignature[x_]:=Sort[rgzCoverageFieldsIn[x]];
rgzCoverageAction=Expand[GetInteractionTerms[LTotal]];
rgzCoverageTerms=If[Head[rgzCoverageAction]===Plus,List@@rgzCoverageAction,{rgzCoverageAction}];
rgzCoverageGroups=GatherBy[rgzCoverageTerms,rgzCoverageSignature];
rgzCoverageActionInventory=Map[<|"Fields"->rgzCoverageSignature[First[#]],"Legs"->Length[rgzCoverageSignature[First[#]]],"Monomials"->Length[#]|>&,rgzCoverageGroups];
rgzCoverageExportSignatures=Sort[DeleteDuplicates[Sort[rgzCoverageName /@ #[[1,All,1]]]& /@ rgzCoverageExport]];
rgzCoverageRelevant=Select[rgzCoverageActionInventory,MemberQ[{3,4},#["Legs"]]&];
rgzCoverageMissing=Complement[Lookup[rgzCoverageRelevant,"Fields"],rgzCoverageExportSignatures];
(* Action monomials may cancel by IBP/antisymmetric color. Require native FR
   to prove that each absent field signature has no vertex, not a guessed zero. *)
rgzCoverageZeros=Table[Module[{part,v},
 part=Total[Select[rgzCoverageTerms,rgzCoverageSignature[#]===sig&]];
 v=FeynmanRules[part,FlavorExpand->True,MinParticles->3,MaxParticles->4,ConservedQuantumNumbers->False,ScreenOutput->False];
 <|"Fields"->sig,"VanishingVertexVerified"->TrueQ[v==={}]|>],{sig,rgzCoverageMissing}];
rgzCoverageAh=Expand[Ah[rgzCoverageLorentz,rgzCoverageColour]];
rgzCoverageAhDegrees=Sort[DeleteDuplicates[Length[rgzCoverageFieldsIn[#]]& /@ If[Head[rgzCoverageAh]===Plus,List@@rgzCoverageAh,{rgzCoverageAh}]]];
rgzCoverageQuarkSignatures=Select[rgzCoverageExportSignatures,AnyTrue[#,StringStartsQ["Q"]]&];
rgzCoverageGhostFields={eta,etabar,\[Omega],\[Omega]bar,\[Zeta],\[Zeta]bar,c,cbar};
rgzCoverageGhostBars={etabar,\[Omega]bar,\[Zeta]bar,cbar};
rgzCoverageGhostParticles=Complement[rgzCoverageGhostFields,rgzCoverageGhostBars];
rgzCoverageGhostSeagulls=Select[rgzCoverageTerms,Length[rgzCoverageSignature[#]]===4&&!FreeQ[#,x_[___]/;MemberQ[rgzCoverageGhostFields,x]]&];
rgzCoverageGhostTadpoleRows=Map[Function[term,<|"Fields"->rgzCoverageSignature[term],
 "OneAntighostDerivative"->(Length[Cases[term,del[x_[___],_]/;MemberQ[rgzCoverageGhostBars,x],{0,Infinity}]]===1),
 "NoGhostDerivative"->FreeQ[term,del[x_[___],_]/;MemberQ[rgzCoverageGhostParticles,x]],
 "Reason"->"Integral q_mu f(q^2)=0 in translation-invariant dimensional regularization; spectator tensor propagators are even."|>],rgzCoverageGhostSeagulls];
rgzCoverageChecks=<|"OnlyMinimalQuarkInteractions"->(Sort[rgzCoverageQuarkSignatures]===Sort[Table[Sort[{"A","Q"<>ToString[n],"Q"<>ToString[n]<>"bar"}],{n,2}]]),
 "GhostSeagullTadpolesOdd"->(Length[rgzCoverageGhostTadpoleRows]>0&&AllTrue[rgzCoverageGhostTadpoleRows,TrueQ[#["OneAntighostDerivative"]&&#["NoGhostDerivative"]]&]),
 "AhContainsDegreesOneToThree"->(rgzCoverageAhDegrees==={1,2,3}),
 "AllExportedSignaturesHaveActionTerms"->(Complement[rgzCoverageExportSignatures,Lookup[rgzCoverageRelevant,"Fields"]]==={}),
 "EveryUnexportedActionSignatureProvedZero"->AllTrue[rgzCoverageZeros,TrueQ[#["VanishingVertexVerified"]]&],
 "ExportOnlyThreeOrFourLegs"->AllTrue[rgzCoverageExportSignatures,MemberQ[{3,4},Length[#]]&],
 "TwoQuarkCubicVertices"->And@@(MemberQ[rgzCoverageExportSignatures,Sort[{"A","Q"<>ToString[#],"Q"<>ToString[#]<>"bar"}]]& /@{1,2}),
 "EtaCubicVertex"->MemberQ[rgzCoverageExportSignatures,Sort[{"A","eta","etabar"}]]|>;
rgzCoverageTopologies={
 <|"Object"->"Mixed bosonic self energy","ExternalLegs"->2,"Loops"->1,"Valences"->{{4},{3,3}},"Sectors"->{"BosonTadpole","BosonBubble","GhostBubble","QuarkBubble"},"Note"->"Ghost seagulls exist: their single derivative on the internal antighost gives an odd q tadpole, zero in translation-invariant dimensional regularization. No quark seagull."|>,
 <|"Object"->"Quark-gluon vertex","ExternalLegs"->3,"Loops"->1,"Valences"->{{3,3,3},{3,4},{5}},"ImplementedSurvivors"->{"AV","FullMatrixNAV"},"Note"->"Higher valences would require unavailable multi-quark contacts or a five-leg vertex containing two quarks."|>,
 <|"Object"->"Connected qqprime amplitude","ExternalLegs"->4,"Loops"->1,"MaximumValence"->6,"Survivors"->{"MixedSelfInsertion","AV1","AV2","NAV1","NAV2","Box1","Box2"},"ExternalLSZ"->"four half-residues","Note"->"E=4 alone does NOT prove four-leg truncation; use minimal quark coupling and attachments."|>};
rgzCoveragePacket=<|"Schema"->"RGZOneLoopActionCoverage/1","Checks"->rgzCoverageChecks,
 "ActionVertexInventoryVerified"->AllTrue[Values[rgzCoverageChecks],TrueQ],
 "VertexInventory"->rgzCoverageRelevant,"ExportedFieldSignatures"->rgzCoverageExportSignatures,"VanishingActionSignatures"->rgzCoverageZeros,
 "HigherLegActionInventory"->Select[rgzCoverageActionInventory,#["Legs"]>4&],"AhFieldDegrees"->rgzCoverageAhDegrees,
 "CountingIdentity"->"Sum_v (n_v-2) = E+2L-2; quadratic mixings resummed in K^-1",
 "LoopCounting"->"hbar expansion at fixed m^2,mu^2,mG^4; not a Taylor series in gs at fixed gamma",
 "AhTruncationRequirements"->{"tau.Ah and horizon: Ah degree <=3","ghost pair.Ah: Ah degree <=2","Ah^2: degree combinations (1,1),(1,2),(1,3),(2,2)"},
 "GhostSeagullTadpoleRows"->rgzCoverageGhostTadpoleRows,"TopologyRequirements"->rgzCoverageTopologies,"GraphEnumerationIndependentlyVerified"->False,
 "Scope"->"Action signatures and structural one-loop bounds. No independent labeled-graph bijection, symmetry-factor or ST certificate."|>;
