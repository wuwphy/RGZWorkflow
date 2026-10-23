(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* A verification ledger: missing functional kernels never count as passes. *)
RGZIdentityRequirements[] := {
 <|"ID"->"ClassicalBRST","Identity"->"s^2=0; s Ah=0; s S=0 (with regulator source)","Scope"->"SU(3), BCH through g^2; local auxiliary algebra"|>,
 <|"ID"->"QuarkWard","Identity"->"q.gamma = S0^-1(p+q)-S0^-1(p)","Scope"->"Bare quark line, exact Clifford algebra"|>,
 <|"ID"->"PropagatorConstraint","Identity"->"K D=i 1; longitudinal Ah multiplier contact identities", "Scope"->"Actual FR quadratic kernel and FA propagators"|>,
 <|"ID"->"NAVReconstruction","Identity"->"V = right longitudinal + left longitudinal + genuine remainder", "Scope"->"All full-matrix NAV connections; algebraic decomposition"|>,
 <|"ID"->"MixedVertexST","Identity"->"delta_c delta_Phi_i delta_Phi_j S(Gamma)=0", "Scope"->"Mixed vertices, two-point inverse matrix, ghost/antifield kernels"|>,
 <|"ID"->"QuarkVertexST","Identity"->"q.Gamma=g F (S^-1 H-Hbar S^-1)", "Scope"->"One-loop quark self energy, ghost dressing and scattering kernels"|>,
 <|"ID"->"OneLoopST","Identity"->"B_S Gamma^(1)=0", "Scope"->"Complete one-loop effective action including BRST source insertions"|>,
 <|"ID"->"Nielsen","Identity"->"partial_alpha Gamma=-B_Gamma partial_chi Gamma", "Scope"->"s alpha=chi; source insertions, regulator doublet retained"|>,
 <|"ID"->"CompositeAh","Identity"->"partial_alpha <Ah Ah>=0", "Scope"->"Full nonlinear composite insertions and physical regulator limit"|>
};
identityEvidence[id_,status_,detail_] := <|"ID"->id,"Status"->status,"Detail"->detail|>;
RGZCheckIdentities[seconds_:300] := Module[{r,algebra,ward,rows,props,nav,connections},
 r=ready[];If[FailureQ[r],Return[r]];
 algebra=timed["BRSTAlgebra",Get[FileNameJoin[{backendDirectory[],"RGZ_Identity_Algebra.wl"}]],seconds];
 ward=timed["QuarkWard",Module[{gam,z2,id2,v,w,m,slash},
 z2=ConstantArray[0,{2,2}];id2=IdentityMatrix[2];
 gam=Join[{ArrayFlatten[{{id2,z2},{z2,-id2}}]},Table[ArrayFlatten[{{z2,PauliMatrix[j]},{-PauliMatrix[j],z2}}],{j,3}]];
 slash[x_]:=x[[1]]gam[[1]]-Sum[x[[j+1]]gam[[j+1]],{j,3}];
 v=Array[Unique["RGZWorkflow`Private`wardP"]&,4];w=Array[Unique["RGZWorkflow`Private`wardQ"]&,4];m=Unique["RGZWorkflow`Private`wardMass"];
 And[Expand[slash[w]-((slash[v+w]-m IdentityMatrix[4])-(slash[v]-m IdentityMatrix[4]))]===ConstantArray[0,{4,4}],
 And@@Flatten[Table[gam[[i]].gam[[j]]+gam[[j]].gam[[i]]===2 If[i===j,If[i===1,1,-1],0]IdentityMatrix[4],{i,4},{j,4}]]]],seconds];
 rows={identityEvidence["ClassicalBRST",Which[FailureQ[algebra],"Stopped",AssociationQ[algebra]&&AllTrue[Values[algebra],TrueQ],"Passed",True,"Failed"],algebra],
 identityEvidence["QuarkWard",Which[FailureQ[ward],"Stopped",TrueQ[ward],"Passed",True,"Failed"],ward]};
 (* Propagator verification must have been run in this session, against current model data. *)
 props=RGZResult["Propagators"];connections=RGZResult["PropagatorConnections"];
 AppendTo[rows,identityEvidence["PropagatorConstraint",If[ListQ[props]&&Length[props]===69&&AssociationQ[connections]&&Length[connections]===8,
 If[AllTrue[Values[connections],TrueQ[#===0]&]&&AllTrue[Join[Lookup[props,"\:6a21\:578b\:51cf\:95f5\:6c0f"],Lookup[props,"\:6a21\:578b\:51cf\:4e8c\:6b21\:6838"]],TrueQ[#===0]&],"Passed","Failed"],"NotRun"],"Run RGZCheckPropagators[] first; includes all 69 comparisons and eight contacts."]];
 nav=RGZResult["NAVPinch"];
 AppendTo[rows,identityEvidence["NAVReconstruction",If[AssociationQ[nav]&&ListQ[Lookup[nav,"Connections",None]],
 If[Length[nav["Connections"]]>0&&Length[nav["Connections"]]===Lookup[nav,"ConnectionCount",-1]&&AllTrue[nav["Connections"],TrueQ[Lookup[#,"ExactPolynomialReconstruction",False]]&],"Passed","Failed"],"NotRun"],"Run RGZPTStart[] with FullMatrix NAV; reconstruction is not a mixed-vertex STI proof."]];
 Scan[AppendTo[rows,identityEvidence[#,"NotImplemented","Required ghost/antifield or composite source kernels are not implemented; no zero result inferred."]]&, {"MixedVertexST","QuarkVertexST","OneLoopST","Nielsen","CompositeAh"}];
 r=<|"Status"->If[AnyTrue[rows,MemberQ[{"Failed","Stopped"},#["Status"]]&],"IdentityCheckFailed","PartialVerification"],"Name"->"STWardIdentities","Rows"->Length[rows],"Statistics"->Counts[Lookup[rows,"Status"]],"Checks"->rows,"Requirements"->RGZIdentityRequirements[]|>;
 storeResult["Identities",r];r
];
