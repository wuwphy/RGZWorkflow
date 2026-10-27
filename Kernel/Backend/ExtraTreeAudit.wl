(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Read the actual full quadratic action, not the FA two-point rules.
   Ordered ghost covariance is <field(p) barfield(-p)>; reversed order has
   the Grassmann minus sign. Spectator metric/deltas are shown separately. *)
ClearAll[rgzExtraGhostRow,rgzExtraQuarkRow];
rgzExtraGhostRow[field_Symbol,bar_Symbol,label_String,expectedK_,expectedM_,spectator_] := Module[
 {selected,kin,mass,unmatched,kt,mt,den,prop,ref,aa=Unique["kin"],bb=Unique["mass"],graded},
 selected=Select[rgzExtraTerms,!FreeQ[#,_field|_bar]&];
 kt=HoldPattern[z_. del[field[args___],idx_] del[bar[args___],idx_]]:>z aa;
 mt=HoldPattern[z_. field[args___] bar[args___]]:>z bb;
 unmatched=selected/.{kt,mt};
 kin=Total[Coefficient[#,aa]&/@unmatched];mass=Total[Coefficient[#,bb]&/@unmatched];
 unmatched=Expand[Total[unmatched]-kin aa-mass bb];
 den=kin rgzExtraS+mass;prop=I/den;ref=I/(expectedK rgzExtraS+expectedM);
 graded={{0,-den},{den,0}};
 <|"Field"->label,"KineticCoefficient"->kin,"MassCoefficient"->mass,"Unmatched"->unmatched,
 "ReferenceCoefficients"->{expectedK,expectedM},"ReferencePropagator"->ref,"Kernel"->den,"Propagator"->prop,"ReverseOrderedPropagator"->-prop,"Spectators"->spectator,
 "Checks"-><|"ActualActionMatchesReference"->TrueQ[Cancel[kin-expectedK]===0&&Cancel[mass-expectedM]===0&&unmatched===0],
 "PropagatorMatchesReference"->TrueQ[Cancel[prop-ref]===0],
 "GradedGaussianInverse"->TrueQ[Map[Cancel,graded.(I Inverse[graded])-I IdentityMatrix[2],{2}]===ConstantArray[0,{2,2}]]|>|>];
rgzExtraQuarkRow[field_Symbol,bar_Symbol,label_String,mass_] := Module[
 {selected,mapped,kc,mc,aa=Unique["kin"],bb=Unique["mass"],unmatched,rgzExtraSlashCoefficient,rgzExtraScalarCoefficient,den,product},
 selected=Select[rgzExtraTerms,!FreeQ[#,_field|_bar]&];
 mapped=selected/.{
 HoldPattern[z_. Dot[bar[Index[Spin,si_],col_],del[field[Index[Spin,sj_],col_],lor_]] Ga[lor_,Index[Spin,si_],Index[Spin,sj_]]]:>z aa,
 HoldPattern[z_. Dot[bar[indices___],field[indices___]]]:>z bb};
 kc=Total[Coefficient[#,aa]&/@mapped];mc=Total[Coefficient[#,bb]&/@mapped];unmatched=Expand[Total[mapped]-kc aa-mc bb];
 (* i partial -> p. In the Clifford algebra slash(p)^2=s. *)
 rgzExtraSlashCoefficient=kc/I;rgzExtraScalarCoefficient=mc;den=rgzExtraSlashCoefficient^2 rgzExtraS-rgzExtraScalarCoefficient^2;
 product={Cancel[(rgzExtraSlashCoefficient^2 rgzExtraS-rgzExtraScalarCoefficient^2)/den],Cancel[(rgzExtraSlashCoefficient (-rgzExtraScalarCoefficient)+rgzExtraScalarCoefficient rgzExtraSlashCoefficient)/den]};
 <|"Field"->label,"KineticCoefficient"->kc,"MassCoefficient"->mc,"Unmatched"->unmatched,
 "ReferenceCoefficients"->{I,-mass},"ReferencePropagatorCoefficients"->I {mass,1}/(rgzExtraS-mass^2),"DiracKernelCoefficients"->{rgzExtraScalarCoefficient,rgzExtraSlashCoefficient},"PropagatorCoefficients"->I {-rgzExtraScalarCoefficient,rgzExtraSlashCoefficient}/den,
 "CoefficientBasis"->{1,"slash(p)"},"Spectators"->"delta_colour; open Dirac indices",
 "Checks"-><|"ActualActionMatchesReference"->TrueQ[kc===I&&Cancel[mc+mass]===0&&unmatched===0],
 "CliffordInverseBothSides"->TrueQ[product==={1,0}],
 "PropagatorMatchesReference"->TrueQ[Map[Cancel,I {-rgzExtraScalarCoefficient,rgzExtraSlashCoefficient}/den-I {mass,1}/(rgzExtraS-mass^2)]==={0,0}]|>|>];
rgzExtraL2=Expand[GetQuadraticTerms[LTotal]];
rgzExtraTerms=If[Head[rgzExtraL2]===Plus,List@@rgzExtraL2,{rgzExtraL2}];
rgzExtraRows={rgzExtraGhostRow[eta,etabar,"eta",1,0,"delta_adjoint (scalar: no metric trace D)"],
 rgzExtraGhostRow[\[Omega],\[Omega]bar,"omega",-1,mu2,"metric_Lorentz delta_adjoint delta_Gribov"],
 rgzExtraGhostRow[\[Zeta],\[Zeta]bar,"zeta",-1,mu2,"metric_Lorentz delta_adjoint delta_Gribov"],
 rgzExtraQuarkRow[Q1,Q1bar,"Q1",mQ1],rgzExtraQuarkRow[Q2,Q2bar,"Q2",mQ2]};
rgzExtraChecks=Association[Flatten[Map[Function[row,KeyValueMap[(row["Field"]<>":"<>#1)->#2&,row["Checks"]]],rgzExtraRows]]];
rgzExtraTreePacket=<|"Schema"->"RGZExtraTreeAudit/1","Rows"->rgzExtraRows,"Checks"->rgzExtraChecks,
 "AllChecksPassed"->AllTrue[Values[rgzExtraChecks],TrueQ],"QuadraticActionSHA256"->Hash[rgzExtraL2,"SHA256"],
 "MomentumSquared"->rgzExtraS,"FourierConvention"->"exp(-i p.x); D=i K^-1",
 "FAPropagatorsIndependentlyCompared"->False,"Scope"->"Independent FR-action coefficients, ordered Gaussian and Clifford inverses; does not certify loop/ST identities."|>;
