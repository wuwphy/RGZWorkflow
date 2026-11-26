(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Optional research calculation. Load after RGZWorkflow.wl. No source caches are changed.
   The input C is the existing PT convention -(D0 Pi D0)AA, in raw TID units.
   Pi includes its Feynman amplitude i. A candidate is never promoted to a proven PT result. *)
BeginPackage["RGZPTResearch`"];
RGZPTSelfEnergy::usage="RGZPTSelfEnergy[packet,s] amputates a completed FourPinch packet. s is its squared-momentum expression. Raw TID measure is converted once. Returns a candidate, not a PT certificate.";
RGZPTEuclidean::usage="RGZPTEuclidean[result,Q2] expresses its inverse propagator at s=-Q2 on the declared Euclidean sheet. No mass limit or analytic threshold continuation is taken.";
RGZPTCharge::usage="RGZPTCharge[finiteSigmaE,Q2,treeInverseE,mu2,alphaMu] defines a finite tree-factor-stripped MOM shape at one loop. Input must already be finite; PT/RG invariance is not inferred.";
RGZMSbarRunning::usage="RGZMSbarRunning[Q2,mu2,alphaMu,Nf:2,Nc:3] solves the one-loop MSbar beta function; this does not require a PT decomposition.";
Begin["`Private`"];
invalidQ[x_]:=!FreeQ[x,_Failure|_Missing|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity];
RGZPTSelfEnergy[packet_Association,s_] := Module[{names={"box","navAll","avAll","qseAll"},parts,a,ordinary,pinch,common,sigma,measure=1/(2Pi)^4},
 If[Lookup[packet,"Mode",None]=!="FourPinch"||!AssociationQ[Lookup[packet,"pinchParts",None]]||
 !KeyExistsQ[packet,"CommonConventionSelfEnergyT"],Return[Failure["CompletedFourPinchPacketRequired",<||>]]];
 parts=packet["pinchParts"];If[!And@@(KeyExistsQ[parts,#]&/@names)||invalidQ[{s,parts,packet["CommonConventionSelfEnergyT"]}],Return[Failure["IncompleteTwoPointInput",<||>]]];
 If[!FreeQ[{parts,packet["CommonConventionSelfEnergyT"]},FeynCalc`TID|FeynCalc`FeynAmpDenominator],Return[Failure["IntegratedTwoPointInputRequired",<||>]]];
 a=(s-RGZWorkflow`Engine`\[Mu]^2)/((s-RGZWorkflow`Engine`m^2)(s-RGZWorkflow`Engine`\[Mu]^2)+RGZWorkflow`Engine`mG^4);
 parts=Map[FeynCalc`FCI[RGZWorkflow`Engine`RGZLoopPoleMasses[#]]&,parts];
 ordinary=measure FeynCalc`FCI[RGZWorkflow`Engine`RGZLoopPoleMasses[packet["CommonConventionSelfEnergyT"]]];pinch=measure Total[Lookup[parts,names]];common=ordinary+pinch;
 sigma=-I common/a^2;
 <|"Status"->"CandidateTwoPointKernel","SquaredMomentum"->s,"TreeScalarM"->a,"OrdinaryCommonKernel"->ordinary,
 "PinchCommonParts"->Map[measure #&,parts],"CommonKernel"->common,"SelfEnergyM"->sigma,
 "InverseScalarM"->1/a-sigma,"OneLoopScalarM"->a+a^2 sigma,
 "ConnectedInsertion"->-common,"AmputationReconstruction"->(I a^2 sigma-common),
 "Conditions"->{Unequal[s,RGZWorkflow`Engine`\[Mu]^2],Unequal[(s-RGZWorkflow`Engine`m^2)(s-RGZWorkflow`Engine`\[Mu]^2)+RGZWorkflow`Engine`mG^4,0]},
 "LoopMeasureConversion"->measure,"NAVMode"->Lookup[packet,"NAVMode",Missing["NotSpecified"]],
 "SourceSHA256"->Hash[packet,"SHA256"],"PTIdentitiesVerified"->False,"RGInvariantVerified"->False,
 "Convention"->"D0=-i A; Pi=i Sigma; D1=D0 Pi D0=-i A^2 Sigma; C=-D1=i A^2 Sigma; inverse scalar A^-1-Sigma",
 "Outstanding"->{"Full RGZ Ward/ST pinch completeness","Process independence","Renormalized PT identities and RG invariance","Physical-sheet continuation"}|>
];
RGZPTEuclidean[result_Association,q2_] := Module[{s,a,k,sigma},
 If[!And@@(KeyExistsQ[result,#]&/@{"SquaredMomentum","TreeScalarM","SelfEnergyM"})||invalidQ[{result,q2}],Return[Failure["TwoPointResultRequired",<||>]]];
 s=FeynCalc`FCI[result["SquaredMomentum"]];a=-(FeynCalc`FCI[result["TreeScalarM"]]/.s->-q2);k=1/a;sigma=FeynCalc`FCI[result["SelfEnergyM"]]/.s->-q2;
 <|"Status"->"CandidateEuclideanKernel","Q2"->q2,"TreeScalarE"->a,"TreeInverseE"->k,"SelfEnergyE"->sigma,
 "InverseScalarE"->k+sigma,"OneLoopScalarE"->a-a^2 sigma,
 "Conditions"->{q2>0},"BranchPrescription"->"Evaluate on the Euclidean sheet before any continuation; no independent logarithm or root branch changes.",
 "FiniteRenormalizationApplied"->False,"PTIdentitiesVerified"->False,"RGInvariantVerified"->False|>
];
RGZPTCharge[finiteSigmaE_,q2_Symbol,treeInverseE_,mu2_,alphaMu_] := Module[{h,sub,shift,finiteProbe},
 finiteProbe=finiteSigmaE/._RGZWorkflow`RGZPVFinite->0;
 If[invalidQ[{finiteSigmaE,treeInverseE,mu2,alphaMu}]||!FreeQ[finiteProbe,System`D|FeynCalc`Epsilon|FeynCalc`EpsilonUV|FeynCalc`EpsilonIR|FeynCalc`SMP["Delta_UV"]|FeynCalc`A0|FeynCalc`B0|FeynCalc`PaVe|FeynCalc`TID|FeynCalc`FeynAmpDenominator],Return[Failure["FiniteRenormalizedSelfEnergyRequired",<||>]]];
 h=finiteSigmaE/treeInverseE;sub=h/.q2->mu2;shift=h-sub;
 <|"Status"->"FiniteMOMShapeCandidate","Q2"->q2,"ReferenceScaleSquared"->mu2,"AlphaAtReference"->alphaMu,
 "NormalizedInverseCorrection"->shift,"OneLoopEffectiveCoupling"->alphaMu(1-shift),
 "Conditions"->{q2>0,mu2>0,alphaMu>0,Abs[shift]<1},
 "Definition"->"Factor out the RGZ tree propagator, then subtract the finite dimensionless shape at mu2. Alpha(mu2) is an input in this definition.",
 "FiniteInputAssumed"->True,"PTIdentitiesVerified"->False,"RGInvariantVerified"->False,
 "Scope"->"Strict one-loop finite shape; not an IR freezing prediction or an all-order Dyson resummation. Momentum subtraction of an unrenormalized massive ratio is not implemented."|>
];
RGZMSbarRunning[q2_,mu2_,alphaMu_,nf_:2,nc_:3] := Module[{beta0,den},
 If[!IntegerQ[nf]||nf<0||!IntegerQ[nc]||nc<2||invalidQ[{q2,mu2,alphaMu}],Return[Failure["RunningCouplingArguments",<||>]]];
 beta0=11nc/3-2nf/3;den=1+beta0 alphaMu Log[q2/mu2]/(4Pi);
 <|"Scheme"->"MSbar","LoopOrder"->1,"Beta0"->beta0,"Alpha"->alphaMu/den,
 "BetaConvention"->"d alpha / d log(mu^2) = -beta0 alpha^2/(4 pi)",
 "Conditions"->{q2>0,mu2>0,alphaMu>0,den>0},"Scope"->"One-loop RG solution; no RGZ mass-threshold matching or nonperturbative IR prediction."|>
];
End[];EndPackage[];
