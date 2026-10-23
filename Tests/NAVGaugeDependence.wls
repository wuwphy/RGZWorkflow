(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
Begin["RGZNAVGaugeAudit`"];
project=DirectoryName[DirectoryName[$InputFileName]];
summary=Import[FileNameJoin[{project,"output","review","NAVMatrix-summary.json"}],"RawJSON"];
packet=Import[summary["File"],"WXF"];
rows=packet["Connections"];
Print["RawAlphaPresent = ",Count[Lookup[rows,"Tensor"],x_/;!FreeQ[x,RGZWorkflow`Engine`\[Alpha]]]];
Print["TransverseAlphaPresent = ",Count[Lookup[rows,"TransverseTensor"],x_/;!FreeQ[x,RGZWorkflow`Engine`\[Alpha]]]];
pvec={5,1,2,1};qvec={2,1,-1,1/2};metric=DiagonalMatrix[{1,-1,-1,-1}];
vec[x_]:=Coefficient[Expand[x],RGZWorkflow`Engine`p]pvec+Coefficient[Expand[x],RGZWorkflow`Engine`q]qvec;
slot[x_]:=x/.{RGZWorkflow`Engine`rgzNAVNu->1,RGZWorkflow`Engine`rgzNAVRho->2,RGZWorkflow`Engine`rgzNAVSigma->3};
colour[x_]:=x/.{RGZWorkflow`Engine`rgzNAVA->1,RGZWorkflow`Engine`rgzNAVB->2,RGZWorkflow`Engine`rgzNAVC->3};
component[x_]:=(x/.RGZWorkflow`Engine`rgzNAVDelta[z_]:>(z-RGZWorkflow`Engine`m^2)(z-RGZWorkflow`Engine`\[Mu]^2)+RGZWorkflow`Engine`mG^4)/.{FeynCalc`SPD[v_,w_]:>vec[v].metric.vec[w],FeynCalc`FVD[v_,a_]:>vec[v][[slot[a]]],FeynCalc`MTD[a_,b_]:>metric[[slot[a],slot[b]]],FeynCalc`SUNF[a_,b_,c_]:>Signature[colour/@{a,b,c}],FeynCalc`SUNDelta[a_,b_]:>Boole[colour[a]===colour[b]],FeynCalc`SD[a_,b_]:>Boole[colour[a]===colour[b]],FeynCalc`SMP["g_s"]->1,FeynCalc`SUNN->3,RGZWorkflow`Engine`m->1,RGZWorkflow`Engine`\[Mu]->2,RGZWorkflow`Engine`mG->3,RGZWorkflow`Engine`M->1,System`D->4};
byX=Association@Table[f->Total[component/@Lookup[Select[rows,First[#["Fields"]]===f&],"TransverseTensor",{}]],{f,{"A","phi","phibar","beta","betabar","xi","tau"}}];
diff[x_]:=Cancel[(x/.RGZWorkflow`Engine`\[Alpha]->2)-(x/.RGZWorkflow`Engine`\[Alpha]->1)];
Do[Print["Transverse endpoint ",f," delta(alpha2-alpha1) = ",diff[byX[f]]],{f,Keys[byX]}];
fullDifference=diff[Total[Values[byX]]];Print["Full transverse tensor component difference = ",fullDifference];
Print["Numeric difference = ",N[fullDifference,12]];
aux=Select[rows,MemberQ[{{"A","phi","phibar"},{"A","phibar","phi"},{"A","beta","betabar"},{"A","betabar","beta"}},#["Fields"]]&];
Print["Central-A phi/beta subgroup alpha-free (transverse exchange) = ",FreeQ[Lookup[aux,"TransverseTensor"],RGZWorkflow`Engine`\[Alpha]]];
Print["No unresolved tensor heads in numeric sum = ",FreeQ[Total[Values[byX]],FeynCalc`SPD|FeynCalc`FVD|FeynCalc`MTD|FeynCalc`SUNF|RGZWorkflow`Engine`rgzNAVDelta]];
End[];Exit[];
