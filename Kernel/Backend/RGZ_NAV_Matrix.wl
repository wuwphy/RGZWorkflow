(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* All cubic bosonic connections to three A sources. Central momenta: p,q,-p-q.
   Row F, column A propagators are used: scalar-vector entries are odd. *)
rgzNAVName[x_] := Switch[SymbolName[x],"\[Phi]"|"\:03d5","phi","\[Phi]bar"|"\:03d5bar","phibar","\[Beta]","beta","\[Beta]bar","betabar","\[Xi]","xi","\[Tau]","tau",_,SymbolName[x]];
rgzNAVFields={"A","b","tau","xi","phi","phibar","beta","betabar"};
rgzNAVVectors={"A","phi","phibar","beta","betabar"};
rgzNAVDelta[z_]:=(z-m^2)(z-\[Mu]^2)+mG^4;
rgzNAVPT[v_,a_,b_]:=MTD[a,b]-FVD[v,a] FVD[v,b]/SPD[v,v];
rgzNAVProp[field_,v_,a_,b_,col_,spectator_,source_,transverse_:False]:=Module[{z=SPD[v,v],den},den=rgzNAVGaugeDen[v];
 Switch[field,
 "A",-I SUNDelta[col,source] ((z-\[Mu]^2)/rgzNAVDen[v] rgzNAVPT[v,a,b]+If[transverse,0,\[Alpha] FVD[v,a] FVD[v,b]/den]),
 "xi",If[transverse,0,\[Alpha] FVD[v,b] SUNDelta[col,source]/den],
 "tau",If[transverse,0,\[Alpha] M^4 FVD[v,b] SUNDelta[col,source]/(z den)],
 "b",If[transverse,0,z FVD[v,b] SUNDelta[col,source]/den],
 _,I If[MemberQ[{"beta","betabar"},field],-1,1] mG^2 SUNF[source,col,spectator] rgzNAVPT[v,a,b]/(2 Sqrt[SUNN] rgzNAVDen[v])]];
rgzNAVConnections[]:=Flatten[Table[With[{names=rgzNAVName/@rgzRaw[[id,1,All,1]]},
 Table[<|"VertexID"->id,"Fields"->order,"LegOrder"->First@Select[Permutations[Range[3]],names[[#]]===order&]|>,{order,Select[Permutations[names],And@@(MemberQ[rgzNAVFields,#]& /@ #)&]}]],{id,Select[rgzIDs,Length[rgzRaw[[#,1]]]===3&]}],1];
rgzNAVConnection[record_,transverse_:False]:=Module[{rules={},legs=record["LegOrder"],fields=record["Fields"],mom={p,q,-p-q},lor={rgzNAVL0,rgzNAVL1,rgzNAVL2},gl={rgzNAVC0,rgzNAVC1,rgzNAVC2},gr={rgzNAVE0,rgzNAVE1,rgzNAVE2},outlor={rgzNAVNu,rgzNAVRho,rgzNAVSigma},outcol={rgzNAVA,rgzNAVB,rgzNAVC},vertex,expr},
 Do[rules=Join[rules,{rgzP[legs[[i]]]->mom[[i]],Symbol["RGZWorkflow`Engine`\:03bc"<>ToString[legs[[i]]]]->lor[[i]],Symbol["RGZWorkflow`Engine`Glu"<>ToString[legs[[i]]]]->gl[[i]],Symbol["RGZWorkflow`Engine`Gri"<>ToString[legs[[i]]]]->gr[[i]]}],{i,3}];
 vertex=rgzFRFC[record["VertexID"]]/.rules/.HoldPattern[System`N]->SUNN;
 expr=vertex Times@@Table[rgzNAVProp[fields[[i]],mom[[i]],lor[[i]],outlor[[i]],gl[[i]],gr[[i]],outcol[[i]],transverse&&i===1],{i,3}];
 FCE[Contract[SUNSimplify[FCI[expr],SUNNToCACF->False]]]];
rgzNAVRestoreDen[x_]:=x/.{rgzNAVDen[v_]:>rgzNAVDelta[SPD[v,v]],rgzNAVGaugeDen[v_]:>SPD[v,v]^2+\[Alpha] M^4};
rgzNAVAttach[tensor_,mass_]:=FCE[DiracSimplify[Contract[SUNSimplify[FCI[-I SMP["g_s"]^2 tensor SUNT[rgzNAVC,rgzNAVB] (GAD[rgzNAVSigma].GSD[rgzNAVIncoming-q].GAD[rgzNAVRho]+mass GAD[rgzNAVSigma].GAD[rgzNAVRho])/(SPD[rgzNAVIncoming-q,rgzNAVIncoming-q]-mass^2)],SUNNToCACF->False]]]];
rgzNAVBuild[dir_,mass_,seconds_]:=Module[{records=rgzNAVConnections[],rows={},file,row,r,t,tensor,transverse,packet},
 Do[file=FileNameJoin[{dir,"connection-"<>ToString[n]<>".wxf"}];
 If[FileExistsQ[file],row=Import[file,"WXF"],
 r=TimeConstrained[rgzNAVConnection[records[[n]]],seconds,$TimedOut];If[r===$TimedOut,Return[Failure["NAVConnectionTimeout",<|"Connection"->n|>]]];
 t=TimeConstrained[rgzNAVConnection[records[[n]],True],seconds,$TimedOut];If[t===$TimedOut,Return[Failure["NAVConnectionTimeout",<|"Connection"->n,"Transverse"->True|>]]];
 row=Join[records[[n]],<|"Tensor"->r,"TransverseTensor"->t|>];
 r=RGZWorkflow`Private`atomicWXF[file,row];If[FailureQ[r],Return[r]]];AppendTo[rows,row];Print["NAV ",n," / ",Length[records]],{n,Length[records]}];
 tensor=Total[Lookup[rows,"Tensor"]];transverse=Total[Lookup[rows,"TransverseTensor"]];
 packet=<|"Schema"->"RGZNAVMatrix/1","Connections"->rows,"ConnectionCount"->Length[rows],"ConservedCurrentConnectionCount"->Count[Lookup[rows,"Fields"][[All,1]],Alternatives@@rgzNAVVectors],"Tensor"->tensor,"TransverseTensor"->transverse,
 "ByExternalField"->Association@Table[f->Total[Lookup[Select[rows,First[#["Fields"]]===f&],"Tensor",{}]],{f,rgzNAVFields}],
 "Integrand"->Total[Table[Print["NAV Dirac ",n," / ",Length[rows]];rgzNAVAttach[rows[[n]]["Tensor"],mass],{n,Length[rows]}]],"TransverseIntegrand"->Total[rgzNAVAttach[#["TransverseTensor"],mass]& /@ rows],
 "QuarkLine"->I (GSD[rgzNAVIncoming-q]+mass)/ (SPD[rgzNAVIncoming-q,rgzNAVIncoming-q]-mass^2),
 "QuarkColourOrder"->SUNT[rgzNAVC,rgzNAVB],"QuarkGammaOrder"->{rgzNAVSigma,rgzNAVRho},"QuarkVertexFactor"->-SMP["g_s"]^2,
 "Momenta"-><|"Transfer"->p,"Loop"->q,"IncomingQuark"->rgzNAVIncoming,"CentralIncoming"->{p,q,-p-q}|>,
 "Meaning"->"Full colour and Lorentz contracted NAV vertex integrand, with external exchange propagator and quark line. Integrate d^D q/(2 pi)^D; no pinch decomposition performed."|>;
 packet=rgzNAVRestoreDen[packet];
 r=RGZWorkflow`Private`atomicWXF[FileNameJoin[{dir,"result.wxf"}],packet];If[FailureQ[r],r,packet]];
