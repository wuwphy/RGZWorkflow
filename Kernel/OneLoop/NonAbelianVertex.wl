(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Build the complete cubic field-matrix connections to three A sources.

   Inputs:
   The checked tree/model data and the graph or task data named by the called function.

   Dependencies:
   Tree/ definitions and Algebra/ tools; Internal/ supplies configuration and persistence.
*)

(* All cubic bosonic connections to three A sources. Central momenta: p,q,-p-q.
   Row F, column A propagators are used: scalar-vector entries are odd. *)
rgzNAVName[x_] := Switch[SymbolName[x],"\[Phi]"|"ϕ","phi","\[Phi]bar"|"ϕbar","phibar","\[Beta]","beta","\[Beta]bar","betabar","\[Xi]","xi","\[Tau]","tau",_,SymbolName[x]];
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
 "phi"|"phibar"|"beta"|"betabar",I If[MemberQ[{"beta","betabar"},field],-1,1] mG^2 SUNF[source,col,spectator] rgzNAVPT[v,a,b]/(2 Sqrt[SUNN] rgzNAVDen[v]),
 _,Failure["UnknownNAVField",<|"Field"->field|>]]];
rgzNAVConnections[]:=Flatten[Table[With[{names=rgzNAVName/@rgzRaw[[id,1,All,1]]},
 Table[<|"VertexID"->id,"Fields"->order,"LegOrder"->First@Select[Permutations[Range[3]],names[[#]]===order&]|>,{order,Select[Permutations[names],And@@(MemberQ[rgzNAVFields,#]& /@ #)&]}]],{id,Select[rgzIDs,Length[rgzRaw[[#,1]]]===3&]}],1];
rgzNAVConnection[record_,transverse_:False]:=Module[{rules={},legs=record["LegOrder"],fields=record["Fields"],mom={p,q,-p-q},lor={rgzNAVL0,rgzNAVL1,rgzNAVL2},gl={rgzNAVC0,rgzNAVC1,rgzNAVC2},gr={rgzNAVE0,rgzNAVE1,rgzNAVE2},outlor={rgzNAVNu,rgzNAVRho,rgzNAVSigma},outcol={rgzNAVA,rgzNAVB,rgzNAVC},vertex,expr},
 If[!RGZWorkflow`Private`navMatrixDescriptorQ[record],Return[Failure["InvalidNAVConnection",<||>]]];
 Do[rules=Join[rules,{rgzP[legs[[i]]]->mom[[i]],Symbol["RGZWorkflow`Engine`μ"<>ToString[legs[[i]]]]->lor[[i]],Symbol["RGZWorkflow`Engine`Glu"<>ToString[legs[[i]]]]->gl[[i]],Symbol["RGZWorkflow`Engine`Gri"<>ToString[legs[[i]]]]->gr[[i]]}],{i,3}];
 vertex=rgzFRFC[record["VertexID"]]/.rules/.HoldPattern[System`N]->SUNN;
 If[!RGZLoopValidValueQ[vertex],Return[Failure["InvalidNAVVertex",<||>]]];
 expr=vertex Times@@Table[rgzNAVProp[fields[[i]],mom[[i]],lor[[i]],outlor[[i]],gl[[i]],gr[[i]],outcol[[i]],transverse&&i===1],{i,3}];
 FCE[Contract[SUNSimplify[FCI[expr],SUNNToCACF->False]]]];
rgzNAVRestoreDen[x_]:=x/.{rgzNAVDen[v_]:>rgzNAVDelta[SPD[v,v]],rgzNAVGaugeDen[v_]:>SPD[v,v]^2+\[Alpha] M^4};
rgzNAVAttach[tensor_,mass_]:=If[!RGZLoopValidValueQ[{tensor,mass}],Failure["InvalidNAVAttachmentInput",<||>],FCE[DiracSimplify[Contract[SUNSimplify[FCI[-I SMP["g_s"]^2 tensor SUNT[rgzNAVC,rgzNAVB] (GAD[rgzNAVSigma].GSD[rgzNAVIncoming-q].GAD[rgzNAVRho]+mass GAD[rgzNAVSigma].GAD[rgzNAVRho])/(SPD[rgzNAVIncoming-q,rgzNAVIncoming-q]-mass^2)],SUNNToCACF->False]]]]];
(* Identities cover actual converted vertices as well as their metadata. *)
rgzNAVMatrixSource[record_] := Module[{vertex},
 If[!RGZWorkflow`Private`navMatrixDescriptorQ[record],Return[Failure["InvalidNAVConnection",<||>]]];
 vertex=rgzFRFC[record["VertexID"]];
 If[!RGZLoopValidValueQ[vertex],Return[Failure["InvalidNAVVertex",<||>]]];
 Hash[{"NAVMatrixSource/2",RGZWorkflow`Private`workflowFingerprint[],
   KeyTake[record,{"VertexID","Fields","LegOrder"}],vertex},"SHA256"]];
rgzNAVMatrixSources[records_] := rgzNAVMatrixSource /@ records;
rgzNAVBuild[dir_,mass_,seconds_]:=Module[{tag=Unique["NAVMatrixReturn"]},Catch[
 Module[{records,sources,rows={},attachments={},file,row,attached,r,t,packet,n,identity},
 If[!StringQ[dir] || !NumericQ[seconds] || !TrueQ[seconds>0] || !RGZLoopValidValueQ[mass],
  Throw[Failure["NAVMatrixArguments",<||>],tag]];
 records=TimeConstrained[rgzNAVConnections[],seconds,$TimedOut];
 If[!ListQ[records] || records==={} || !AllTrue[records,RGZWorkflow`Private`navMatrixDescriptorQ],
  Throw[Failure["InvalidNAVConnections",<||>],tag]];
 sources=TimeConstrained[rgzNAVMatrixSources[records],seconds,$TimedOut];
 If[!ListQ[sources] || !AllTrue[sources,IntegerQ],Throw[Failure["NAVMatrixSourceStopped",<||>],tag]];
 If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
 Do[
 file=FileNameJoin[{dir,"connection-"<>ToString[n]<>".wxf"}];
 row=If[FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],Missing["NoCache"]];
 If[!RGZWorkflow`Private`navMatrixConnectionQ[row] ||
   KeyTake[row,{"VertexID","Fields","LegOrder"}]=!=records[[n]] || row["InputIdentity"]=!=sources[[n]],
  r=TimeConstrained[rgzNAVRestoreDen[rgzNAVConnection[records[[n]]]],seconds,$TimedOut];
  If[!RGZLoopValidValueQ[r],Throw[Failure["NAVConnectionStopped",<|"Connection"->n|>],tag]];
  t=TimeConstrained[rgzNAVRestoreDen[rgzNAVConnection[records[[n]],True]],seconds,$TimedOut];
  If[!RGZLoopValidValueQ[t],Throw[Failure["NAVConnectionStopped",<|"Connection"->n,"Transverse"->True|>],tag]];
  row=Join[records[[n]],<|"Tensor"->r,"TransverseTensor"->t,"ValidationPolicy"->"NAVMatrix/2",
    "InputIdentity"->sources[[n]],"TensorHash"->Hash[{r,t},"SHA256"]|>];
  If[!RGZWorkflow`Private`navMatrixConnectionQ[row],Throw[Failure["NAVConnectionCacheInvalid",<|"Connection"->n|>],tag]];
  r=RGZWorkflow`Private`atomicWXF[file,row];If[FailureQ[r],Throw[r,tag]]];
 AppendTo[rows,row];Print["NAV ",n," / ",Length[records]],{n,Length[records]}];
 Do[
 identity=RGZWorkflow`Private`navMatrixAttachmentIdentity[rows[[n]],mass];
 file=FileNameJoin[{dir,"attachment-"<>ToString[n]<>".wxf"}];
 attached=If[FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],Missing["NoCache"]];
 If[!RGZWorkflow`Private`navMatrixAttachmentQ[attached,n,identity],
  r=TimeConstrained[rgzNAVAttach[rows[[n]]["Tensor"],mass],seconds,$TimedOut];
  If[!RGZLoopValidValueQ[r],Throw[Failure["NAVAttachmentStopped",<|"Connection"->n|>],tag]];
  t=TimeConstrained[rgzNAVAttach[rows[[n]]["TransverseTensor"],mass],seconds,$TimedOut];
  If[!RGZLoopValidValueQ[t],Throw[Failure["NAVAttachmentStopped",<|"Connection"->n,"Transverse"->True|>],tag]];
  attached=<|"Connection"->n,"Identity"->identity,"Integrand"->r,"TransverseIntegrand"->t,
    "PayloadHash"->Hash[{r,t},"SHA256"]|>;
  r=RGZWorkflow`Private`atomicWXF[file,attached];If[FailureQ[r],Throw[r,tag]]];
 AppendTo[attachments,attached];Print["NAV Dirac ",n," / ",Length[records]],{n,Length[records]}];
 packet=<|"Schema"->"RGZNAVMatrix/1","Connections"->rows,"ConnectionCount"->Length[rows],
 "ConservedCurrentConnectionCount"->Count[Lookup[rows,"Fields"][[All,1]],Alternatives@@rgzNAVVectors],
 "Tensor"->Total[Lookup[rows,"Tensor"]],"TransverseTensor"->Total[Lookup[rows,"TransverseTensor"]],
 "ByExternalField"->Association@Table[f->Total[Lookup[Select[rows,First[#["Fields"]]===f&],"Tensor",{}]],{f,rgzNAVFields}],
 "QuarkMass"->mass,"AttachmentRecords"->attachments,
 "Integrand"->Total[Lookup[attachments,"Integrand"]],"TransverseIntegrand"->Total[Lookup[attachments,"TransverseIntegrand"]],
 "QuarkLine"->I (GSD[rgzNAVIncoming-q]+mass)/(SPD[rgzNAVIncoming-q,rgzNAVIncoming-q]-mass^2),
 "QuarkColourOrder"->SUNT[rgzNAVC,rgzNAVB],"QuarkGammaOrder"->{rgzNAVSigma,rgzNAVRho},"QuarkVertexFactor"->-SMP["g_s"]^2,
 "Momenta"-><|"Transfer"->p,"Loop"->q,"IncomingQuark"->rgzNAVIncoming,"CentralIncoming"->{p,q,-p-q}|>,
 "Meaning"->"Full colour and Lorentz contracted NAV vertex integrand, with external exchange propagator and quark line. Integrate d^D q/(2 pi)^D; no pinch decomposition performed."|>;
 If[!RGZWorkflow`Private`navMatrixMatchesQ[packet,records,sources,mass],Throw[Failure["InvalidNAVMatrixResult",<||>],tag]];
 r=RGZWorkflow`Private`atomicWXF[FileNameJoin[{dir,"result.wxf"}],packet];If[FailureQ[r],r,packet]
 ],tag]];
