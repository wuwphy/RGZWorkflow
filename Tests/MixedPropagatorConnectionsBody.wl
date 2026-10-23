(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
checks=<||>;record[name_,value_]:=(AssociateTo[checks,name->TrueQ[value]];Print[name," : ",checks[name]]);
start=RGZMXStart["\:573a\:5bf9"->{5,6},"\:7c7b\:522b"->{"\:73bb\:8272\:573a\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe"},"\:590d\:7528AA\:7f13\:5b58"->False,"\:5355\:6b65\:65f6\:9650"->120];
If[FailureQ[start],Print[start];Exit[1]];
ins=First[Values[rgzMXJob["\:63d2\:5165\:56fe"]]];
particleGraphs=Cases[ins,g:h_[_,lev_][___] /; RGZLoopName[h]==="FeynmanGraph" && !FreeQ[lev,Particles] :> g,Infinity];
rows=rgzMXJob["\:9010\:56fe"];
record["ParticleGraphsMatchAmplitudes",Length[particleGraphs]===Length[rows]&&Length[rows]>0];
If[!TrueQ[checks["ParticleGraphsMatchAmplitudes"]],Print[{Length[particleGraphs],Length[rows]}];Exit[1]];
(* Identify the kinetic graphs by their two insertion endpoints, rather
   than assuming FA graph numbers are stable across dependency versions. *)
groups=<|"FirstLoop"->{},"SecondLoop"->{}|>;
Do[
 types=Table[Field[k]/.List@@particleGraphs[[n]],{k,3,6}];
 types=types /. (h_[class_Integer,indices_List] /; MemberQ[{"S","V"},RGZLoopName[h]] :> h[class]);
 pairs=Partition[types,2];
 Do[other=3-pos;
  If[MemberQ[{{S[1],S[1]},{S[1],V[1]},{V[1],S[1]},{V[1],V[1]}},pairs[[pos]]]&&
    MemberQ[{{V[2],-V[2]},{-V[2],V[2]}},pairs[[other]]],
    name=If[pos===1,"FirstLoop","SecondLoop"];AssociateTo[groups,name->Append[groups[name],n]]],{pos,2}],{n,Length[rows]}];
record["FourKineticGraphsPerRouting",Sort[Length/@Values[groups]]==={4,4}];
Print["Identified kinetic graphs = ",groups];If[!TrueQ[checks["FourKineticGraphsPerRouting"]],Exit[1]];
deltas=<||>;
Do[
 fc=RGZMXConvertOne[rows[[n]]["\:539f\:59cbFA\:632f\:5e45"]];
 If[FailureQ[fc],Print[fc];Exit[1]];
 projected=RGZMXProject[fc["\:632f\:5e45"],5,6];
 expr=RGZMixAdjoint[projected["T"],5,6,60];
 expr=expr/.{SUNN->connectionN,System`N->connectionN};
 expr=expr/.{Pair[Momentum[p,___],Momentum[p,___]]->connectionS,Pair[Momentum[q,___],Momentum[q,___]]->connectionX,Pair[Momentum[p,___],Momentum[q,___]]->connectionZ,Pair[Momentum[q,___],Momentum[p,___]]->connectionZ};
 delta=TimeConstrained[Cancel[Together[expr-(expr/.\[Alpha]->1)]],60,$TimedOut];
 record["GeneralMassProjection:"<>ToString[n],FreeQ[delta,_Failure|_Missing|$Failed|$Aborted|$TimedOut]];
 AssociateTo[deltas,ToString[n]->delta],{n,DeleteDuplicates[Flatten[Values[groups]]]}];
Do[
 group=Lookup[deltas,ToString/@groups[name]];
 record["NontrivialIndividualGraphs:"<>name,AllTrue[group,!TrueQ[#===0]&]];
 record["GeneralMassWardCancellation:"<>name,TrueQ[TimeConstrained[Cancel[Together[Total[group]]],60,$TimedOut]===0]],{name,Keys[groups]}];
(* Mutate only this test's isolated model copy. A manually selected old
   cache must be rejected even when its model basename is unchanged. *)
savedGeneric=Import[modelRGZ<>".gen","Text"];
Internal`WithLocalSettings[
 Export[modelRGZ<>".gen",savedGeneric<>"\n(* cache identity regression probe *)\n","Text"],
 resumed=RGZMXResume[rgzMXJob["\:7f13\:5b58\:76ee\:5f55"]];
 record["RejectSamePathChangedModelCache",FailureQ[resumed]&&resumed[[1]]==="StaleGraphCache"],
 Export[modelRGZ<>".gen",savedGeneric,"Text"]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False],"WorkDirectory"->RGZWorkflow`Private`$config["WorkDirectory"]|>];
Exit[If[And@@Values[checks],0,1]];
