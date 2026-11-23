(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* One open FP chain: native class sign, ordered tree and six permutations.
   Fixture is the actual FR-exported FP cubic, not a fitted reference.
   This is not a general ordering rule for eta or vector/multiple ghosts. *)
If[$Context =!= "RGZWorkflow`Engine`",With[{script=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
 RGZWorkflow`RGZInitialize[CreateDirectory[],"Display"->"Quiet"];
 Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],Get[FileNameJoin[{project,"Tests","fixtures","FPGhostCubic.wl"}]],"WXF"];
 If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
 RGZWorkflow`Private`loadQuarkEngine[];
 RGZWorkflow`Private`engineGet["Tree/VertexRules.wl",False];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
checks=<||>;rec[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
r=rgzCheckOne[1];rec["ActualFPOrderedTreeMatches",MemberQ[{"Three-way agreement","FA=FR; hand calculation not supplied"},r["Status"]]];
rec["ComparisonMapIsScopedToFP",rgzVertexAmplitudeNormalize[test,{"eta","etabar","A"}]===test&&rgzVertexAmplitudeNormalize[test,{"ω","ωbar","A"}]===test&&rgzVertexAmplitudeNormalize[test,{"cbar","c","cbar","c"}]===test];
RGZWorkflow`Private`engineGet["Tree/LegOrdering.wl",False];
rgzOrderResults=rgzOrderOne[1,#]&/@Permutations[{"cbar","c","A"}];
RGZWorkflow`Private`engineGet["Tree/Permutations.wl",False];
perms=rgzPermCompare[1,#]&/@Permutations[Range[3]];
rec["AllSixFPOrdersGenerated",Length[perms]===6&&AllTrue[perms,#["Status"]==="Passed"&]];
Print[KeyTake[#, {"ExternalLegs","Status"}]&/@perms];
Exit[If[And@@Values[checks],0,1]];
