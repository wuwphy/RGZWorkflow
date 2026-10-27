(* SPDX-License-Identifier: GPL-3.0-or-later *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
checks=<||>;rec[n_,v_]:=AssociateTo[checks,n->TrueQ[v]];
before=RGZWorkflow`Private`$results;
p=RGZPaperResult[];
rec["ReadableReference",AssociationQ[p]];
If[AssociationQ[p],
 rec["ReferenceCannotCertifyCurrentTask",p["ReferenceOnly"]===True&&p["CurrentTaskCertified"]===False];
 rec["EightRows31Functions144Entries",Length[p["Rows"]]===8&&Length[p["Functions"]]===31&&Total[Length[#["ReducedCoefficients"]]&/@p["Rows"]]===144];
 rec["PublishedRowCounts",Length[#["ReducedCoefficients"]]&/@p["Rows"]==={15,31,19,19,13,13,15,19}];
 expected={(RGZPaper`Nc^2-1)RGZPaper`gs^4/(16Pi^2 RGZPaper`Nc^2(RGZPaper`s+RGZPaper`t)),RGZPaper`gs^4/(4Pi^2),
 (RGZPaper`Nc^2-1)RGZPaper`gs^4/(8Pi^2 RGZPaper`Nc^2(RGZPaper`s+RGZPaper`t)),RGZPaper`gs^4/(2Pi^2 RGZPaper`t RGZPaper`Nc(RGZPaper`s+RGZPaper`t)),
 (RGZPaper`Nc^2-1)RGZPaper`gs^4/(64Pi^2 RGZPaper`Nc^2(RGZPaper`a-RGZPaper`b)),RGZPaper`gs^4/(32Pi^2 RGZPaper`Nc(RGZPaper`a-RGZPaper`b)),
 (RGZPaper`Nc^2-1)RGZPaper`gs^4/(32Pi^2 RGZPaper`s RGZPaper`Nc^2(RGZPaper`s+RGZPaper`t)),RGZPaper`gs^4/(16Pi^2 RGZPaper`s(RGZPaper`s+RGZPaper`t))};
 rec["PublishedPrefactors",MapThread[Cancel[Together[#1-#2]]===0&,{Lookup[p["Rows"],"Prefactor"],expected}]===ConstantArray[True,8]];
];
rec["NoLiveEvidenceMutation",SameQ[before,RGZWorkflow`Private`$results]];
RGZPaper`a=1;
rec["AssignedReferenceSymbolsRejected",MatchQ[RGZPaperResult[],Failure["PaperSymbolsAssigned",_Association]]];
Clear[RGZPaper`a];
rec["ReadAfterClearing",AssociationQ[RGZPaperResult[]]];
Print[checks];Exit[If[AllTrue[Values[checks],TrueQ],0,1]];
