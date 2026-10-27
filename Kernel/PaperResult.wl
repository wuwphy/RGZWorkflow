(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Read the author's inert electronic reference without promoting a live task. *)
RGZPaperResult[] := Module[{file,meta,held,names,p,rows,ids,pairs,sha},
 file=FileNameJoin[{$packageRoot,"Paper","data","form-factors.wl"}];
 meta=Quiet[Check[Import[FileNameJoin[{$packageRoot,"Paper","provenance.json"}],"RawJSON"],$Failed]];
 If[!FileExistsQ[file]||!AssociationQ[meta],Return[Failure["PaperResultMissing",<||>]]];
 sha=IntegerString[FileHash[file,"SHA256"],16,64];
 If[sha=!=Lookup[meta,"FormFactorsSHA256",None],Return[Failure["PaperResultIntegrityMismatch",<||>]]];
 held=Quiet[Check[ToExpression["("<>Import[file,"Text"]<>")",InputForm,HoldComplete],$Failed]];
 names=DeleteDuplicates[Cases[held,s_Symbol/;Context[Unevaluated[s]]==="RGZPaper`":>
  Context[Unevaluated[s]]<>SymbolName[Unevaluated[s]],{0,Infinity},Heads->True]];
 If[!AllTrue[names,ToExpression[#,InputForm,Function[sym,OwnValues[sym]==={}&&DownValues[sym]==={}&&
  UpValues[sym]==={}&&SubValues[sym]==={}&&NValues[sym]==={},HoldAll]]&],
  Return[Failure["PaperSymbolsAssigned",<|"Action"->"Read the reference table in a fresh kernel or clear definitions in the RGZPaper` context."|>]]];
 p=Quiet[Check[ReleaseHold[held],$Failed]];
 If[!AssociationQ[p]||Lookup[p,"Schema",None]=!="RGZPRDFormFactors/1"||Lookup[p,"LoopOrder",None]=!=1||
 !ListQ[Lookup[p,"Rows",None]]||!ListQ[Lookup[p,"Functions",None]],Return[Failure["InvalidPaperResult",<||>]]];
 rows=p["Rows"];ids=({Lookup[#,"ColorID",None],Lookup[#,"SpinID",None]}&/@rows);
 If[Length[rows]=!=8||Sort[ids]=!=Sort[Tuples[{{1,2},Range[4]}]]||Length[p["Functions"]]=!=31||First[p["Functions"]]=!=1||
 !AllTrue[rows,AssociationQ[#]&&KeyExistsQ[#,"Prefactor"]&&ListQ[Lookup[#,"ReducedCoefficients",None]]&],Return[Failure["InvalidPaperResult",<||>]]];
 pairs=Flatten[Lookup[rows,"ReducedCoefficients"],1];
 If[Length[pairs]=!=144||!AllTrue[pairs,MatchQ[#,{_Integer,_}]&&0<=First[#]<31&]||
 !AllTrue[rows,DuplicateFreeQ[First/@#["ReducedCoefficients"]]&]||qqFFInvalidQ[p],Return[Failure["InvalidPaperResult",<||>]]];
 Join[p,<|"ReferenceOnly"->True,"CurrentTaskCertified"->False,"Provenance"->meta,"File"->file|>]
];
