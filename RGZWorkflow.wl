(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
Begin["RGZWorkflow`Loader`"];
(* Portable loader. Remove only undefined notebook placeholders for public API
   names before BeginPackage; preserve all user definitions. *)
Module[{packageFile=FileNameJoin[{DirectoryName[$InputFileName],"Kernel","RGZWorkflow.wl"}],names},
 names=StringCases[Import[packageFile,"Text"],RegularExpression["(?m)^(RGZ[A-Za-z0-9]+|\\$RGZWorkflowVersion)::usage"]->"$1"];
 names=Join[names,{"name","sym"}]; (* Undefined placeholders leaked by the previous loader. *)
 If[MemberQ[$Packages,"FeynCalc`"],names=Join[names,{"$LoadAddOns","$FeynCalcStartupMessages"}]];
 Scan[Function[name,If[Names["Global`"<>name]=!={},
  ToExpression["Global`"<>name,InputForm,Function[sym,
   If[OwnValues[sym]==={}&&DownValues[sym]==={}&&UpValues[sym]==={}&&SubValues[sym]==={}&&NValues[sym]==={}&&DefaultValues[sym]==={}&&FormatValues[sym]==={}&&Options[sym]==={}&&Attributes[sym]==={},Remove[sym]],HoldAll]]]],names];
 Get[packageFile]
];
End[];
