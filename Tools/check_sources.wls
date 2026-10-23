(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Syntax only; never evaluates source or tests. *)
root=DirectoryName[DirectoryName[$InputFileName]];
files=Join@@(FileNames["*",FileNameJoin[{root,#}],Infinity]&/@{"Kernel","Models","Tests","Tools"});
files=Select[files,FileType[#]===File&&MemberQ[{"wl","m","wls","wlt","fr","mod","gen"},FileExtension[#]]&];
failed=Select[files,!TrueQ[SyntaxQ[Import[#,"Text"]]]&];
Print[<|"Files"->Length[files],"SyntaxFailures"->(FileNameDrop[#,FileNameDepth[root]]&/@failed)|>];
Exit[If[failed==={},0,1]];
