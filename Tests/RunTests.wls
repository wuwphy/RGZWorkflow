(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
$RGZTestProject=DirectoryName[DirectoryName[$InputFileName]];
report=TestReport[FileNameJoin[{DirectoryName[$InputFileName],"Core.wlt"}]];
Print[<|"Succeeded"->report["TestsSucceededCount"],"Failed"->report["TestsFailedCount"]|>];
Exit[If[report["TestsFailedCount"]===0,0,1]];
