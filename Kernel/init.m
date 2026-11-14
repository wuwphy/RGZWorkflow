(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Load the standard Wolfram package entry point.

   Inputs:
   A Wolfram package load request.

   Dependencies:
   RGZWorkflow.wl.
*)

Get[FileNameJoin[{DirectoryName[$InputFileName],"RGZWorkflow.wl"}]];
