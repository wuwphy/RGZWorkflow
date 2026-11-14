(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Specify the shared one-loop two-point topologies and their insertion routes.

   Inputs:
   Canonical FeynArts propagator and vertex types.

   Dependencies:
   FeynArts. No InsertFields call or graph task runs on load.
   Both mixed and standalone AA calculations use these exact topology definitions.
*)

(* Three sectors: every internal V/S line gets one FULL-propagator
   insertion; every internal U line gets NONE. Topology[2] gives 1/2,
   not two loops. No physical counterterms on the cubic/quartic vertices. *)
rgzLoopTopTadpole = TopologyList[Topology[2][
  Propagator[Incoming][Vertex[1][1],Vertex[4][3]],
  Propagator[Outgoing][Vertex[1][2],Vertex[4][3]],
  Propagator[FALoop[1]][Vertex[4][3],Vertex[2,1][4]],
  Propagator[FALoop[1]][Vertex[4][3],Vertex[2,1][4]]]];

rgzLoopTopBosonBubble = TopologyList[Topology[2][
  Propagator[Incoming][Vertex[1][1],Vertex[3][3]],
  Propagator[Outgoing][Vertex[1][2],Vertex[3][4]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[2,1][5]],
  Propagator[FALoop[1]][Vertex[3][4],Vertex[2,1][5]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[2,1][6]],
  Propagator[FALoop[1]][Vertex[3][4],Vertex[2,1][6]]]];

rgzLoopTopGhostBubble = TopologyList[Topology[2][
  Propagator[Incoming][Vertex[1][1],Vertex[3][3]],
  Propagator[Outgoing][Vertex[1][2],Vertex[3][4]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[3][4]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[3][4]]]];

