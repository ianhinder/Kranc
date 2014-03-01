
(*  Copyright 2004-2013 Sascha Husa, Ian Hinder, Christiane Lechner

    This file is part of Kranc.

    Kranc is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Kranc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

BeginPackage[
  "CodeGenConfiguration`",
  {"Errors`", "Helpers`", "Kranc`", "CodeGenKranc`", "DGFE`", "OpenCL`",
   "Vectorisation`", "CaKernel`"}];

CreateConfiguration::usage = "Create the content of the configuration.ccl file.";

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Configuration file
   ------------------------------------------------------------------------ *)

Options[CreateConfiguration] = ThornOptions;

CreateConfiguration[opts:OptionsPattern[]] :=
  {FileHeader["CCL"],
   "REQUIRES GenericFD\n",
    If[OptionValue[UseLoopControl],
      If[OptionValue[UseVectors], 
        "REQUIRES LoopControl\n", "OPTIONAL LoopControl\n{\n}\n"],{}],
   If[OptionValue[UseDGFE], DGFEConfigurationCCL[], {}],
   If[OptionValue[UseOpenCL], OpenCLConfigurationCCL[], {}],
   If[OptionValue[UseVectors], VectorisationConfigurationCCL[], {}],
   If[OptionValue[UseCaKernel], CaKernelConfigurationCLL[], {}]
  };

End[];

EndPackage[];
