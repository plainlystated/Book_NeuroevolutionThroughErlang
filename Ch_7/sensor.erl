%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sensor).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,{Id,Cx_PId,Scape,SensorName,VL,Fanout_PIds}} ->
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Fanout_PIds)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Fanout_PIds)->
	receive
		{Cx_PId,sync}->
			SensoryVector = sensor:SensorName(VL,Scape),
			[Pid ! {self(),forward,SensoryVector} || Pid <- Fanout_PIds],
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Fanout_PIds);
		{ExoSelf_PId,terminate} ->
			ok
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(VL,_Scape)->
	rng1(VL,[]).
rng1(0,Acc)->
	Acc;
rng1(VL,Acc)-> 
	rng1(VL-1,[random:uniform()|Acc]).
%rng/2 is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

xor_GetInput(VL,Scape)->
	Scape ! {self(),sense},
	receive
		{Scape,percept,SensoryVector}->
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:xor_sim/2, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
%xor_GetInput/2 contacts the XOR simulator and requests the sensory vector, which in this case should be a vector of length 2. The sensor checks that the incoming sensory signal, the percept, is indeed of length 2. If the vector length differs, then this is printed to the console and a dummy vector of appropriate length is constructed and used. This prevents unnecessary crashes in the case of errors, and gives the researcher a chance to fix the error and hotswap the code.

prompt_user(_VL, _Scape) ->
  {ok, N1} = io:fread("First number (0 or 1): ", "~d"),
  {ok, N2} = io:fread("Second number (0 or 1): ", "~d"),
  lists:flatten([N1, N2]).
