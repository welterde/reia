%
% reiac: Command-line compiler for producing .beam files from .re files
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reiac).
-export([file/1,file/2]).

file(Filename) ->
  case filename:extension(Filename) of
    ".re" ->
      file(Filename, filename:basename(Filename, ".re") ++ ".beam");
    Ext ->
      {error, {bad_extension, Ext}}
  end.
  
file(Filename, Outfile) ->
  case file:read_file(Filename) of
    {ok, Data} ->
      case reia_parse:string(binary_to_list(Data)) of
        {ok, [{module, _, _, _}] = Forms} ->
          module(Filename, Outfile, Forms);
        {ok, [{class, _, Constant, _}] = Forms} ->
          {constant, _, Name} = Constant,
          case internal_class(Name) of
            true -> void;
            false -> io:format("Warning: reiac is intended for core Reia classes only~n")
          end,
          module(Filename, Outfile, Forms);
        {ok, _Forms} ->
          {error, "compiled Reia must define exactly one module or class"};
        {error, {Line, Message}} ->
          {error, io_lib:format("Line ~w: ~s", [Line, Message])}
      end;
    {error, Err} ->
      {error, io_lib:format("~p", [Err])}
  end.
  
module(Infile, Outfile, Forms) ->
  {ok, _Module, Bin} = forms(Infile, Forms),
  file:write_file(Outfile, Bin),
  {ok, Outfile}.
  
forms(FileName, Forms) ->
  Passes = [case Pass of dynamic -> static; _ -> Pass end || Pass <- reia_compiler:default_passes()],
  [ModuleDecl|Functions] = reia_compiler:compile(Forms, Passes),
  Attributes = [
    ModuleDecl,
    {attribute, 1, file, {FileName, 1}}, %% FIXME this is probably incorrect
    {attribute, 1, code, Forms}
  ],
  compile:forms(Attributes ++ Functions, [
    debug_info, 
    export_all, 
    verbose, 
    report_errors, 
    report_warnings
    %{parse_transform, smart_exceptions}
  ]).
  
internal_class('Object') ->
  true;
internal_class(_) ->
  false.