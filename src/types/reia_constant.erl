-module(reia_constant).
-export([funcall/3]).

funcall({constant, Name}, inspect, []) ->
  case code:ensure_loaded(Name) of
    {module, _Name} ->
      reia_string:from_list(atom_to_list(Name));
    {error, _Error} ->
      throw({error, {Name, "not loaded"}})
  end;  
funcall(Constant, to_s, []) ->
  funcall(Constant, inspect, []);
funcall({constant, Name}, Method, Arguments) ->
  apply(Name, Method, Arguments).
