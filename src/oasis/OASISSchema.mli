
open OASISTypes

type 'a t = 'a OASISSchema_intern.t

(** [new_field schm plugin_id name value help pivot_data sync] 
    Create a field for a plugin. Create a new field for the plugin defined
    by [plugin_id] in the schema [schm]. The field basename is [name], for which 
    we will add the name of the plugin and ["X"] before -- to know that this 
    field is related to this plugin. [value] defines how to parse/print the
    valye. [help] is an helper text for this field. [pivot_data] and [sync] defines
    how to match back the datastructure where the result will be stored with 
    the field in _oasis. 

    The result of this function is a getter that helps you to get the data
    from the field parsed in the _oasis file. You should use it to generate
    a datastructure (the one that will be used by [sync]).
  *)
val new_field :
  ('b t) ->
  OASISPlugin.all_t ->
  name ->
  ?default:'a ->
  'a OASISValues.t -> 
  (unit -> string) -> 
  'c OASISPlugin.prop -> ('b -> 'c -> 'a) -> 
  PropList.Data.t -> 'a

(** Create a conditional field for a plugin. Sees {!new_field} for explanation.
  *)
val new_field_conditional :
  ('b t) ->
  OASISPlugin.all_t ->
  name ->
  ?default:'a ->
  'a OASISValues.t ->
  (unit -> string) -> 
  'c OASISPlugin.prop -> ('b -> 'c -> 'a OASISExpr.choices) -> 
  PropList.Data.t -> 
  'a OASISExpr.choices

