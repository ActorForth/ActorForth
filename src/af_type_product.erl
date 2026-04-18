-module(af_type_product).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% Product type definition syntax:
%%   type Point x Int y Int .
%%
%% This registers:
%%   - Type 'Point' with fields [{x, 'Int'}, {y, 'Int'}]
%%   - Constructor 'point' in Any dict: takes Int Int from stack, builds Point
%%   - Getter 'x' in Point dict: pushes field value
%%   - Getter 'y' in Point dict: pushes field value

init() ->
    %% 'type' word starts a product type definition
    af_type:add_op('Any', #operation{
        name = "type",
        sig_in = [],
        sig_out = [],
        impl = fun op_type/1
    }),

    %% Register the compiler state types
    af_type:register_type(#af_type{name = 'TypeDefinition'}),
    af_type:register_type(#af_type{name = 'TypeFieldName'}),
    af_type:register_type(#af_type{name = 'TypeFieldType'}),

    %% . in TypeFieldType or TypeFieldName finishes definition
    af_type:add_op('TypeFieldType', #operation{
        name = ".",
        sig_in = ['TypeFieldType'],
        sig_out = [],
        impl = fun op_type_dot/1
    }),
    af_type:add_op('TypeFieldName', #operation{
        name = ".",
        sig_in = ['TypeFieldName'],
        sig_out = [],
        impl = fun op_type_dot/1
    }),

    %% Set handlers
    init_handlers(),
    ok.

init_handlers() ->
    af_type:register_type(#af_type{
        name = 'TypeDefinition',
        ops = get_ops('TypeDefinition'),
        handler = fun handle_type_definition/2
    }),
    af_type:register_type(#af_type{
        name = 'TypeFieldName',
        ops = get_ops('TypeFieldName'),
        handler = fun handle_field_name/2
    }),
    af_type:register_type(#af_type{
        name = 'TypeFieldType',
        ops = get_ops('TypeFieldType'),
        handler = fun handle_field_type/2
    }).

get_ops(TypeName) ->
    {ok, #af_type{ops = Ops}} = af_type:get_type(TypeName),
    Ops.

%% 'type' pushes TypeDefinition state
op_type(Cont) ->
    State = #{name => undefined, fields => []},
    Cont#continuation{
        data_stack = [{'TypeDefinition', State} | Cont#continuation.data_stack]
    }.

%% Handler: TypeDefinition — capture type name, transition to TypeFieldName
handle_type_definition(TokenValue, Cont) ->
    [{'TypeDefinition', State} | Rest] = Cont#continuation.data_stack,
    TypeName = list_to_atom(TokenValue),
    NewState = State#{name => TypeName},
    Cont#continuation{
        data_stack = [{'TypeFieldName', NewState} | Rest]
    }.

%% Handler: TypeFieldName — capture field name, transition to TypeFieldType
handle_field_name(TokenValue, Cont) ->
    [{'TypeFieldName', State} | Rest] = Cont#continuation.data_stack,
    FieldName = list_to_atom(TokenValue),
    NewState = State#{current_field => FieldName},
    Cont#continuation{
        data_stack = [{'TypeFieldType', NewState} | Rest]
    }.

%% Handler: TypeFieldType — capture field type, store pair, back to TypeFieldName
handle_field_type(TokenValue, Cont) ->
    [{'TypeFieldType', State} | Rest] = Cont#continuation.data_stack,
    #{fields := Fields, current_field := FieldName} = State,
    FieldType = list_to_atom(TokenValue),
    NewState = State#{
        fields => Fields ++ [{FieldName, FieldType}],
        current_field => undefined
    },
    Cont#continuation{
        data_stack = [{'TypeFieldName', NewState} | Rest]
    }.

%% . finishes product type definition
op_type_dot(Cont) ->
    %% Could be TypeFieldType or TypeFieldName on TOS
    [{_, State} | Rest] = Cont#continuation.data_stack,
    #{name := TypeName, fields := Fields} = State,
    Dict = register_product_type(TypeName, Fields, Cont#continuation.dictionary),
    Cont#continuation{data_stack = Rest, dictionary = Dict, dispatch_cache = #{}}.

%% Register a product type with auto-generated constructor and getters.
%%
%% NEW STORAGE FORMAT (2026-04-17):
%%
%%   Instance = {TypeName, V1, V2, ..., Vn}
%%
%% where field `i` (1-indexed, zero-indexed in the field list) is at tuple
%% position i+1, and `Vi` is the RAW value (not tagged). The tag lives only
%% on the outer tuple. Field types are known from the registry, so the
%% tag on individual field values is redundant.
%%
%% Previous format was `{TypeName, #{field => {Type, Value}}}`. The old
%% format cost a map allocation per update and a tagged tuple per field;
%% the new format makes field access a single BEAM `element/2` and
%% field update a single `setelement/3`.
%%
%% The #af_type{fields = [{FieldName, FieldType}, ...]} metadata carries
%% the field->position mapping so getters/setters can index correctly.
register_product_type(TypeName, Fields, Dict0) ->
    %% Register the type itself, recording field layout
    af_type:register_type(#af_type{name = TypeName, fields = Fields}),

    %% Constructor: lowercase type name, registered in Any
    ConstructorName = string:lowercase(atom_to_list(TypeName)),
    FieldTypes = [FType || {_, FType} <- Fields],
    ConstructorSigIn = lists:reverse(FieldTypes),
    Constructor = #operation{
        name = ConstructorName,
        sig_in = ConstructorSigIn,
        sig_out = [TypeName],
        impl = make_constructor(TypeName, length(Fields)),
        source = auto
    },
    af_type:add_op('Any', Constructor),

    %% Getters: one per field, registered in the new type. Each gets its
    %% 1-indexed tuple position baked in.
    lists:foldl(fun({FieldName, FieldType}, Pos) ->
        Getter = #operation{
            name = atom_to_list(FieldName),
            sig_in = [TypeName],
            sig_out = [FieldType, TypeName],
            impl = make_getter(FieldType, Pos),
            source = auto
        },
        af_type:add_op(TypeName, Getter),
        Pos + 1
    end, 2, Fields),  %% pos 1 is the type atom; fields start at pos 2

    %% Setters: field name with ! suffix, takes new value + instance
    lists:foldl(fun({FieldName, FieldType}, Pos) ->
        SetterName = atom_to_list(FieldName) ++ "!",
        Setter = #operation{
            name = SetterName,
            sig_in = [FieldType, TypeName],
            sig_out = [TypeName],
            impl = make_setter(Pos),
            source = auto
        },
        af_type:add_op('Any', Setter),
        Pos + 1
    end, 2, Fields),

    %% Update local dictionary if present
    case Dict0 of
        undefined -> undefined;
        _ -> sync_type_from_ets(TypeName, sync_type_from_ets('Any',
                af_type:dict_register_type(
                    #af_type{name = TypeName, fields = Fields}, Dict0)))
    end.

sync_type_from_ets(TypeName, Dict) ->
    case af_type:get_type(TypeName) of
        {ok, Type} -> maps:put(TypeName, Type, Dict);
        _ -> Dict
    end.

%% Build constructor: takes NumFields tagged stack items, strips tags,
%% builds a positional tuple {TypeName, V1, V2, ..., Vn}.
make_constructor(TypeName, NumFields) ->
    fun(Cont) ->
        {Values, Rest} = take_n(NumFields, Cont#continuation.data_stack),
        %% Values are in stack order (TOS first). Stack: ... val1 val2 ... valN
        %% where valN is TOS. Field order matches definition order, so:
        ReversedValues = lists:reverse(Values),
        RawVals = [V || {_, V} <- ReversedValues],
        Instance = list_to_tuple([TypeName | RawVals]),
        Cont#continuation{data_stack = [Instance | Rest]}
    end.

%% Build getter: non-destructive, leaves instance on stack, pushes tagged
%% {FieldType, FieldValue} on top.
make_getter(FieldType, Pos) ->
    fun(Cont) ->
        [Instance | Rest] = Cont#continuation.data_stack,
        RawVal = element(Pos, Instance),
        Cont#continuation{data_stack = [{FieldType, RawVal}, Instance | Rest]}
    end.

%% Build setter: pops tagged new value + instance, returns updated instance.
%% new_value is stripped of its tag; instance keeps its type tag.
make_setter(Pos) ->
    fun(Cont) ->
        [{_VType, NewVal}, Instance | Rest] = Cont#continuation.data_stack,
        UpdatedInstance = setelement(Pos, Instance, NewVal),
        Cont#continuation{data_stack = [UpdatedInstance | Rest]}
    end.

%% Take N items from top of stack
take_n(N, Stack) ->
    lists:split(N, Stack).
