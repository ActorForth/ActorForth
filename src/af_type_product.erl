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
    Cont#continuation{data_stack = Rest, dictionary = Dict}.

%% Register a product type with auto-generated constructor and getters.
%% Updates both ETS (for compilation tools) and the local dictionary.
register_product_type(TypeName, Fields, Dict0) ->
    %% Register the type itself
    af_type:register_type(#af_type{name = TypeName}),
    Dict1 = af_type:dict_register_type(#af_type{name = TypeName}, Dict0),

    %% Constructor: lowercase type name, registered in Any
    ConstructorName = string:lowercase(atom_to_list(TypeName)),
    FieldTypes = [FType || {_, FType} <- Fields],
    ConstructorSigIn = lists:reverse(FieldTypes),
    Constructor = #operation{
        name = ConstructorName,
        sig_in = ConstructorSigIn,
        sig_out = [TypeName],
        impl = make_constructor(TypeName, Fields),
        source = auto
    },
    af_type:add_op('Any', Constructor),
    Dict2 = af_type:dict_add_op('Any', Constructor, Dict1),

    %% Getters: one per field, registered in the new type
    Dict3 = lists:foldl(fun({FieldName, FieldType}, DAcc) ->
        Getter = #operation{
            name = atom_to_list(FieldName),
            sig_in = [TypeName],
            sig_out = [FieldType],
            impl = make_getter(FieldName),
            source = auto
        },
        af_type:add_op(TypeName, Getter),
        af_type:dict_add_op(TypeName, Getter, DAcc)
    end, Dict2, Fields),

    %% Setters: field name with ! suffix, takes new value + instance
    %% Registered in Any so they're found when TOS is the new value type
    Dict4 = lists:foldl(fun({FieldName, FieldType}, DAcc) ->
        SetterName = atom_to_list(FieldName) ++ "!",
        Setter = #operation{
            name = SetterName,
            sig_in = [FieldType, TypeName],
            sig_out = [TypeName],
            impl = make_setter(FieldName),
            source = auto
        },
        af_type:add_op('Any', Setter),
        af_type:dict_add_op('Any', Setter, DAcc)
    end, Dict3, Fields),

    Dict4.

%% Build constructor function
%% Takes N values from stack (in order matching field list), creates instance
make_constructor(TypeName, Fields) ->
    NumFields = length(Fields),
    fun(Cont) ->
        {Values, Rest} = take_n(NumFields, Cont#continuation.data_stack),
        %% Values are in stack order (TOS first), fields are in definition order
        %% Stack: ... field1_val field2_val  (field2 is TOS)
        %% We need to pair fields with values in reverse
        ReversedValues = lists:reverse(Values),
        FieldMap = maps:from_list(
            lists:zipwith(
                fun({FName, _FType}, {_VType, VVal}) ->
                    {FName, {_VType, VVal}}
                end,
                Fields, ReversedValues
            )
        ),
        Instance = {TypeName, FieldMap},
        Cont#continuation{data_stack = [Instance | Rest]}
    end.

%% Build getter function (non-destructive: leaves instance on stack)
make_getter(FieldName) ->
    fun(Cont) ->
        [{_TypeName, FieldMap} = Instance | Rest] = Cont#continuation.data_stack,
        Value = maps:get(FieldName, FieldMap),
        Cont#continuation{data_stack = [Value, Instance | Rest]}
    end.

%% Build setter function: new_value instance setter! -> updated_instance
make_setter(FieldName) ->
    fun(Cont) ->
        [NewValue, {TypeName, FieldMap} | Rest] = Cont#continuation.data_stack,
        UpdatedMap = maps:put(FieldName, NewValue, FieldMap),
        Cont#continuation{data_stack = [{TypeName, UpdatedMap} | Rest]}
    end.

%% Take N items from top of stack
take_n(N, Stack) ->
    lists:split(N, Stack).
