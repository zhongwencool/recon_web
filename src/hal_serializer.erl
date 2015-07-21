
-module(hal_serializer).

-include("recon_web.hrl").
%% API
-export([to_json/1]).

%%TEST
%%-compile(export_all).

%%========================================
%% API
%%========================================

to_json(Resource) ->
    to_json2(res_to_json_object(Resource)).

to_json2({object, Object}) ->
    lager:debug("Object:~p~n",[Object]),
    jiffy:encode(Object);
to_json2({array, Array}) ->
    lager:debug("Array:~p~n",[Array]),
    jiffy:encode(Array).

%%========================================
%% INTERNAL
%%========================================

res_to_json_object(#hal_resource_data{rels = Rels,
    embedded = Embedded,properties = Properties}) ->
    object([
        {<<"_links">>, rels_to_json_object(Rels)} |
        [  {<<"_embedded">>, embeddeds_to_json_object(Embedded)} |
            prop_objects_to_json(Properties)
        ]
    ]).

%%%%%%%%%%%%%%
%%%rels
%%%%%%%%%%%%%%
rels_to_json_object(Rels) ->
    object([begin rels_to_json_object2(Rel)end||Rel<-Rels]).

rels_to_json_object2(#hal_rel_data{name = Name,
    links = Links}) ->
    {Name, rel_links_to_json3(Links)}.

rel_links_to_json3(Links) when is_list(Links) ->
    {array,[{link_to_json_object4(Link)} || Link <- Links]};
rel_links_to_json3(Link) ->
    object(link_to_json_object4(Link)).

link_to_json_object4(#hal_link_data{ href = Href,
    options = Options}) ->
    [{href,Href}|Options].

%%%%%%%%%%%%%%
%%%embeddeds
%%%%%%%%%%%%%%
embeddeds_to_json_object(Embeddeds)->
    object(embeddeds_to_json_object2(Embeddeds)).

embeddeds_to_json_object2(Embeddeds) ->
    [begin embedded_to_json_object3(Embedded) end||Embedded <-Embeddeds].

embedded_to_json_object3({Rel, Resources})when is_list(Resources) ->
    {Rel, array([res_to_json_object(R) || R <- Resources])};
embedded_to_json_object3({Rel, Resource}) ->
    {Rel, res_to_json_object(Resource)}.

%%%%%%%%%%%%%%
%%%property
%%%%%%%%%%%%%%
prop_objects_to_json(PropertyObjects) ->
    prop_objects_to_json2(PropertyObjects, []).

prop_objects_to_json2([], Acc) ->
    lists:flatten(Acc);
prop_objects_to_json2([Object | Tail], Acc) ->
    prop_objects_to_json2(Tail, [prop_object_to_json3(Object) | Acc]).

prop_object_to_json3({object, Properties}) ->
    object_props_to_json4(Properties).

object_props_to_json4(Properties) ->
    [begin {Key, prop_value_to_json_value5(Value)} end||{Key, Value} <- Properties].

prop_value_to_json_value5(Value = {object, _Properties}) ->
    object(prop_object_to_json3(Value));
prop_value_to_json_value5(Value) when is_list(Value) ->
    array([prop_value_to_json_value5(X) || X <- Value]);
prop_value_to_json_value5(Value) ->
    Value.


object(Properties) ->
    {object, {[build_property(Property) || Property <- Properties]}}.

array(Values) ->
    {array, [transform_value(Value) || Value <- Values]}.

build_property({Key, Value})->
    {transform_key(Key), transform_value(Value)}.

transform_key(Key) when is_binary(Key) ->
    Key;
transform_key(Key) when is_list(Key) ->
    erlang:list_to_binary(Key);
transform_key(Key) when is_atom(Key) ->
    Key;
transform_key(Key) when is_integer(Key) ->
    erlang:integer_to_binary(Key).

transform_value({object, Object}) ->
    Object;
transform_value({array, Array}) ->
    Array;
transform_value(Value) ->
    Value.

%%%%%%%%%%%%%%
%%%TEST
%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(HAL_RESOURCE(Rels, Embeddeds, Properties),
    #hal_resource_data{
        rels       = Rels,
        embedded   = Embeddeds,
        properties = Properties
    }).
-define(HAL_REL(Name, Links),
    #hal_rel_data{
        name  = Name,
        links = Links
    }).
-define(HAL_LINK(Href, Properties),
    #hal_link_data{
        href    = Href,
        options = Properties
    }).
-define(HAL_EMBEDDED(Name, Resources), {Name, Resources}).
-define(HAL_PROPERTY(Name, Value), {Name, Value}).
-define(HAL_PROPERTY_OBJECT(Properties), {object, Properties}).

empty_resource_to_json_test() ->
    JSON = hal_serializer:to_json(?HAL_RESOURCE([], [], [])),

    ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{}}">>).

resource_with_properties_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE([], [], [?HAL_PROPERTY_OBJECT([{1, 2}])])
    ),

    ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":2}">>).

resource_with_nested_properties_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE(
            [],
            [],
            [
                ?HAL_PROPERTY_OBJECT([
                    {1, ?HAL_PROPERTY_OBJECT([
                        {2, 3}
                    ])
                    }
                ])
            ]
        )
    ),

    ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":{\"2\":3}}">>).

resource_with_list_in_property_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE(
            [],
            [],
            [?HAL_PROPERTY_OBJECT([ {1, [2, 3, 4]} ])]
        )
    ),

    ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":[2,3,4]}">>).

resource_with_nested_collection_of_objects_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE(
            [],
            [],
            [
                ?HAL_PROPERTY_OBJECT([ {1, [
                    ?HAL_PROPERTY_OBJECT([{2, 3}]),
                    ?HAL_PROPERTY_OBJECT([{4, 5}])
                ]}
                ])
            ]
        )
    ),

    ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":[{\"2\":3},{\"4\":5}]}">>).

resource_with_links_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE(
            [?HAL_REL(self, ?HAL_LINK(<<"/self">>, []))],
            [],
            []
        )
    ),

    ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{}}">>).

resource_with_multiple_links_per_rel_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE(
            [?HAL_REL(test,
                 [
                  ?HAL_LINK(<<"/test">>, []),
                  ?HAL_LINK(<<"/test2">>, [])
                ])],
            [],
            []
        )
    ),

    ?assert(JSON == <<"{\"_links\":{\"test\":[{\"href\":\"/test\"},{\"href\":\"/test2\"}]},\"_embedded\":{}}">>).

resource_with_links_and_embedded_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE(
            [?HAL_REL(self, ?HAL_LINK(<<"/self">>, []))],
            [?HAL_EMBEDDED(<<"thing">>, ?HAL_RESOURCE([?HAL_REL(self, ?HAL_LINK(<<"/self/1">>, []))], [], []))],
            []
        )
    ),

    ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{\"thing\":{\"_links\":{\"self\":{\"href\":\"/self/1\"}},\"_embedded\":{}}}}">>).

resource_with_links_and_embeddeds_to_json_test() ->
    JSON = hal_serializer:to_json(
        ?HAL_RESOURCE(
            [?HAL_REL(self, ?HAL_LINK(<<"/self">>, []))],
            [?HAL_EMBEDDED(<<"things">>,
                [
                    ?HAL_RESOURCE([?HAL_REL(self, ?HAL_LINK(<<"/self/1">>, []))], [], []),
                    ?HAL_RESOURCE([?HAL_REL(self, ?HAL_LINK(<<"/self/2">>, []))], [], [])
                ]
            )
            ],
            []
        )
    ),

    ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{\"things\":[{\"_links\":{\"self\":{\"href\":\"/self/1\"}},\"_embedded\":{}},{\"_links\":{\"self\":{\"href\":\"/self/2\"}},\"_embedded\":{}}]}}">>).

%%to_json
create_empty_object_test() ->
    ?assert(object([]) == {object, {[]}}).

create_object_test() ->
    Object = object([{p1, v1}, {<<"p2">>, v2}, {<<"p3">>, "v3"} ]),
    ?assert(Object == {object, {[{p1, v1}, {<<"p2">>, v2}, {<<"p3">>, "v3"}]}}).

object_to_json_test() ->
    JSON = to_json2(object([{p1, v1}])),
    ?assert(JSON == <<"{\"p1\":\"v1\"}">>).

create_array_test() ->
    Array = array([1, 2]),
    ?assert(Array == {array, [1, 2]}).

array_to_json_test() ->
    JSON = to_json2(array([1, 2])),
    ?assert(JSON == <<"[1,2]">>).

complex_object_to_json_test() ->
    JSON = to_json2(
        object([{1, 2}, {a, object([{3, 4}])}])
    ),
    ?assert(JSON == <<"{\"1\":2,\"a\":{\"3\":4}}">>).

-endif.
