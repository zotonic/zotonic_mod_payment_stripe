%% Handle stripe callbacks

-module(controller_stripe_webhook).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    is_authorized/1,
    process/4
]).

-define(TIMESTAMP_TOLERANCE, 10).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

is_authorized(Context) ->
    {Body, Context1} = cowmachine_req:req_body(Context),
    case is_valid_signature(Body, Context1) of
        true ->
            {true, z_context:set(body, Body, Context1)};
        false ->
            lager:error("[mod_payment_stripe] Stripe webhook: rejected secret."),
            {<<"Stripe-Webhook-Secret">>, Context1}
    end.

process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
    Parsed = #{ <<"type">> := EventType } = z_json:decode(z_context:get(body, Context)),
    case handle(EventType, Parsed, Context) of
        ok ->
            {true, Context};
        {error, session_data} ->
            {{halt, 404}, Context};
        {error, _} ->
            {{halt, 500}, Context}
    end.

handle(<<"ping">>, _Ps, _Context) ->
    lager:debug("Stripe: pong"),
    ok;
handle(<<"checkout.session.async_payment_failed">>, Payload, Context) ->
    sync_session(Payload, Context);
handle(<<"checkout.session.async_payment_succeeded">>, Payload, Context) ->
    sync_session(Payload, Context);
handle(<<"checkout.session.completed">>, Payload, Context) ->
    sync_session(Payload, Context);
handle(<<"checkout.session.expired">>, Payload, Context) ->
    sync_session(Payload, Context);
handle(_Type, _Ps, _Context) ->
    ok.

sync_session(#{
        <<"object">> := <<"event">>,
        <<"data">> := #{
            <<"object">> := #{
                <<"object">> := <<"checkout.session">>,
                <<"id">> := _SessionId
            } = Session
        }
    }, Context) ->
    case m_payment_stripe_api:sync_payment_session_status(Session, Context) of
        {ok, {_PaymentNr, _Status}} ->
            ok;
        {error, _} = Error ->
            Error
    end;
sync_session(Payload, _Context) ->
    lager:error("[mod_payment_stripe] Unknown payload when processing webhook data ~p",
                [ Payload ]),
    {error, payload}.

is_valid_signature(Body, Context) ->
    case z_context:get_req_header(<<"stripe-signature">>, Context) of
        undefined ->
            false;
        <<>> ->
            false;
        Sig ->
            Ps = lists:map(
                fun(P) ->
                    [A,B] = binary:split(P, <<"=">>),
                    {A,B}
                end,
                binary:split(Sig, <<",">>, [ global ])),
            T = proplists:get_value(<<"t">>, Ps),
            V1 = proplists:get_value(<<"v1">>, Ps),
            Key = m_config:get_value(mod_payment_stripe, webhook_secret, Context),
            MySig = crypto:mac(hmac, sha256, Key, <<T/binary, ".", Body/binary>>),
            MySigHex = z_string:to_lower(z_utils:hex_encode(MySig)),
            TV = binary_to_integer(T),
            Now = z_datetime:timestamp(),
            (       MySigHex =:= V1
            andalso abs(Now - TV) < ?TIMESTAMP_TOLERANCE)
    end.
