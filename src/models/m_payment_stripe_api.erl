%% @copyright 2021-2026 Marc Worrell
%% @doc API interface and (push) state handling for Stripe PSP
%% @end

%% Copyright 2021-2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_payment_stripe_api).

-export([
    create/2,

    payment_url/2,

    expire_payment_session/2,
    sync_payment_session_status/2,

    fetch_session/2,

    api_key/1
    ]).

-export([
    api_test/1,
    test/1
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include("zotonic_mod_payment/include/payment.hrl").


-define(BASE_URL, "https://api.stripe.com").
-define(TIMEOUT_REQUEST, 10000).
-define(TIMEOUT_CONNECT, 5000).



test(Context) ->
    PaymentRequest = #payment_request{
        key = undefined,
        user_id = undefined,
        amount = 1.0,
        currency = <<"EUR">>,
        language = z_context:language(Context),
        description_html = <<"Test">>,
        is_qargs = false,
        is_recurring_start = false,
        extra_props = [
            {email, <<"marc@worrell.nl">>},
            {name_surname, <<"Pietersen">>}
        ],
        preferred_psp_module = mod_payment_stripe
    },
    case z_notifier:first(PaymentRequest, Context) of
        #payment_request_redirect{ redirect_uri = RedirectUri } ->
            {ok, RedirectUri};
        Other ->
            Other
    end.


api_test(Context) ->
    PaymentNr = <<"foobar1234">>,
    SuccessUrl = z_context:abs_url(
        z_dispatcher:url_for(
            stripe_payment_redirect,
            [ {payment_nr, PaymentNr}, {status, "ok"} ],
            Context),
        Context),
    CancelUrl = z_context:abs_url(
        z_dispatcher:url_for(
            stripe_payment_redirect,
            [ {payment_nr, PaymentNr}, {status, "cancel"} ],
            Context),
        Context),
    Args = [
        {<<"mode">>, <<"payment">>},
        {<<"locale">>, <<"en">>},
        {<<"cancel_url">>, <<CancelUrl/binary, "&session_id={CHECKOUT_SESSION_ID}">>},
        {<<"success_url">>, <<SuccessUrl/binary, "&session_id={CHECKOUT_SESSION_ID}">>},
        {<<"customer_email">>, <<"marc@worrell.nl">>},
        {<<"line_items[0][price_data][currency]">>, <<"EUR">>},
        {<<"line_items[0][price_data][unit_amount]">>, <<"1234">>},
        {<<"line_items[0][price_data][product_data][name]">>, <<"Payment">>},
        {<<"line_items[0][price_data][product_data][description]">>, <<"hello">>},
        {<<"line_items[0][quantity]">>, <<"1">>},
        {<<"metadata[payment_nr]">>, PaymentNr},
        {<<"metadata[user_id]">>, <<"1">>}
    ],
    api_call(post, "/v1/checkout/sessions", Args, Context).


%% @doc Create a new session with Stripe
%% See https://stripe.com/docs/api/checkout/sessions/create#create_checkout_session-line_items-price_data
create(PaymentId, Context) ->
    {ok, Payment} = m_payment:get(PaymentId, Context),

    % For now, do not support recurring payments.
    false = maps:get(<<"is_recurring_start">>, Payment),

    Currency = maps:get(<<"currency">>, Payment),
    Amount = maps:get(<<"amount">>, Payment),
    PaymentNr = maps:get(<<"payment_nr">>, Payment),
    SuccessUrl = z_context:abs_url(
        z_dispatcher:url_for(
            stripe_payment_redirect,
            [ {payment_nr, PaymentNr}, {status, "ok"} ],
            Context),
        Context),
    CancelUrl = z_context:abs_url(
        z_dispatcher:url_for(
            stripe_payment_redirect,
            [ {payment_nr, PaymentNr}, {status, "cancel"} ],
            Context),
        Context),
    Email = maps:get(<<"email">>, Payment),
    Language = case maps:get(<<"language">>, Payment) of
        undefined -> z_context:language(Context);
        Lang -> Lang
    end,
    ContextLang = z_context:set_language(Language, Context),
    Args = [
        {<<"mode">>, <<"payment">>},
        {<<"locale">>, Language},
        {<<"cancel_url">>, <<CancelUrl/binary, "&session_id={CHECKOUT_SESSION_ID}">>},
        {<<"success_url">>, <<SuccessUrl/binary, "&session_id={CHECKOUT_SESSION_ID}">>},
        {<<"line_items[0][price_data][currency]">>, Currency},
        {<<"line_items[0][price_data][unit_amount]">>, erlang:round(Amount*100)},
        {<<"line_items[0][price_data][product_data][name]">>, ?__("Payment", ContextLang)},
        {<<"line_items[0][price_data][product_data][description]">>, valid_description( maps:get(<<"description">>, Payment) )},
        {<<"line_items[0][quantity]">>, <<"1">>},
        {<<"metadata[payment_nr]">>, PaymentNr},
        {<<"metadata[user_id]">>, maps:get(<<"user_id">>, Payment)}
    ] ++ customer_email(Email) ++ metadata(Payment),
    case api_call(post, "/v1/checkout/sessions", Args, Context) of
        {ok, #{
            <<"url">> := PaymentUrl,
            <<"id">> := StripeId
        } = JSON} ->
            m_payment_log:log(
                PaymentId,
                <<"CREATED">>,
                [
                    {psp_module, mod_payment_stripe},
                    {psp_external_log_id, StripeId},
                    {description, <<"Created Stripe payment ", StripeId/binary>>},
                    {request_result, JSON}
                ],
                Context),
            {ok, #payment_psp_handler{
                psp_module = mod_payment_stripe,
                psp_external_id = StripeId,
                psp_data = JSON,
                redirect_uri = PaymentUrl
            }};
        {ok, JSON} ->
            m_payment_log:log(
                PaymentId,
                <<"ERROR">>,
                [
                    {psp_module, mod_payment_stripe},
                    {description, "API Error creating order with Stripe"},
                    {request_result, JSON},
                    {request_args, Args}
                ],
                Context),
            ?LOG_ERROR(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe API returned unexpected payment create response">>,
                result => error,
                reason => json,
                payment_id => PaymentId,
                response => JSON
            }),
            {error, json};
        {error, Error} ->
            m_payment_log:log(
                PaymentId,
                <<"ERROR">>,
                [
                    {psp_module, mod_payment_stripe},
                    {description, "API Error creating order with Stripe"},
                    {request_result, Error},
                    {request_args, Args}
                ],
                Context),
            ?LOG_ERROR(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe API error creating payment">>,
                result => error,
                reason => Error,
                payment_id => PaymentId
            }),
            {error, Error}
    end.


metadata(#{ <<"props">> := Props }) ->
    lists:flatten([
        metadata(<<"reference">>, Props),
        metadata(<<"note">>, Props)
    ]).

metadata(K, Props) when is_map(Props) ->
    case maps:get(K, Props, undefined) of
        undefined -> [];
        V -> {<<"metadata[", K/binary, "]">>, V}
    end;
metadata(_, _Props) ->
    [].


customer_email(undefined) ->
    [];
customer_email(<<>>) ->
    [];
customer_email(Email) ->
    [{<<"customer_email">>, Email}].


valid_description(undefined) -> <<>>;
valid_description(D) when is_binary(D) -> D.


%% @doc Expire a payment session, this should be done after the cancel URL has been called.
-spec expire_payment_session(SessionId, Context) -> {ok, {PaymentNr, Status}} | {error, term()}
    when SessionId :: binary() | undefined,
         Context :: z:context(),
         PaymentNr :: binary(),
         Status :: new | pending | paid | cancelled.
expire_payment_session(undefined, _Context) ->
    {error, session_id};
expire_payment_session(SessionId, Context) ->
    DT = calendar:universal_time(),
    Url = "/v1/checkout/sessions/" ++ binary_to_list(SessionId) ++ "/expire",
    case fetch_session(SessionId, Context) of
        {ok, #{
            <<"status">> := <<"open">>
        }} ->
            case api_call(post, Url, [], Context) of
                {ok, #{
                    <<"status">> := <<"expired">>,
                    <<"mode">> := <<"payment">>,
                    <<"metadata">> := #{
                        <<"payment_nr">> := PaymentNr
                    }
                } = Session} ->
                    set_payment_status(PaymentNr, cancelled, DT, Session, Context);
                {ok, JSON} ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_payment_stripe,
                        text => <<"Stripe expire session returned unexpected status">>,
                        result => error,
                        reason => session_data,
                        stripe_session_id => SessionId,
                        stripe_session => JSON
                    }),
                    {error, session_data};
                {error, _} = Error ->
                    Error
            end;
        {ok, _} = SessionReturn ->
            sync_payment_session_status_1(SessionReturn, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Set the payment status from a session (map or id)
%% See also https://stripe.com/docs/api/checkout/sessions/object
-spec sync_payment_session_status(Session, Context) -> {ok, {PaymentNr, Status}} | {error, term()}
    when Session :: binary() | undefined | map(),
         Context :: z:context(),
         PaymentNr :: binary() | undefined,
         Status :: new | pending | paid | cancelled.
sync_payment_session_status(undefined, _Context) ->
    {error, session_id};
sync_payment_session_status(SessionId, Context) when is_binary(SessionId) ->
    sync_payment_session_status_1(fetch_session(SessionId, Context), Context);
sync_payment_session_status(#{ <<"object">> := <<"checkout.session">> } = Session, Context) ->
    sync_payment_session_status_1({ok, Session}, Context).

sync_payment_session_status_1({ok, Session}, Context) ->
    DT = calendar:universal_time(),
    case Session of
        #{
            <<"status">> := <<"open">>,
            <<"metadata">> := #{
                <<"payment_nr">> := PaymentNr
            }
        } ->
            % Checkout not yet started
            {ok, {PaymentNr, new}};
        #{
            <<"status">> := <<"complete">>,
            <<"payment_status">> := PaymentStatus,
            <<"mode">> := <<"payment">>,
            <<"metadata">> := #{
                <<"payment_nr">> := PaymentNr
            }
        } ->
            case PaymentStatus of
                <<"unpaid">> ->
                    % Payment in progress
                    set_payment_status(PaymentNr, pending, DT, Session, Context);
                <<"paid">> ->
                    set_payment_status(PaymentNr, paid, DT, Session, Context)
            end;
        #{
            <<"status">> := <<"expired">>,
            <<"mode">> := <<"payment">>,
            <<"metadata">> := #{
                <<"payment_nr">> := PaymentNr
            }
        } ->
            set_payment_status(PaymentNr, cancelled, DT, Session, Context);
        #{
            <<"id">> := SessionId,
            <<"status">> := <<"expired">>,
            <<"payment_status">> := <<"unpaid">>,
            <<"mode">> := <<"payment">>
        } ->
            ?LOG_INFO(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Ignoring expired Stripe payment session without payment_nr">>,
                result => ok,
                reason => no_payment_nr,
                stripe_session_id => SessionId,
                stripe_session_payment_link => maps:get(<<"payment_link">>, Session, undefined)
            }),
            {ok, {undefined, cancelled}};
        #{ <<"id">> := SessionId } ->
            ?LOG_ERROR(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe payment session has unexpected status">>,
                result => error,
                reason => session_data,
                stripe_session_id => SessionId,
                stripe_session => Session
            }),
            {error, session_data}
    end;
sync_payment_session_status_1({error, _} = Error, _Context) ->
    Error.


set_payment_status(PaymentNr, Status, DT, Session, Context) ->
    case m_payment:get(PaymentNr, Context) of
        {ok, #{ <<"id">> := PaymentId, <<"status">> := CurrentStatus }} ->
            m_payment_log:log(
                PaymentId,
                <<"stripe.session">>,
                #{
                    <<"psp_module">> => mod_payment_stripe,
                    <<"psp_external_log_id">> => maps:get(<<"id">>, Session, undefined),
                    <<"stripe_session">> => Session
                },
                Context),
            _ = maybe_update_contact(PaymentId, Session, CurrentStatus, Status, Context),
            case mod_payment:set_payment_status(PaymentId, Status, DT, Context) of
                ok -> {ok, {PaymentNr, Status}};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe status for unknown payment">>,
                result => error,
                reason => not_found,
                payment_nr => PaymentNr,
                stripe_session_id => maps:get(<<"id">>, Session, undefined)
            }),
            Error
    end.


%% @doc Retrieve a session
fetch_session(SessionId, Context) ->
    Url = "/v1/checkout/sessions/" ++ binary_to_list(SessionId),
    api_call(get, Url, [], Context).

maybe_update_contact(_PaymentId, _Session, _CurrentStatus, new, _Context) ->
    ok;
maybe_update_contact(PaymentId, Session, new, _Status, Context) ->
    case m_payment:maybe_update_contact(PaymentId, payment_link_contact(Session), Context) of
        ok ->
            ok;
        {error, need_contact} ->
            maybe_fetch_payment_link_contact(PaymentId, Session, Context);
        {error, _} = Error ->
            Error
    end;
maybe_update_contact(_PaymentId, _Session, _CurrentStatus, _Status, _Context) ->
    ok.

maybe_fetch_payment_link_contact(PaymentId, #{ <<"id">> := SessionId }, Context) ->
    case fetch_session(SessionId, Context) of
        {ok, Session} ->
            case m_payment:maybe_update_contact(PaymentId, payment_link_contact(Session), Context) of
                ok -> ok;
                {error, need_contact} -> ok;
                {error, _} = Error -> Error
            end;
        {error, _} ->
            ok
    end;
maybe_fetch_payment_link_contact(_PaymentId, _Session, _Context) ->
    ok.

payment_link_contact(Session) ->
    Customer = maps:get(<<"customer_details">>, Session, #{}),
    Address = maps:get(<<"address">>, Customer, #{}),
    maps:merge(
        address_props(Address),
        maps:merge(
            #{
                <<"email">> => maps:get(<<"email">>, Customer, maps:get(<<"customer_email">>, Session, undefined)),
                <<"phone">> => maps:get(<<"phone">>, Customer, undefined)
            },
            name_props(maps:get(<<"name">>, Customer, undefined)))).

name_props(Name) when is_binary(Name) ->
    case binary:split(z_string:trim(Name), <<" ">>, [global, trim_all]) of
        [] ->
            #{};
        [<<>>] ->
            #{};
        [Surname] ->
            #{ <<"name_surname">> => Surname };
        [First | Rest] ->
            #{ <<"name_first">> => First,
               <<"name_surname">> => iolist_to_binary(lists:join(<<" ">>, Rest)) }
    end;
name_props(_) ->
    #{}.

address_props(Address) when is_map(Address) ->
    #{
        <<"address_street_1">> => maps:get(<<"line1">>, Address, undefined),
        <<"address_street_2">> => maps:get(<<"line2">>, Address, undefined),
        <<"address_postcode">> => maps:get(<<"postal_code">>, Address, undefined),
        <<"address_city">> => maps:get(<<"city">>, Address, undefined),
        <<"address_state">> => maps:get(<<"state">>, Address, undefined),
        <<"address_country">> => maps:get(<<"country">>, Address, undefined)
    };
address_props(_) ->
    #{}.

%% @doc Return the URL to the status page on the Stripe dashboard.
-spec payment_url( Session, Context ) -> {ok, Url} | {error, term()}
    when Session :: binary() | map(),
         Context :: z:context(),
         Url :: binary().
payment_url(SessionId, Context) when is_binary(SessionId) ->
    case fetch_session(SessionId, Context) of
        {ok, Session} ->
            payment_url_fetched(Session);
        {error, _} = Error ->
            Error
    end;
payment_url(Session, Context) when is_map(Session) ->
    case payment_intent_url(Session) of
        {error, payment_intent} ->
            case maps:get(<<"id">>, Session, undefined) of
                SessionId when is_binary(SessionId), SessionId =/= <<>> ->
                    payment_url(SessionId, Context);
                _ ->
                    {error, payment_intent}
            end;
        Result ->
            Result
    end.

payment_url_fetched(Session) ->
    case payment_intent_url(Session) of
        {ok, _Url} = Result ->
            Result;
        {error, payment_intent} ->
            checkout_session_url(Session)
    end.

payment_intent_url(#{ <<"payment_intent">> := PaymentIntent } = Session)
    when is_binary(PaymentIntent), PaymentIntent =/= <<>> ->
    {ok, dashboard_url(Session, [<<"payments/">>, PaymentIntent])};
payment_intent_url(_Session) ->
    {error, payment_intent}.

checkout_session_url(#{ <<"id">> := SessionId } = Session)
    when is_binary(SessionId), SessionId =/= <<>> ->
    {ok, dashboard_url(Session, [<<"checkout/sessions/">>, SessionId])};
checkout_session_url(_Session) ->
    {error, payment_intent}.

dashboard_url(Session, Path) ->
    iolist_to_binary([
        dashboard_url_prefix(Session),
        Path
    ]).

dashboard_url_prefix(#{ <<"livemode">> := true }) ->
    <<"https://dashboard.stripe.com/">>;
dashboard_url_prefix(_Session) ->
    <<"https://dashboard.stripe.com/test/">>.

api_call(Method, Endpoint, Args, Context) ->
    case api_key(Context) of
        {ok, ApiKey} ->
            Args1 = lists:map(
                fun({K, V}) ->
                    {z_convert:to_binary(K), z_convert:to_binary(V)}
                end,
                Args),
            Body = cow_qs:qs(Args1),
            Url = ?BASE_URL ++ z_convert:to_list(Endpoint),
            Hs = [
                {"Authorization", "Bearer " ++ z_convert:to_list(ApiKey)}
            ],
            Request = case Method of
                get when Body =/= <<>> ->
                    {Url ++ "?" ++ z_convert:to_list(Body), Hs};
                get ->
                    {Url ++ "?" ++ z_convert:to_list(Body), Hs};
                post ->
                    {Url, Hs, "application/x-www-form-urlencoded", Body}
            end,
            ?LOG_DEBUG(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe API call">>,
                method => Method,
                endpoint => Endpoint,
                url => Url
            }),
            case httpc:request(
                Method, Request,
                [
                    {autoredirect, true},
                    {relaxed, false},
                    {timeout, ?TIMEOUT_REQUEST},
                    {connect_timeout, ?TIMEOUT_CONNECT}
                ],
                [
                    {sync, true},
                    {body_format, binary}
                ])
            of
                {ok, {{_, X20x, _}, Headers, Payload}} when ((X20x >= 200) and (X20x < 400)) ->
                    case proplists:get_value("content-type", Headers) of
                        undefined ->
                            {ok, Payload};
                        ContentType ->
                            case binary:match(list_to_binary(ContentType), <<"json">>) of
                                nomatch ->
                                    {ok, Payload};
                                _ ->
                                    Props = jsx:decode(Payload, [return_maps]),
                                    {ok, Props}
                            end
                    end;
                {ok, {{_, Code, _}, Headers, Payload}} ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_payment_stripe,
                        text => <<"Stripe API error">>,
                        result => error,
                        reason => {code, Code},
                        payload => Payload,
                        headers => Headers
                    }),
                    {error, Code};
                {error, _} = Error ->
                    Error
            end;
        {error, enoent} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe API key not set">>,
                result => error,
                reason => api_key_not_set
            }),
            {error, api_key_not_set}
    end.

%% @doc Return the secret API key to communicate with Stripe
api_key(Context) ->
    case m_config:get_value(mod_payment_stripe, secret_key, Context) of
        undefined -> {error, enoent};
        <<>> ->  {error, enoent};
        ApiKey -> {ok, ApiKey}
    end.
