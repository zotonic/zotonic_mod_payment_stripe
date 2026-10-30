%% @copyright 2026 Marc Worrell
%% @doc Discover Checkout Sessions, the payment identity used by this module.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(payment_stripe_poll).
-export([page/2]).
-include_lib("zotonic_mod_payment/include/payment.hrl").

-spec page(#payment_psp_poll{}, z:context()) -> {ok, list(), term()} | {error, term()}.
page(#payment_psp_poll{cursor = {events, Cursor}} = Request, Context) ->
    events(Request#payment_psp_poll{cursor = Cursor}, Context);
page(#payment_psp_poll{since = Since, until = Until, cursor = Cursor}, Context) ->
    Args = [{limit, 100}, {"created[gte]", z_datetime:datetime_to_timestamp(Since)},
        {"created[lte]", z_datetime:datetime_to_timestamp(Until)}],
    Args1 = case Cursor of undefined -> Args; _ -> Args ++ [{starting_after, Cursor}] end,
    Path = "/v1/checkout/sessions?" ++ mochiweb_util:urlencode(Args1),
    case m_payment_stripe_api:api_call(get, Path, [], Context) of
        {ok, #{<<"data">> := Sessions, <<"has_more">> := More}} when is_boolean(More) ->
            Payments = [payment(S) || #{<<"mode">> := <<"payment">>} = S <- Sessions,
                maps:get(<<"payment_status">>, S) =/= <<"no_payment_required">>],
            Next = case More of
                false -> {events, undefined};
                true -> maps:get(<<"id">>, lists:last(Sessions))
            end,
            {ok, Payments, Next};
        {error, _} = Error -> Error;
        _ -> {error, session_list}
    end.

%% The creation snapshot must not outrank later provider events in this poll.
payment(Session) ->
    {ok, Data} = m_payment_stripe_api:webhook_payment(Session,
        #{<<"created">> => maps:get(<<"created">>, Session)}),
    Data#{created_at => calendar:system_time_to_universal_time(maps:get(<<"created">>, Session), second)}.

%% Stripe retains Events for 30 days. Session creation remains a separate pass
%% for open sessions and older catch-up windows; it cannot recover old events.
events(#payment_psp_poll{since = Since, until = Until, cursor = Cursor}, Context) ->
    Types = [<<"checkout.session.completed">>, <<"checkout.session.expired">>,
        <<"checkout.session.async_payment_succeeded">>, <<"checkout.session.async_payment_failed">>,
        <<"charge.refunded">>],
    Args = [{limit, 100}, {"created[gte]", z_datetime:datetime_to_timestamp(Since)},
        {"created[lte]", z_datetime:datetime_to_timestamp(Until)}]
        ++ [{"types[]", T} || T <- Types]
        ++ case Cursor of undefined -> []; _ -> [{starting_after, Cursor}] end,
    Path = "/v1/events?" ++ mochiweb_util:urlencode(Args),
    case m_payment_stripe_api:api_call(get, Path, [], Context) of
        {ok, #{<<"data">> := Events, <<"has_more">> := More}} when is_boolean(More) ->
            case event_payments(Events, Context, []) of
                {ok, Payments} ->
                    Next = case More of
                        false -> undefined;
                        true -> {events, maps:get(<<"id">>, lists:last(Events))}
                    end,
                    {ok, Payments, Next};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error;
        _ -> {error, event_list}
    end.

event_payments([], _, Acc) -> {ok, lists:reverse(Acc)};
event_payments([E | Rest], Context, Acc) ->
    case event_payment(E, Context) of
        {ok, Data} -> event_payments(Rest, Context, [Data | Acc]);
        ignore -> event_payments(Rest, Context, Acc);
        {error, _} = Error -> Error
    end.

event_payment(#{<<"type">> := <<"checkout.session.", _/binary>>,
                <<"data">> := #{<<"object">> := #{<<"mode">> := <<"payment">>} = Session}} = E, _) ->
    case maps:get(<<"payment_status">>, Session) of
        <<"no_payment_required">> -> ignore;
        _ -> event_session(Session, E)
    end;
event_payment(#{<<"type">> := <<"charge.refunded">>,
                <<"data">> := #{<<"object">> := #{<<"refunded">> := true,
                    <<"payment_intent">> := Intent}}} = E, Context) when is_binary(Intent) ->
    %% Resolve to the Checkout Session identity; do not create a second charge payment.
    Path = "/v1/checkout/sessions?" ++ mochiweb_util:urlencode([{payment_intent, Intent}, {limit, 1}]),
    case m_payment_stripe_api:api_call(get, Path, [], Context) of
        {ok, #{<<"data">> := [#{<<"mode">> := <<"payment">>} = Session]}} ->
            case event_session(Session, E) of
                {ok, Data} -> {ok, Data#{status => refunded}};
                {error, _} = Error -> Error
            end;
        {ok, #{<<"data">> := []}} -> ignore;
        {ok, #{<<"data">> := [#{<<"mode">> := _}]}} -> ignore;
        {error, _} = Error -> Error;
        _ -> {error, refund_session}
    end;
event_payment(_, _) -> ignore.

event_session(Session, #{<<"created">> := Time} = Event) ->
    case m_payment_stripe_api:webhook_payment(Session, Event) of
        {ok, Data} -> {ok, Data#{updated_at => calendar:system_time_to_universal_time(Time, second)}};
        {error, _} = Error -> Error
    end.
