%% @doc Discover Checkout Sessions, the payment identity used by this module.
-module(payment_stripe_poll).
-export([page/2]).
-include_lib("zotonic_mod_payment/include/payment.hrl").

-spec page(#payment_psp_poll{}, z:context()) -> {ok, list(), term()} | {error, term()}.
page(#payment_psp_poll{since = Since, until = Until, cursor = Cursor}, Context) ->
    Args = [{limit, 100}, {"created[gte]", z_datetime:datetime_to_timestamp(Since)},
        {"created[lte]", z_datetime:datetime_to_timestamp(Until)}],
    Args1 = case Cursor of undefined -> Args; _ -> Args ++ [{starting_after, Cursor}] end,
    Path = "/v1/checkout/sessions?" ++ mochiweb_util:urlencode(Args1),
    case m_payment_stripe_api:api_call(get, Path, [], Context) of
        {ok, #{<<"data">> := Sessions, <<"has_more">> := More}} when is_boolean(More) ->
            ObservedAt = calendar:universal_time(),
            Payments = [payment(S, ObservedAt) || #{<<"mode">> := <<"payment">>} = S <- Sessions,
                maps:get(<<"payment_status">>, S) =/= <<"no_payment_required">>],
            Next = case More of
                false -> undefined;
                true -> maps:get(<<"id">>, lists:last(Sessions))
            end,
            {ok, Payments, Next};
        {error, _} = Error -> Error;
        _ -> {error, session_list}
    end.

payment(Session, ObservedAt) ->
    %% A list is a snapshot, not a webhook event. It is only used for discovery;
    %% known payments are skipped by the shared poller.
    {ok, Data} = m_payment_stripe_api:webhook_payment(Session,
        #{<<"created">> => z_datetime:datetime_to_timestamp(ObservedAt)}),
    Data#{created_at => calendar:system_time_to_universal_time(maps:get(<<"created">>, Session), second)}.
