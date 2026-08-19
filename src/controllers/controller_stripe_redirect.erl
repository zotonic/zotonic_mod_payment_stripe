%% @copyright 2021-2026 Marc Worrell
%% @doc Stripe redirects the user with a GET to this controller
%% after a payment has been done at their HTML gateway.
%% This controller processes the payment status and then redirects
%% to the payment_psp_done page.
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

-module(controller_stripe_redirect).

-export([
    allowed_methods/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
    ]).

-include_lib("kernel/include/logger.hrl").

allowed_methods(Context) ->
    {[ <<"GET">>, <<"POST">> ], Context}.

resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    % Args: session_id / status=cancel|ok / payment_nr
    SessionId = z_context:get_q(<<"session_id">>, Context),
    case z_context:get_q(<<"status">>, Context) of
        <<"ok">> ->
            case m_payment_stripe_api:sync_payment_session_status(SessionId, Context) of
                {ok, {PaymentNr, _S}} ->
                    redirect(PaymentNr, Context);
                {error, _} ->
                    redirect(undefined, Context)
            end;
        <<"cancel">> ->
            case m_payment_stripe_api:expire_payment_session(SessionId, Context) of
                {ok, {PaymentNr, _S}} ->
                    redirect(PaymentNr, Context);
                {error, _} ->
                    redirect(undefined, Context)
            end;
        Status ->
            ?LOG_WARNING(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe redirect with unknown status">>,
                result => error,
                reason => unknown_status,
                status => Status,
                stripe_session_id => SessionId
            }),
            redirect(undefined, Context)
    end.

redirect(PaymentNr, Context) ->
    PaymentNr1 = case PaymentNr of
        undefined -> z_context:get_q(<<"payment_nr">>, Context);
        _ -> PaymentNr
    end,
    Args = [
        {payment_nr, PaymentNr1}
    ],
    Location = z_context:abs_url(
        z_dispatcher:url_for(payment_psp_done, Args, none, Context),
        Context),
    {{true, Location}, Context}.
