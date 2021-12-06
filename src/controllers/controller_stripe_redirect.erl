%% @copyright 2021 Marc Worrell
%% @doc Strips redirect the user with a GET to this controller
%%      after a payment has been done at their HTML gateway.
%%      This controller processes the payment status and then redirects
%%      to either the payment_psp_done or payment_psp_cancel page.

%% Copyright 2021 Marc Worrrell
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
                {ok, {PaymentNr, S}} ->
                    redirect(disp(S), PaymentNr, Context);
                {error, _} ->
                    redirect(payment_psp_cancel, undefined, Context)
            end;
        <<"cancel">> ->
            case m_payment_stripe_api:expire_payment_session(SessionId, Context) of
                {ok, {PaymentNr, S}} ->
                    redirect(disp(S), PaymentNr, Context);
                {error, _} ->
                    redirect(payment_psp_cancel, undefined, Context)
            end;
        Status ->
            lager:warning("[stripe] redirect with unknown status ~p", [ Status ]),
            redirect(payment_psp_cancel, undefined, Context)
    end.

disp(cancelled) -> payment_psp_cancel;
disp(expired) -> payment_psp_cancel;
disp(failed) -> payment_psp_cancel;
disp(paid) -> payment_psp_done;
disp(pending) -> payment_psp_done.

redirect(Dispatch, PaymentNr, Context) ->
    Args = [
        {payment_nr, PaymentNr}
    ],
    Location = z_context:abs_url(z_dispatcher:url_for(Dispatch, Args, Context), Context),
    {{true, Location}, Context}.
