%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2021 Maximonster Interactive Things
%% @doc Handle mailgun callbacks

%% Copyright 2020-2021 Maximonster Interactive Things
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

-module(controller_mailgun_webhook).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    is_authorized/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

is_authorized(Context) ->
    {Body, Context1} = wrq:req_body(Context),
    case mochijson:binary_decode(Body) of
        {struct, Parsed} ->
            case proplists:lookup(<<"signature">>, Parsed) of
                {<<"signature">>, {struct, SigProps}} ->
                    MySecret = m_config:get_value(mod_mailgun, webhook_secret, Context1),
                    Sig = z_convert:to_binary( proplists:get_value(<<"signature">>, SigProps) ),
                    Token = z_convert:to_binary( proplists:get_value(<<"token">>, SigProps) ),
                    Timestamp = z_convert:to_binary( proplists:get_value(<<"timestamp">>, SigProps) ),
                    MyHmacSig = crypto:mac(hmac, sha256, MySecret, <<Timestamp/binary, Token/binary>>),
                    case z_string:to_lower( z_convert:to_binary( z_utils:hex_encode(MyHmacSig) ) ) of
                        Sig ->
                            Context2 = z_context:set(json, Parsed, Context1),
                            {true, Context2};
                        MySig ->
                            lager:warning("Mailgun webhook: wrong signature ~p, expected ~p",
                                          [ Sig, MySig ]),
                            {false, Context1}
                    end;
                _ ->
                    {false, Context1}
            end;
        _ ->
            {false, Context1}
    end.

process(<<"POST">>, _, _, Context) ->
    Parsed = z_context:get(json, Context),
    case proplists:get_value(<<"event-data">>, Parsed) of
        {struct, EventData} ->
            handle_event(proplists:get_value(<<"event">>, EventData), EventData, Context);
        _ ->
            nop
    end,
    {true, Context}.


handle_event(<<"delivered">>, EventData, _Context) ->
    Recipient = proplists:get_value(<<"recipient">>, EventData),
    {struct, Message} = proplists:get_value(<<"message">>, EventData),
    {struct, Headers} = proplists:get_value(<<"headers">>, Message),
    MessageId = proplists:get_value(<<"message-id">>, Headers),
    StatusMessage = extract_status_message(proplists:get_value(<<"delivery-status">>, EventData)),
    z_email_server:delivery_report(relayed, Recipient, MessageId, StatusMessage);
handle_event(<<"opened">>, EventData, Context) ->
    Recipient = proplists:get_value(<<"recipient">>, EventData),
    lager:info("[mailgun] Opened email by ~s", [ Recipient ]),
    m_email_status:mark_read(Recipient, Context);
handle_event(<<"clicked">>, EventData, Context) ->
    Recipient = proplists:get_value(<<"recipient">>, EventData),
    lager:info("[mailgun] Clicked email by ~s", [ Recipient ]),
    m_email_status:mark_read(Recipient, Context);
handle_event(<<"failed">>, EventData, _Context) ->
    % Failure
    Recipient = proplists:get_value(<<"recipient">>, EventData),
    {struct, Message} = proplists:get_value(<<"message">>, EventData),
    {struct, Headers} = proplists:get_value(<<"headers">>, Message),
    MessageId = proplists:get_value(<<"message-id">>, Headers),
    StatusMessage = extract_status_message(proplists:get_value(<<"delivery-status">>, EventData)),
    case proplists:get_value(<<"log-level">>, EventData) of
        <<"warn">> ->
            % Temp failure
            z_email_server:delivery_report(temporary_failure, Recipient, MessageId, StatusMessage);
        <<"error">> ->
            % Permanent failure
            z_email_server:delivery_report(permanent_failure, Recipient, MessageId, StatusMessage);
        LogLevel ->
            % Unknown
            lager:warning("[mailgun] Unknown log level on failed email: ~p", [ LogLevel ]),
            ok
    end;
handle_event(<<"complained">>, EventData, Context) ->
    % Spam complaint -- disable user with a permanent failure
    Recipient = proplists:get_value(<<"recipient">>, EventData),
    lager:warning("[mailgun] Spam complaint from ~s", [ Recipient ]),
    z_notifier:notify(
        #email_failed{
            recipient = Recipient,
            is_final = true,
            status = <<"Spam complaint">>
        },
        Context);
handle_event(<<"unsubscribed">>, EventData, _Context) ->
    Recipient = proplists:get_value(<<"recipient">>, EventData),
    lager:warning("[mailgun] Unsubscribed event from ~s [unhandled]", [ Recipient ]),
    ok;
handle_event(_Event, _EventData, _Context) ->
    ok.


extract_status_message({struct, DeliveryStatus}) ->
    case proplists:lookup(<<"message">>, DeliveryStatus) of
        {<<"message">>, Message} when Message =/= <<>> ->
            map_status_message(Message);
        _ ->
            {<<"code">>, Code} = proplists:lookup(<<"code">>, DeliveryStatus),
            {<<"description">>, Desc} = proplists:lookup(<<"description">>, DeliveryStatus),
            iolist_to_binary([
                    z_convert:to_binary(Code), " ", map_status_message(Desc)
                ])
    end;
extract_status_message(_) ->
    <<>>.


map_status_message(<<"Too old">>) -> <<"Failed to deliver due to bounce. Check email address.">>;
map_status_message(Msg) -> Msg.

