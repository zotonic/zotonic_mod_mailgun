%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2024 Maximonster Interactive Things
%% @doc Handle mailgun callbacks
%% @end

%% Copyright 2020-2024 Maximonster Interactive Things
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
    accepted_content_types/1,
    is_authorized/1,
    process/4,

    handle_event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

accepted_content_types(Context) ->
    {[
        {<<"application">>, <<"json">>, []}
    ], Context}.

is_authorized(Context) ->
    {Parsed, Context1} = z_controller_helper:decode_request({<<"application">>, <<"json">>, []}, Context),
    case maps:get(<<"signature">>, Parsed, undefined) of
        SigProps when is_map(SigProps) ->
            MySecret = m_config:get_value(mod_mailgun, webhook_secret, Context1),
            Sig = z_convert:to_binary( maps:get(<<"signature">>, SigProps, <<>>) ),
            Token = z_convert:to_binary( maps:get(<<"token">>, SigProps, <<>>) ),
            Timestamp = z_convert:to_binary( maps:get(<<"timestamp">>, SigProps, <<>>) ),
            MyHmacSig = crypto:mac(hmac, sha256, MySecret, <<Timestamp/binary, Token/binary>>),
            case z_string:to_lower( z_convert:to_binary( z_utils:hex_encode(MyHmacSig) ) ) of
                Sig ->
                    Context2 = z_context:set(json, Parsed, Context1),
                    {true, Context2};
                MySig ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_mailgun,
                        text => <<"Mailgun webhook: wrong signature on event">>,
                        signature => Sig,
                        expected => MySig
                    }),
                    {false, Context1}
            end;
        _ ->
            ?LOG_WARNING(#{
                in => zotonic_mod_mailgun,
                text => <<"Mailgun webhook: no signature on event">>
            }),
            {false, Context1}
    end.

process(<<"POST">>, _, _, Context) ->
    Parsed = z_context:get(json, Context),
    case maps:get(<<"event-data">>, Parsed, undefined) of
        EventData when is_map(EventData) ->
            handle_event(EventData, Context);
        _ ->
            nop
    end,
    {true, Context}.


handle_event(#{ <<"event">> := <<"delivered">> } = EventData, _Context) ->
    Recipient = maps:get(<<"recipient">>, EventData),
    Message = maps:get(<<"message">>, EventData),
    Headers = maps:get(<<"headers">>, Message),
    MessageId = maps:get(<<"message-id">>, Headers),
    StatusMessage = extract_status_message(maps:get(<<"delivery-status">>, EventData)),
    z_email_server:delivery_report(relayed, Recipient, MessageId, StatusMessage);
handle_event(#{ <<"event">> := <<"opened">> } = EventData, Context) ->
    Recipient = maps:get(<<"recipient">>, EventData),
    ?LOG_INFO(#{
        in => zotonic_mod_mailgun,
        text => <<"Open event on email via Mailgun">>,
        recipient => Recipient
    }),
    m_email_status:mark_read(Recipient, Context);
handle_event(#{ <<"event">> := <<"clicked">> } = EventData, Context) ->
    Recipient = maps:get(<<"recipient">>, EventData),
    ?LOG_INFO(#{
        in => zotonic_mod_mailgun,
        text => <<"Click event on email via Mailgun">>,
        recipient => Recipient
    }),
    m_email_status:mark_read(Recipient, Context);
handle_event(#{ <<"event">> := <<"failed">> } = EventData, _Context) ->
    % Failure
    Recipient = maps:get(<<"recipient">>, EventData),
    Message = maps:get(<<"message">>, EventData, #{}),
    Headers = maps:get(<<"headers">>, Message, #{}),
    MessageId = maps:get(<<"message-id">>, Headers, maps:get(<<"id">>, EventData, <<>>)),
    StatusMessage = extract_status_message(maps:get(<<"delivery-status">>, EventData)),
    case maps:get(<<"severity">>, EventData, undefined) of
        <<"permanent">> ->
            % Permanent failure
            z_email_server:delivery_report(permanent_failure, Recipient, MessageId, StatusMessage);
        LogLevel ->
            % Unknown
            ?LOG_WARNING(#{
                in => zotonic_mod_mailgun,
                text => <<"Unknown or missing log level on failed email - assuming temporary_failure">>,
                log_level => LogLevel,
                event_data => EventData
            }),
            z_email_server:delivery_report(temporary_failure, Recipient, MessageId, StatusMessage)
    end;
handle_event(#{ <<"event">> := <<"complained">> } = EventData, Context) ->
    % Spam complaint -- disable user with a permanent failure
    Recipient = maps:get(<<"recipient">>, EventData),
    Message = maps:get(<<"message">>, EventData, #{}),
    Headers = maps:get(<<"headers">>, Message, #{}),
    MessageId = maps:get(<<"message-id">>, Headers, maps:get(<<"id">>, EventData, <<>>)),
    ?LOG_WARNING(#{
        in => zotonic_mod_mailgun,
        text => <<"Spam complaint received via Mailgun">>,
        recipient => Recipient,
        message_id => MessageId
    }),
    z_notifier:notify(
        #email_failed{
            message_nr = MessageId,
            recipient = Recipient,
            is_final = true,
            status = <<"Spam complaint">>,
            reason = error
        },
        Context);
handle_event(#{ <<"event">> := <<"unsubscribed">> } = EventData, _Context) ->
    Recipient = maps:get(<<"recipient">>, EventData, <<>>),
    ?LOG_WARNING(#{
        in => zotonic_mod_mailgun,
        text => <<"Unsubscribed event received via Mailgun - ignored">>,
        recipient => Recipient
    }),
    ok;
handle_event(_Event, _Context) ->
    ok.


extract_status_message(#{ <<"message">> := Message }) when Message =/= <<>> ->
    map_status_message(Message);
extract_status_message(#{ <<"code">> := Code, <<"description">> := Desc }) ->
    iolist_to_binary([
            z_convert:to_binary(Code), " ", map_status_message(Desc)
        ]);
extract_status_message(_) ->
    <<>>.


map_status_message(<<"Too old">>) -> <<"Failed to deliver due to bounce. Check email address.">>;
map_status_message(Msg) -> Msg.

