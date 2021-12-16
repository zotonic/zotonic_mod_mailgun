%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2021 Maximonster Interactive Things
%% @doc Integration of mailgun as email relay.

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

-module(mod_mailgun).

-mod_title("Mailgun email relay").
-mod_description("Integrate mailgun as email relay.").
-mod_depends([ mod_email_status ]).

-export([
    observe_email_status/2,
    observe_email_send_encoded/2
    ]).

% Testing
-export([
    mailgun_block/2,
    mailgun_clear/2
    ]).

-define(MAILGUN_API_URL, "https://api.mailgun.net/v3/").

-include_lib("zotonic_core/include/zotonic.hrl").


observe_email_status(#email_status{ is_manual = true, is_valid = false, recipient = Email }, Context) ->
    mailgun_block(Email, Context);
observe_email_status(#email_status{ is_manual = true, is_valid = true, recipient = Email }, Context) ->
    mailgun_clear(Email, Context);
observe_email_status(#email_status{}, _Context) ->
    ok.


observe_email_send_encoded(#email_send_encoded{
            message_id = MsgId,
            from = _From,
            to = To,
            encoded = EncodedEmail,
            options = _Options
        },
        Context) ->
    case is_mailgun_configured(Context) of
        true ->
            send_email(MsgId, To, EncodedEmail, Context);
        false ->
            undefined
    end.

send_email(MsgId, To, EncodedEmail, Context) ->
    ApiUrl = mailgun_api_url(Context),
    lager:info("[smtp] Sending email to <~s> (~s), via mailgun \"~s\"",
               [To, MsgId, ApiUrl]),
    PostData = [
        #{
            name => <<"to">>,
            value => To
        },
        #{
            name => <<"message">>,
            data => EncodedEmail,
            filename => <<"message.eml">>,
            mime => <<"message/rfc822">>
        }
    ],
    {Body, CT} = z_multipart_encode:encode(PostData),
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    Auth = base64:encode_to_string( "api:" ++ z_convert:to_list(ApiKey) ),
    Hs = [
        {"Content-Length", integer_to_list(size(Body))},
        {"Authorization", "Basic " ++ Auth}
    ],
    case httpc:request(
        post,
        {ApiUrl ++ "/messages.mime", Hs, z_convert:to_list(CT), Body},
        httpc_http_options(),
        httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            #{
                <<"id">> := MsgId,
                <<"message">> := Message
            } = z_json:decode(Payload),
            {ok, <<Message/binary, " ", MsgId/binary>>};
        {ok, {{_, EAccess, _}, _Headers, _Payload}}
            when EAccess =:= 401;
                 EAccess =:= 403 ->
            {error, eacces, {temporary_failure, "mailgun.com", <<"401 error on Mailgun API - check the API key">>}};
        {ok, {{_, Err, _}, _Headers, _Payload}}
            when Err >= 500 ->
            ErrB = z_convert:to_binary(Err),
            {error, internal_error, {temporary_failure, "mailgun.com", <<ErrB/binary, " error on Mailgun API">>}};
        {ok, {{_, Err, _}, _Headers, _Payload}} ->
            ErrB = z_convert:to_binary(Err),
            {error, unknown, {temporary_failure, "mailgun.com", <<ErrB/binary, " error on Mailgun API">>}};
        {error, _} = Error ->
            Error
    end.

mailgun_clear(Email, Context) ->
    case is_mailgun_configured(Context) of
        true ->
            Url = iolist_to_binary([ "/bounces/", z_url:url_encode(Email) ]),
            Payload = <<>>,
            mailgun_api(delete, Url, Payload, Context);
        false ->
            ok
    end.

mailgun_block(Email, Context) ->
    case is_mailgun_configured(Context) of
        true ->
            Url = iolist_to_binary([ "/bounces" ]),
            Payload = iolist_to_binary([
                    "address=", z_url:url_encode(Email)
                ]),
            mailgun_api(post, Url, Payload, Context);
        false ->
            ok
    end.

is_mailgun_configured(Context) ->
    ApiUrl = z_convert:to_binary( m_config:get_value(mod_mailgun, api_url, Context) ),
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    case {ApiUrl, ApiKey} of
        {<<>>, <<>>} ->
            false;
        _ ->
            true
    end.

mailgun_api(delete, Url, _Payload, Context) ->
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    Auth = base64:encode_to_string( "api:" ++ z_convert:to_list(ApiKey) ),
    Hs = [
        {"Authorization", "Basic " ++ Auth}
    ],
    httpc:request(
        delete,
        {mailgun_api_url(Context) ++ z_convert:to_list(Url), Hs},
        httpc_http_options(),
        httpc_options());
mailgun_api(post, Url, Payload, Context) ->
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    Auth = base64:encode_to_string( "api:" ++ z_convert:to_list(ApiKey) ),
    Hs = [
        {"Authorization", "Basic " ++ Auth}
    ],
    httpc:request(
        post,
        {mailgun_api_url(Context) ++ z_convert:to_list(Url), Hs, "application/x-www-form-urlencoded", Payload},
        httpc_http_options(),
        httpc_options()).


mailgun_api_url(Context) ->
    URL = case z_convert:to_binary( m_config:get_value(mod_mailgun, api_url, Context) ) of
        <<>> ->
            lists:flatten([
                ?MAILGUN_API_URL,
                z_convert:to_list( m_config:get_value(mod_mailgun, domain, Context) )
            ]);
        Api ->
            Api
    end,
    z_convert:to_list(URL).


httpc_http_options() ->
    [
        {autoredirect, true},
        {relaxed, true},
        {timeout, 10000},
        {connect_timeout, 2000}
    ].

httpc_options() ->
    [
        {sync, true},
        {body_format, binary}
    ].


