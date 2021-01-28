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
    observe_email_status/2
    ]).

% Testing
-export([
    mailgun_block/2,
    mailgun_clear/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


observe_email_status(#email_status{ is_manual = true, is_valid = false, recipient = Email }, Context) ->
    mailgun_block(Email, Context);
observe_email_status(#email_status{ is_manual = true, is_valid = true, recipient = Email }, Context) ->
    mailgun_clear(Email, Context);
observe_email_status(#email_status{}, _Context) ->
    ok.


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
    Domain = z_convert:to_binary( m_config:get_value(mod_mailgun, domain, Context) ),
    case {ApiUrl, Domain} of
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
    httpc:request(delete, {mailgun_api_url(Context) ++ z_convert:to_list(Url), Hs}, [], []);
mailgun_api(post, Url, Payload, Context) ->
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    Auth = base64:encode_to_string( "api:" ++ z_convert:to_list(ApiKey) ),
    Hs = [
        {"Authorization", "Basic " ++ Auth}
    ],
    httpc:request(post, {mailgun_api_url(Context) ++ z_convert:to_list(Url), Hs, "application/x-www-form-urlencoded", Payload}, [], []).


mailgun_api_url(Context) ->
    URL = m_config:get_value(mod_mailgun, api_url, Context),
    z_convert:to_list(URL).

