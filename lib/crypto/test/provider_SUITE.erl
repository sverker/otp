%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2022. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%
%%

-module(provider_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
         init_per_group/2, end_per_group/2,

         %% Test cases:
         sign_verify_rsa/1,
         sign_verify_dsa/1,
         sign_verify_ecdsa/1,
         sign_verify_eddsa/1,
         sign_verify_rsa_pwd/1,
         sign_verify_rsa_pwd_bad_pwd/1,
         priv_encrypt_pub_decrypt_rsa/1,
         priv_encrypt_pub_decrypt_rsa_pwd/1,
         pub_encrypt_priv_decrypt_rsa/1,
         pub_encrypt_priv_decrypt_rsa_pwd/1,

         end_of_list/1]).



suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds, 10}}
    ].

all() ->
    [
     {group, provider_stored_key}
    ].

groups() ->
    [{provider_stored_key, [],
      [
       sign_verify_rsa,
       sign_verify_dsa,
       sign_verify_ecdsa,
       sign_verify_eddsa,
       sign_verify_rsa_pwd,
       sign_verify_rsa_pwd_bad_pwd,
       priv_encrypt_pub_decrypt_rsa,
       priv_encrypt_pub_decrypt_rsa_pwd,
       pub_encrypt_priv_decrypt_rsa,
       pub_encrypt_priv_decrypt_rsa_pwd,
       %% get_pub_from_priv_key_rsa,
       %% get_pub_from_priv_key_rsa_pwd,
       %% get_pub_from_priv_key_rsa_pwd_no_pwd,
       %% get_pub_from_priv_key_rsa_pwd_bad_pwd,
       %% get_pub_from_priv_key_dsa,
       %% get_pub_from_priv_key_ecdsa,

       end_of_list]}
     %%,{engine_fakes_rsa, [],
     %%  [
     %%   sign_verify_rsa_fake
     %%  ]}
    ].


init_per_group(provider_stored_key, Config) ->
    group_load_provider(Config,  [provider_method_rsa]).

group_load_provider(Config, _ExcludeMthds) ->
    case load_storage_provider(Config) of
        {loaded,P} when is_reference(P) ->
	    %%ok = crypto:engine_register(E, crypto:engine_get_all_methods() -- ExcludeMthds),
            KeyDir = key_dir(Config),
            ct:log("storage provider ~p loaded.~nKeyDir = ~p", [P,KeyDir]),
            [{storage_provider,P}, {storage_dir,KeyDir} | Config];
        {already_loaded,_} ->
            Config;
        {error, notexist} ->
            {skip, "OTP Test provider not found"};
        {error, notsup} ->
            {skip, "Provider not supported on this SSL version"};
        Other ->
            ct:log("Provider load failed: ~p",[Other]),
            {fail, "Provider load failed"}
    end.

end_per_group(_, Config) ->
    case proplists:get_value(storage_provider, Config) of
        undefined ->
            ok;
        _P ->
            %%ok = crypto:provider_unload(P)
            ok
    end.


sign_verify_rsa(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_public_key.pem")},
    sign_verify(rsa, sha, Priv, Pub).


sign_verify_dsa(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"DSA">>,
             key_id => key_id(Config, "dsa_private_key.pem")},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"DSA">>,
             key_id => key_id(Config, "dsa_public_key.pem")},
    sign_verify(dss, sha, Priv, Pub).

sign_verify_ecdsa(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"DSA">>,
             key_id => key_id(Config, "ecdsa_private_key.pem")},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"DSA">>,
             key_id => key_id(Config, "ecdsa_public_key.pem")},
    sign_verify(ecdsa, sha, Priv, Pub).

sign_verify_eddsa(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"DSA">>,
             key_id => key_id(Config, "eddsa_private_key.pem")},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"DSA">>,
             key_id => key_id(Config, "eddsa_public_key.pem")},
    sign_verify(eddsa, sha, Priv, Pub).

sign_verify_rsa_pwd(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    sign_verify(rsa, sha, Priv, Pub).

sign_verify_rsa_pwd_bad_pwd(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "Bad password"},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    try sign_verify(rsa, sha, Priv, Pub) of
        _ -> {fail, "PWD prot pubkey sign succeeded with no pwd!"}
    catch
        error:{badarg,_,_} -> ok
    end.

priv_encrypt_pub_decrypt_rsa(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_public_key.pem")},
    priv_enc_pub_dec(rsa, Priv, Pub,  rsa_pkcs1_padding).

priv_encrypt_pub_decrypt_rsa_pwd(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    priv_enc_pub_dec(rsa, Priv, Pub,  rsa_pkcs1_padding).

pub_encrypt_priv_decrypt_rsa(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_public_key.pem")},
    pub_enc_priv_dec(rsa, Pub, Priv,  rsa_pkcs1_padding).

pub_encrypt_priv_decrypt_rsa_pwd(Config) ->
    Priv = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{provider => provider_ref(Config),
             key_type => <<"RSA">>,
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    pub_enc_priv_dec(rsa, Pub, Priv,  rsa_pkcs1_padding).


pubkey_alg_supported(Alg) ->
    lists:member(Alg,
                 proplists:get_value(public_keys, crypto:supports())).

pub_enc_priv_dec(Alg, KeyEnc, KeyDec, Padding) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            CryptoText = crypto:public_encrypt(Alg, PlainText, KeyEnc, Padding),
            case crypto:private_decrypt(Alg, CryptoText, KeyDec, Padding) of
                PlainText -> ok;
                _ -> {fail, "Encrypt-decrypt error"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.

priv_enc_pub_dec(Alg, KeyEnc, KeyDec, Padding) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            CryptoText = crypto:private_encrypt(Alg, PlainText, KeyEnc, Padding),
            case crypto:public_decrypt(Alg, CryptoText, KeyDec, Padding) of
                PlainText -> ok;
                _ -> {fail, "Encrypt-decrypt error"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.

sign_verify(Alg, Sha, KeySign, KeyVerify) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            Signature = crypto:sign(Alg, Sha, PlainText, KeySign),
            case is_fake(Signature) of
                true ->
                    ct:pal("SIG ~p ~p size ~p~n~p",[Alg,Sha,size(Signature),Signature]),
                    {fail, "Faked RSA impl used!!"};
                false ->
                    case crypto:verify(Alg, Sha, PlainText, Signature, KeyVerify) of
                        true -> ok;
                        _ -> {fail, "Sign-verify error"}
                    end
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.


provider_ref(_Config) ->
    <<"provider=otp_test_provider">>.

load_storage_provider(_Config) ->
    case crypto:get_test_provider() of
        {ok, ProviderLibPath} ->
            crypto:provider_load(ProviderLibPath);

        {error, Error} ->
            {error, Error}
    end.

key_dir(Config) ->
    DataDir = unicode:characters_to_binary(proplists:get_value(data_dir, Config)),
    filename:join(DataDir, "pkcs8").

key_id(Config, File) ->
    filename:join(proplists:get_value(storage_dir,Config), File).


is_fake(Sig) -> is_fake(Sig, 0).

is_fake(<<>>, _) -> true;
is_fake(<<B,Rest/binary>>, B) -> is_fake(Rest, B+1);
is_fake(_, _) -> false.

%% Dummy placeholder test to be put last in lists
%% to avoid annoying missing/unexpected comma problems.
end_of_list(_Config) ->
    ok.
