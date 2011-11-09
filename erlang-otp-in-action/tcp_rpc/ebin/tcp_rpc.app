%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, tcp_rpc,
  [{decription, "RPC Server from Erlang and OTP in action"}, 
   {vsn, "0.1.0"}, 
   {modules, [tr_app, tr_sup, tr_server]}, 
   {registered, [tr_sup]}, 
   {applications, [kernel, stdlib]},
   {mod, {tr_app, []}}
]}.
