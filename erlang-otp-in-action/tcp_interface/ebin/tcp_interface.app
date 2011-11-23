{application, tcp_interface, 
 [{description, "A simple tcp interface"}, 
  {vsn, "0.1.0"}, 
  {modules, [ti_app, ti_server, ti_sup]}, 
  {registered, [ti_sup]}, 
  {applications, [kernel, stdlib]}, 
  {mod, {ti_app, []}}
 ]}.
