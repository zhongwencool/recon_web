{application, recon_web,
 [
  {description, "a web tool to look into VM using recon and cowboy"},
  {vsn, "1.0.0"},
  {registered, [recon_web_sup]},
  {applications, [
                  kernel,
                  stdlib,
                  lager,
                  recon,
                  jsx,
                  cowboy
                 ]},
  {modules,[]},
  {mod, {recon_web_app, []}},
  {env, [
    %%{ip,{"10,142,35,165"}},%% default :localhost,better using nginx to proxypass
%% please keep no space before {ip,'127.0.0.1'} and {port,8080} :
%% your should use line command  `make config IP+=127.0.0.1 PORT+=8080`
%%{ip, "127.0.0.1"},
{port, 8080}
  ]}
 ]}.
