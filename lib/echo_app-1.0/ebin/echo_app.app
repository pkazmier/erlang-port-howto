{application, echo_app,
 [{description, "Echo Port Server"},
  {vsn, "1.0"},
  {modules, [echo_app, echo_sup, echo]},
  {registered, [echo]},
  {applications, [kernel, stdlib]},
  {mod, {echo_app, []}},
  {env, [{extprog, "echo.py"}, {timeout, 3000}, {maxline, 100}]}
 ]}.


