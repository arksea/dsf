cd ..\reg_server
call ..\rebar.cmd -C ..\rebar.config compile
set HOME=.
dialyzer --plt ..\build\.dialyzer_plt --src src -I include -I ..\server_base\include
cd ..\build

