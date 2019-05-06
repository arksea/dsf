set EH=D:/erlang/R16B01
set HOME=.
dialyzer --build_plt -r %EH%/lib/stdlib-1.19.2/ebin %EH%/lib/kernel-2.16.2/ebin %EH%/lib/mnesia-4.9/ebin %EH%/lib/observer-1.3.1/ebin %EH%/lib/debugger-3.2.11/ebin %EH%/lib/runtime_tools-1.8.11/ebin %EH%/lib/et-1.4.4.4/ebin %EH%/lib/sasl-2.3.2/ebin %EH%/lib/erts-5.10.2/ebin

