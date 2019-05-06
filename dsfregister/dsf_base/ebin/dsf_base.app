{application,dsf_base,
             [{description,"DSF BASE Modules"},
              {vsn,"1.1"},
              {registered,[]},
              {applications,[kernel,stdlib,sasl,jsx,thrift,mnesia]},
              {mod,{dsf_base_app,[]}},
              {env,[]},
              {modules,[alarmView_thrift,alarm_types,alertor,alertor_thrift,
                        ark_alarm_handler,dsf_base_app,dsf_base_sup]}]}.
