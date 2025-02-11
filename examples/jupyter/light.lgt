% The Jupytext "light" format for scripts allows us to write goals in a
% Logtalk source file and open it as a notebook. Comments are interpreted as
% Markdown cells as long as separated by blank lines. Goals are interpreted
% as code cells. If there is no kernel spec in the file, you need to select
% the Logtalk kernel after opening the file as a notebook. This example is
% based on the "defaulty" example.

% Start by loading the example and the `ports_profiler` tool:

logtalk_load(defaulty(loader)).

% Activate the ports profiler:

ports_profiler::start.

% Get ports profiling data for the defaulty representation:

defaulty::count_atomics([a,1,_,b,2,_,c,3,_], As, Ns).

% Print the profiling data:

ports_profiler::data.

% Reset the profiling data for the next query:

ports_profiler::reset.

% Get ports profiling data for the tagged representation:

tagged::count_atomics([a(a),n(1),o(_),a(b),n(2),o(_),a(c),n(3),o(_)], As, Ns).

% Print the profiling data:

ports_profiler::data.

% Deactivate the ports profiler:

ports_profiler::stop.
