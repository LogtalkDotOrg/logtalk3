% ---
% jupyter:
%   jupytext:
%     cell_metadata_filter: -all
%     custom_cell_magics: kql
%     text_representation:
%       extension: .lgt
%       format_name: percent
%       format_version: '1.3'
%       jupytext_version: 1.16.7
%   kernelspec:
%     display_name: Logtalk
%     language: logtalk
%     name: logtalk_kernel
% ---

% %% [markdown]
% The Jupytext "percent" format for scripts allows us to write goals in a
% Logtalk source file and open it as a notebook. Markdown cells are marked
% by writing `% %% [markdown]` followed by one or more commented lines for
% the cell contents. Code cells as are marked by writing `% %%` followed
% in the next line by the cell goal. If there is no kernel spec in the file,
% you need to select the Logtalk kernel after opening the file as a notebook.
% This example is based on the "defaulty" example.

% %% [markdown]
% Start by loading the example and the `ports_profiler` tool:

% %%
logtalk_load(defaulty(loader)).

% %% [markdown]
% Activate the ports profiler:

% %%
ports_profiler::start.

% %% [markdown]
% Get ports profiling data for the defaulty representation:

% %%
defaulty::count_atomics([a,1,_,b,2,_,c,3,_], As, Ns).

% %% [markdown]
% Print the profiling data:

% %%
ports_profiler::data.

% %% [markdown]
% Reset the profiling data for the next query:

% %%
ports_profiler::reset.

% %% [markdown]
% Get ports profiling data for the tagged representation:

% %%
tagged::count_atomics([a(a),n(1),o(_),a(b),n(2),o(_),a(c),n(3),o(_)], As, Ns).

% %% [markdown]
% Print the profiling data:

% %%
ports_profiler::data.

% %% [markdown]
% Deactivate the ports profiler:

% %%
ports_profiler::stop.
