:- module(callgraph, 
	[	module_dotpdf/2
   ,  modules_dotpdf/3
   ,  module_dot/2
   ,  modules_dot/3
	]).

/** <module> Visualisation of inter-predicate call graphs

   ---++ Usage
   This module allows you to produce a call graph of one or more modules, where
   nodes (boxes) represent predicates and an edge between
   two predicates indicates that one (the source end) calls the other
   (the pointy end).

   By default, node styles are used to indicate whether or not the predicate is
   exported (bold outline), dynamic (italic label, filled), multifile (box with diagonals
   in the corners). 
   For dynamic predicates, dashed edges are used to represent operations which
   mutate (assert to or retract from) the predicate.

   Items in the recorded database are also represented, as they consitute mutable
   state much like dynamic predicates. Recorded items are labelled as Key:Functor/Arity
   and drawn in a filled octagonal box. Dashed edges represents writing to the
   recorded database and ordinary solid edges represent reading.

   Basic method of usage is:
      1. Load the module you want to graph.
      2. Load this module.
      3. Generate and write the module call graph to a PDF document:
         ==
         ?- module_dotpdf(ModuleName,[]).
         ==

   See module_dot/2 for options that affect graph content and style, and
   module_dotpdf/2 for options that affect rendering.

   ---++ Multi-module graphs

   The predicates modules_dotpdf/3 and modules_dot/3 support graphing multiple
   modules. Each module is drawn inside a box (a Graphviz cluster). Items in
   the recorded database are outside the module system. They can optionally
   be collected into one cluster or into several clusters by key.

   ---++ Implementation notes

   NB. This is very preliminary. The intereface is not necessarily stable.
   The dot DCG implementation is a bit embarassing but it does the job for now.

   Three parts to this:
   1. Using prolog_walk_code/1 to compile a database of inter-predicate calls
      into dynamic predicate edge/3.
   2. Possible transformations of graph (eg pruning).
   3. Collect information about desired modules/predicates into a Dot graph
      structure as defined by dotdct.pl

   ---+++ Types used internally
   
   *  pred_head
      Predicate specifier as Modue:Head where Head is a term with same 
      name and arity of the predicate concerned.
   *  module
      Module name as an atom.
   *  record_head
      Term like Key:Term, reference to recorded database terms with
      Term and given Key.
   *  node
      Node name as Atom:Name/Arity.
   ==
   module == atom.
   functor   ---> atom/natural.
   pred_head ---> module:term.
   node      ---> atom:functor.
   record_head ---> atom:term.
   edge_spec ---> dynamic(module,pred_head,list(edge_type))
                ; recorded(edge_type,record_head).
   edge_type ---> calls ; mutates ; reads ; writes.
   ==
*/

:- predicate_options(module_dot/2,2,[pass_to(module_graph/3,2)]).
:- predicate_options(module_dotpdf/2,2,[method(any),pass_to(module_graph/3,2)]).
:- predicate_options(modules_dot/3,2,[pass_to(modules_graph/4,2)]).
:- predicate_options(modules_dotpdf/3,2,[method(any),pass_to(modules_graph/4,2)]).

:- predicate_options(module_graph/3,2,
      [  prune(boolean)
      ,  hide_list(list)
      ,  recursive(boolean)
      ,  arrowhead(atom)
      ,  font(list(integer))
      ,  pass_to(predopt//2,1)
      ,  pass_to(edgeopt//2,1)
      ]).

:- predicate_options(modules_graph/4,2,
      [  cluster_recorded(oneof([false,by_key,true]))
      ,  prune(boolean)
      ,  hide_list(list)
      ,  recursive(boolean)
      ,  arrowhead(atom)
      ,  font(list(integer))
      ,  pass_to(predopt//2,1)
      ,  pass_to(edgeopt//2,1)
      ]).

:- predicate_options(predopt//2,1,
      [  dynamic_style(atom)
      ,  dynamic_shape(atom)
      ,  export_style(atom)
      ,  multifile_style(atom)
      ,  multifile_shape(atom)
      ,  recorded_style(atom)
      ,  recorded_shape(atom)
      ,  font(list(integer))
      ]).

:- predicate_options(edgeopt//2,1,
      [  mutate_style(atom)
      ,  read_style(atom)
      ,  write_style(atom)
      ]).


:- use_module('library/dcgu').
:- use_module('library/dot').

% ------------ Building the call graph in the Prolog database -----------------------

:- dynamic edge/3.
%% edge( -T:edge_type, -Source:node, -Target:node) is nondet.
%  Dynamic predicate for storing edges discoverd by code analysis.

retract_graph :- retractall(edge(_,_,_)).

%% assert_graph(+Modules:list(atom)) is det.
%  Analyses modules (using prolog_walk_code/1) asserting information about the
%  call graph to a set of private dynamic predicates. 
assert_graph(Mods) :-
   retract_graph,
   sort(Mods,Mods1), % to get rid of duplicates
   forall( member(Mod,Mods1),
      prolog_walk_code([ trace_reference(_), module(Mod), on_trace(trace_call(Mods1)), source(false) ])
   ),
	predicate_property(edge(_,_,_), number_of_clauses(N)),
	format('Got ~D edges.~n', [N]).


%% trace_call(+Mods:list(module), +Goal:pred_head, +Caller:pred_head, +Context) is det.
%  Callback predicate for prolog_walk_code/1.
trace_call(Mods, M1:H1, M2:H2, _) :-
   debug(callgraph,'Considering ~w <--- ~w.',[M1:H1,M2:H2]),
   %memberchk(M1,Mods), memberchk(M2,Mods), 
   head_node(M2:H2,Caller),
   (classify(M1:H1,Class) -> true; Class=normal(M1:H1)),
   assert_edges(Class,Caller,Mods).
trace_call(_,Goal,Caller,_) :-
   debug(callgraph,'Ignoring ~q ---> ~q.',[Caller,Goal]).


%% classify( +G:pred_head, -E:edge_spec) is semidet.
%  Goal Classification predicate. 
classify(M:assert(C),       dynamic(M,C,[mutates])).
classify(M:assertz(C),      dynamic(M,C,[mutates])).
classify(M:asserta(C),      dynamic(M,C,[mutates])).
classify(M:retractall(C),   dynamic(M,C,[mutates])).
classify(M:retract(C),      dynamic(M,C,[mutates,calls])).
classify(_:recorded(K,T,_), recorded(reads,K:T)).
classify(_:recorda(K,T,_),  recorded(writes,K:T)).
classify(_:recorda(K,T),    recorded(writes,K:T)).
classify(_:recordz(K,T,_),  recorded(writes,K:T)).
classify(_:recordz(K,T),    recorded(writes,K:T)).

%% assert_edges(+E:edge_spec, +Caller:node, +Mods:list(module)) is det.
%  Assert relevant edges (if any) for call from Caller to target specified by E.
assert_edges(normal(Goal), Caller, Ms)    :- goal_pred_head(Goal,H), assert_types([calls],H,Caller,Ms).
assert_edges(recorded(T,Spec),Caller,_)   :- rec_spec_node(Spec,N), assert_edge(T,Caller,N).
assert_edges(dynamic(M,C,Ts), Caller, Ms) :- 
   nonvar(C), mod_clause_head(M,C,H), 
   assert_types(Ts,H,Caller,Ms).
assert_edges(_,_,_). % catch all if other clauses fail

rec_spec_node(Key:Term,Node) :-
   (  var(Key) -> K='unknown'; K=Key),
   (  var(Term) -> Node=K:unknown; head_node(K:Term,Node)).

%% mod_clause_head( +M:module, +C:clause, -H:pred_head) is det.
%  Combines a clause (as supplied to assert/retract) with its source
%  module to get the head (including module) of the dynamic predicate being
%  modified. Basically, it allows any module specified in C to to 
%  override M.
mod_clause_head(_, (M:H:-_), M:H) :- !.
mod_clause_head(_, (M:H), M:H) :- !.
mod_clause_head(M, (H:-_), M:H) :- !.
mod_clause_head(M, H, M:H).

%% assert_types(+Ts:list(edge_type), +Target:pred_head, +Caller:node, +Mods:list(module)) is semidet.
%  Assert edges of types in Ts from Caller to Target, but only if the 
%  home module of Target is a member of Mods.
assert_types(Types,M3:H,Caller,_Mods) :-
   %memberchk(M3,Mods),
   head_node(M3:H,Node),
   forall(member(T,Types), assert_edge(T,Caller,Node)).

%% goal_pred_head( +Goal:pred_head, -Pred:pred_head) is semidet.
% matches a goal to it's ultimate, non-built in source predicate.
% Fails if predicate is built in.
goal_pred_head(M1:H,M3:H) :-
   \+predicate_property(M1:H, built_in),
   (predicate_property(M1:H, imported_from(M3)) -> true; M3=M1).

%% assert_edge(+T:edge_type, +N1:node, +N2:node) is det.
% asserts edge if not already asserted.
assert_edge(T,N1,N2) :- 
   (  edge(T,N1,N2) -> true
   ;  debug(callgraph,'Adding edge: ~w --> ~w.',[N1,N2]),
      assertz(edge(T,N1,N2))).

%% head_node( +H:head, -P:node) is det.
%% head_node( -H:head, +P:node) is det.
%  convert predicate or record head from head term to functor/arity.
%  ==
%  head ---> atom:term.
%  ==
head_node(M:H,M:F/A) :- must_be(nonvar,M), (nonvar(H);ground(F/A)), functor(H,F,A).



%% prune_subtrees is det.
%  Operates on the currently asserted graph (see assert_graph/1). It searches
%  for any part of the call graph which is a pure tree, and removes all the nodes below
%  the root. Thus, any 'leaf' predicate which is only ever called by one 'parent' is
%  removed. This is step is repeated until there are no more leaf predicates. The idea
%  is that the child tree can be considered 'private' to its parent.
prune_subtrees :- do_until(prune_subtrees).

prune_subtrees(false) :-
   bagof(Node, prunable(Node), Nodes), !,
   forall(member(N,Nodes), (writeln(pruning:N), retractall(edge(calls,_,N)))).
prune_subtrees(true).

prunable(Node) :-
   setof( Parent, edge(calls,Parent,Node), [_]), % node has exactly one caller
   \+edge(_,Node,_), % edges out 
   head_node(G,Node),
   \+predicate_property(G,dynamic),
   \+predicate_property(G,multifile),
   \+predicate_property(G,exported).

do_until(P) :-
   call(P,Flag),
   (  Flag=true -> true
   ;  do_until(P)
   ).


% ----------------------- GraphML output ----------------------
% Leaving this out for the time being.

% module_graphml(Mod) :-
%    assert_graph([Mod]),
%    current_ugraph(Graph),
%    retract_graph,
%    format(atom(File),'~w.graphml',[Mod]),
%    graphml_write_ugraph(File, nomap, [], Graph).

% nomap(id,node(N),A) :- term_to_atom(N,A), writeln(nomap(id,node(N),A)).
% % nomap(id,edge(_,_),'').
%               % [key(node, color, string), key(edge,color,string)],
% % cmap(color, node(_), green).
% % cmap(color, edge(_), red).


% %% current_ugraph(-Graph:graphml) is det.
% %  Returns the current call graph as a GraphML document structure.
% current_ugraph(Graph) :-
%    findall(Pred, (calls_ir(Mod:Pred,_);calls_ir(_,Mod:Pred)), Preds),
%    sort(Preds,Preds1),
%    setof(Caller-Callees, (member(Caller,Preds1), callees(Mod,Caller,Callees)), Graph).

% callees(Mod,Caller,Callees) :- setof(Callee, calls_ir(Mod:Caller,Mod:Callee),Callees), !.
% callees(_,[]).



% ----------------------- Dot output ----------------------


%% module_dot(+ModuleName,Opts) is det.
%  Writes a call graph for named module as a dot file named "[ModuleName].dot". 
%  This predicate also accepts various options (some of the these types 
%  refer to the GraphViz attribute types):
%     * prune(Prune:bool) / false
%       If true, then graph subtrees are removed using prune_subtrees/0.
%     * recursive(bool) / false
%       If true, then looping edges are used to decorate predicates that call themselves
%       directly. Otherwise, such direct recursion is hidden.
%     * hide_list(list(pred_ind))  / []
%       A list of predicate (name/arity) to hide from the graph.
%     * arrowhead(arrow_type)  / vee
%       Dot arrowhead name as an atom, for all call edges.
%     * export_style(node_style)    / bold
%       Dot node style for exported predicates.
%     * dynamic_shape(node_shape)   / box
%       Dot node shape for dynamic predicates.
%     * dynamic_style(node_style)   / filled
%       Dot node style for dynamic predicates.
%     * multifile_shape(node_shape) / box
%       Dot node shape for multifile predicates.
%     * multifile_style(node_style) / diagonals
%       Dot node style for multifile predicates.
%     * recorded_shape(node_shape)  / octagon
%       Dot node shape for recorded facts.
%     * recorded_style(node_style)  / filled
%       Dot node style for recorded facts.
%     * mutate_style(line_style)    / dashed
%       Dot line style for edges representing mutation of a dynamic predicate.
%     * read_style(line_style)      / solid
%       Dot line style for edges representing readed of a recorded fact.
%     * write_style(line_style)     / dashed
%       Dot line style for edges representing writing of a recored fact.
%     * font(S:string)               
%       Font family (as a list of codes) for all labels. How these names are
%       interpreted depends on your host operating system. On Mac OS X, I find 
%       I am able to use any font available in the "Font Book" application with
%       the name written exactly (including spaces) as in the "Font" column.
%       Default is "Times".
%
% Types for Dot attributes:
% see http://graphviz.org/Documentation.php for more details on
%  * arrow_type: http://graphviz.org/content/attrs#karrowType
%  * node_shape: http://graphviz.org/content/node-shapes 
%  * node_style: http://graphviz.org/content/attrs#kstyle
%
% ==
% line_style ---> solid ; dashed ; dotted ; bold.
% arrow_type ---> normal ; vee ; empty ; box ; none ; dot ; ... . 
% node_shape ---> box ; ellipse ; circle ; diamond ; trapezium ; parallelogram 
%               ; house ; square ; pentagon ; hexagon ; septagon ; octagon ; ... .
% node_style ---> solid ; dashed ; dotted ; bold ; rounded 
%               ; diagonals ; filled ; striped ; wedged. 
% ==
module_dot(Mod,Opts) :-
   check_options(module_dot/2,2,Opts),
   module_graph(Mod,Opts,Graph),
   format(atom(File),'~w.dot',[Mod]),
   graph_dot(Graph,File).

%% module_dotpdf(+Mod,Opts) is det.
%  Writes a call graph for module Mod as a PDF file named "[Mod].pdf". 
%  As well as the options accepted by module_dot/2, this predicate also accepts:
%   * method(Method:graphviz_method) / unflatten
%     Determines which GraphViz programs are used to render the graph. The type 
%     graphviz_method is defined as:
%     ==
%     graphviz_method ---> dot ; neato; fdp ; sfdp ; circo ; twopi
%                        ; unflatten(list(unflatten_opt))
%                        ; unflatten.
%     unflatten_opt   ---> l(N:natural)   % -l<N>
%                        ; fl(N:natural)  % -f -l<N>
%                        ; c(natural).    % -c<N>
%     ==
%     The unflatten methods filter the graph through unflatten before passing
%     on to dot.
module_dotpdf(Mod,Opts) :-
   check_options(module_dotpdf/2,2,Opts),
   module_graph(Mod,Opts,Graph),
   option(method(Method),Opts,unflatten),
   dotrun(Method,pdf,Graph,Mod).

%% modules_dot(+Modules:list(module),+Opts,+Name) is det.
%
%  Mostly like module_dot/2, but takes a list of module names instead of
%  a single module name. Output is written to a dot file named Name.
%  It understands the same options, but in addition:
%     * cluster_recorded(Flag:oneof([false,true,by_key])) / false
%       If true, then all recorded items are collected into a seperate cluster.
%       If by_key, then all recorded items are collected multiple clusters, one
%       for each distinct key..
modules_dot(Mods,Opts,Name) :-
   check_options(modules_dot/3,2,Opts),
   modules_graph(Mods,Opts,Name,Graph),
   format(atom(File),'~w.dot',[Name]),
   graph_dot(Graph,File).

%% modules_dotpdf(+Modules:list(module),+Opts,+Name) is det.
%
%  Mostly like module_dotpdf/2, but takes a list of module names instead of
%  a single module name. Output is written to a PDF file named Name.
%  It understands the same options, but in addition:
%     * cluster_recorded(Flag:oneof([false,true,by_key])) / false
%       If true, then all recorded items are collected into a seperate cluster.
%       If by_key, then all recorded items are collected multiple clusters, one
%       for each distinct key..
modules_dotpdf(Mods,Opts,Name) :-
   check_options(modules_dotpdf/3,2,Opts),
   modules_graph(Mods,Opts,Name,Graph),
   option(method(Method),Opts,unflatten),
   dotrun(Method,pdf,Graph,Name).

check_options(Pred,Arg,Opts) :- maplist(check_predicate_option(Pred,Arg),Opts).

module_graph(Mod,Opts,digraph(Mod,Statements)) :-
   assert_graph([Mod]),
   (option(prune(true),Opts) -> prune_subtrees; true),
   phrase((
         seqmap(global_opts(Opts),[graph,node,edge]),
         recorded_nodes(Opts),
         module_statements(Opts,Mod),     
         module_recorded_edges(Opts,Mod)
      ), Statements, []),
   retract_graph.

modules_graph(Mods,Opts,Name,digraph(Name,Statements)) :-
   assert_graph(Mods),
   (option(prune(true),Opts) -> prune_subtrees; true),
   option(cluster_recorded(CR),Opts,false),
   phrase((
         seqmap(global_opts(Opts),[graph,node,edge]), % global attributes
         recorded_nodes(Opts),
         seqmap(module_subgraph(Opts),Mods),
         seqmap(modules_module_edges(Opts,Mods),Mods),
         recorded_edges(CR,Opts,Mods)
      ), Statements, []),
   retract_graph.

recorded_edges(false,Opts,Mods) --> seqmap(module_recorded_edges(Opts),Mods).
recorded_edges(true,Opts,Mods) --> in_cluster(Opts,recorded, recorded, recorded_edges(false,Opts,Mods)).
recorded_edges(by_key,Opts,Mods) --> 
   {esetof(Key,recorded_key(Key),Keys)},
   seqmap(recorded_key_edges(Opts,Mods),Keys).

recorded_key_edges(Opts,Mods,Key) -->
   {atom_concat(recorded_,Key,ClusterName)},
   in_cluster(Opts,ClusterName, key(Key), seqmap(key_module_recorded_edges(Opts,Key),Mods)).

recorded_node(Node) :- edge(reads,_,Node); edge(writes,_,Node).
recorded_key(Key) :- recorded_node(Key:_).

modules_module_edges(Opts,Mods,M1) -->
   seqmap(inter_module_edges(Opts,M1),Mods).

inter_module_edges(Opts,M1,M2) -->
   ({M1@<M2} -> module_module_edges(Opts,M1,M2);[]).

module_subgraph(Opts,Mod) -->
   in_cluster(Opts,Mod, module(Mod), module_statements(Opts,Mod)).

% declare recorded nodes
recorded_nodes(Opts) -->
   {predopt(Opts,recorded,RecNodeAttr,[])},
   {esetof(with_opts(node(N),RecNodeAttr), recorded_node(N), RecNodes)}, 
   list(RecNodes).

%% module_statements(+Opts,+Mod)// is det.
%  Outputs the currently asserted graph as a dot graph structure,
%  using the given options and restricting the graph to module Mod.
%  The options are documented under module_dot/2.
module_statements(Opts,Mod) -->
   module_nodes(Opts,Mod),
   module_module_edges(Opts,Mod,Mod).

module_nodes(Opts,Mod) -->
   % declare other declarable nodes
   {esetof(with_opts(node(Pred),Attrs), node_decl(Opts,Mod,Pred,Attrs), Decls)}, 
   list(Decls).

module_recorded_edges(Opts,Mod) -->
   key_module_recorded_edges(Opts,_,Mod,reads),
   key_module_recorded_edges(Opts,_,Mod,writes).

key_module_recorded_edges(Opts,Key,Mod) -->
   key_module_recorded_edges(Opts,Key,Mod,reads),
   key_module_recorded_edges(Opts,Key,Mod,writes).

key_module_recorded_edges(Opts,Key,Mod,Type) -->
   {edgeopt(Opts,Type,RAttr,[])},
   findall(with_opts(arrow(Pred,Key:Term),RAttr), edge(Type,Mod:Pred,Key:Term)).

module_module_edges(Opts,M1,M2) -->
   {edgeopt(Opts,mutates,MAttr,[])},
   findall(with_opts(arrow(Mutator,Mutatee),MAttr), visible_mutation(Opts,M1:Mutator,M2:Mutatee)),
   findall(arrow(Caller,Callee), visible_call(Opts,M1:Caller,M2:Callee)).

node_decl(Opts,Mod,Pred,Attrs) :-
   declarable_node(Opts,Mod,Pred),
   debug(callgraph,'Declarable node: ~w.',[Mod:Pred]),
   pred_attr(Opts,Mod:Pred,Attrs).

declarable_node(Opts,M,Pred) :-
   option(hide_list(HideList),Opts,[]),
   (  predicate_property(M:Head, dynamic)
   ;  predicate_property(M:Head, exported)
   ;  predicate_property(M:Head, multifile)
   ),
   \+predicate_property(M:Head, built_in),
   \+predicate_property(M:Head, imported_from(_)),
   head_node(M:Head,M:Pred),
   \+member(Pred, ['$mode'/2,'$pldoc'/4, '$pldoc_link'/2, '$pred_option'/4]),
   \+member(Pred,HideList).

declarable_node(Opts,M,Pred) :-
   option(hide_list(HideList),Opts,[]),
   (edge(reads,M:Pred,_); edge(writes,M:Pred,_)),
   \+edge(calls,_,M:Pred),
   \+edge(calls,M:Pred,_),
   \+edge(mutates,M:Pred,_),
   \+member(Pred,HideList).

visible_call(Opts,M1:Caller,M2:Callee) :-
   option(hide_list(L),Opts,[]),
   option(recursive(T),Opts,false),
   edge(calls,M1:Caller,M2:Callee),
   (T=false -> Caller\=Callee; true),
   \+member(Caller,L),
   \+member(Callee,L).

visible_mutation(Opts,M1:P1,M2:P2) :-
   option(hide_list(L),Opts,[]),
   edge(mutates,M1:P1,M2:P2),
   \+member(P1,L),
   \+member(P2,L).



global_opts(_,graph) --> [].
global_opts(O,node) --> {font(normal,O,F)}, [node_opts([ shape=at(box), fontname=qq(F) ])].
global_opts(O,edge) --> {option(arrowhead(AH),O,vee)}, [edge_opts([ arrowhead=at(AH) ])].

predopt(O,exported) --> 
   {option(export_style(S),O,bold)}, 
   {font(bold,O,F)}, 
   [ style = qq(at(S)), fontname=qq(F) ].
predopt(O,dynamic) --> 
   {option(dynamic_shape(S),O,box)}, 
   {option(dynamic_style(St),O,filled)},
   {font(italic,O,F)}, 
   [ shape = at(S), fontname=qq(F), style = qq(at(St)) ].
predopt(O,multifile) --> 
   {option(multifile_shape(S),O,box)}, 
   {option(multifile_style(St),O,diagonals)},
   [ shape = at(S), style = qq(at(St)) ].
predopt(O,recorded) --> 
   {option(recorded_shape(S),O,octagon)}, 
   {option(recorded_style(St),O,filled)},
   [ shape = at(S), style = qq(at(St)) ].

edgeopt(O,mutates) --> {option(mutate_style(S),O,dashed)}, [ style = qq(at(S)) ].
edgeopt(O,writes) --> {option(write_style(S),O,dashed)}, [ style = qq(at(S)) ].
edgeopt(O,reads) --> {option(read_style(S),O,solid)}, [ style = qq(at(S)) ].

pred_attr(O,Pred,Attrs1) :-
   head_node(Goal,Pred),
   phrase( (  if( predicate_property(Goal,dynamic), predopt(O,dynamic)),
              if( predicate_property(Goal,multifile), predopt(O,multifile)),
              if( predicate_property(Goal,exported), predopt(O,exported))), 
           Attrs, []),
           %   Attrs = [_|_],
   compile_attrs(Attrs,[],Attrs1).

compile_attrs([],A,A).
compile_attrs([style=S|AX],AttrsSoFar,FinalAttrs) :- !,
   (  select(style=OS,AttrsSoFar,A1)
   -> combine_styles(S,OS,NS), A2=[style=NS|A1]
   ;  A2=[style=S|AttrsSoFar]
   ),
   compile_attrs(AX,A2,FinalAttrs).
compile_attrs([A|AX],A0,A2) :- compile_attrs(AX,[A|A0],A2).

combine_styles(qq(S1),qq(S2),qq((S1,",",S2))).

% compile_attrs1([],A,[]).
% compile_attrs1([A|AX],A0,[A|A1]) :- compile_attrs1(AX,[A|A0],A1).

font_family(O) --> {option(font(FF),O,"Times")}, seqmap(out,FF).
font(normal,O,F) :- phrase(font_family(O),F,[]).
font(italic,O,F) :- phrase((font_family(O)," Italic"),F,[]).
font(bold,O,F)   :- phrase((font_family(O)," Bold"),F,[]).

in_cluster(Opts,Name, Label, Phrase) -->
   {atom_concat(cluster_,Name,SubName)},
   {phrase((subgraph_opts(Opts),Phrase),Statements,[])},
   [subgraph(SubName,[label=qq(wr(Label)) | Statements])].

subgraph_opts(Opts) -->
   {font(bold,Opts,F)},
   [labeljust=qq(at(l))],
   [fontname=qq(F)]. 

% general utilities
esetof(A,B,C) :- setof(A,B,C) *-> true; C=[].

list([]) --> [].
list([X|XS]) --> [X], list(XS).
