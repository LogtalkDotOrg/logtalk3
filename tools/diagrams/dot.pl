:- module(dot,[
      dotrun/4
	,	graph_dot/2
	]).

/** <module> Graphviz language 
	
   Produces .dot language graphs from relational and functional schemata.

   Graph strucure is as follows:
   ==
   digraph  ---> digraph(Name:term, G:list(element)).
   subgraph ---> subgraph(Name:term, G:list(element)).
   element  ---> subgraph
               ; option
               ; node_opts(list(option))
               ; edge_opts(list(option))
               ; with_opts(element, list(option))
               ; arrow(term,term)   % directed edge
               ; line(term,term)    % undirected edge
               ; node(term).
   option   ---> opt_name=opt_value.
   opt_name  == atom
   opt_value == phrase
   ==
   Graph, node and edge labels can be terms and are written using write/1 for
   writing in the dot file.

   ---
   Samer Abdallah
   Centre for Digital Music, Queen Mary, University of London, 2007
   Department of Computer Science, UCL, 2014
 */

:- use_module(library(fileutils)).
:- use_module(dcgu).


digraph(Name,G) -->
	"digraph ", wr(Name), cr,
	dotblock([ overlap=at(false)
            , spline=at(true)
            , contentrate=at(true)
            | G]).

subgraph(Name,G) --> "subgraph ", wr(Name), cr, dotblock(G).

dotblock(L) --> brace(( cr, dotlist(L), cr)), cr.
dotline(L) --> "\t", L, ";\n".
dotlist([]) --> "".
dotlist([L|LS]) -->
	if(L=dotblock(B),
		dotblock(B),
		dotline(L)),
	dotlist(LS).


with_opts(A,Opts) --> phrase(A), sp, sqbr(optlist(Opts)).
optlist(L) --> seq(L,",").

node_opts(Opts) --> with_opts(at(node), Opts).
edge_opts(Opts) --> with_opts(at(edge), Opts).
nq(A)   --> wr(A).
node(A) --> qq(wr(A)).
arrow(A,B) --> node(A), " -> ", node(B).
line(A,B)  --> node(A), " -- ", node(B).
(A=B) --> at(A), "=", B.
	

dot_method(M,M) :- member(M,[dot,neato,sfdp,fdp,circo,twopi]).
dot_method(unflatten,M) :- dot_method(unflatten([]),M).
dot_method(unflatten(Opts),M) :-
   phrase(("unflatten",seqmap(uopt,Opts)," | dot"),Codes,[]),
   atom_codes(M,Codes).

uopt(l(N)) --> " -l", wr(N).
uopt(fl(N)) --> " -f -l", wr(N).
uopt(c(N)) --> " -c", wr(N).

%% dotrun( +Method:graphviz_method, +Fmt:atom, G:digraph, +File:atom) is det.
%
%  Method determines which GraphViz programs are used to render the graph:
%  ==
%  graphviz_method ---> dot ; neato; fdp ; sfdp ; circo ; twopi
%                     ; unflatten
%                     ; unflatten(list(unflatten_opt)).
%  unflatten_opt   ---> l(N:natural)   % -l<N> 
%                     ; fl(N:natural)  % -f -l<N>
%                     ; c(natural).    % -c<N> 
%  ==
%  The unflatten method attempts to alleviate the problem of very wide graphs,
%  and implies that dot is used to render the graph. The default option list is empty.
%
%  Fmt can be any format supported by Graphviz under the -T option, including
%  ps, eps, pdf, svg, png.
%
%  See man page for unflatten for more information.
%  TODO: Could add more options for dot.
dotrun(Meth1,Fmt,Graph,File) :-
   dot_method(Meth1,Meth),
   member(Fmt,[ps,eps,pdf]),
   format(atom(Cmd),'~w -T~w > "~w.~w"',[Meth,Fmt,File,Fmt]), 
   format('Running: ~w ...\n',Cmd),
	with_output_to_file(pipe(Cmd),writedcg(Graph)).

%% graph_dot( +G:digraph, +File:atom) is det.
graph_dot(Graph,File) :-
	with_output_to_file(File,writedcg(Graph)).

%%% Options

% Graph options
dotopt(graph,[size,page,ratio,margin,nodesep,ranksep,ordering,rankdir,
	pagedir,rank,rotate,center,nslimit,mclimit,layers,color,href,splines,
	start,epsilon,root,overlap, mindist,'K',maxiter]).


% Node options
dotopt(node, [label,fontsize,fontname,shape,color,fillcolor,fontcolor,style,
	layer,regular,peripheries,sides,orientation,distortion,skew,href,target,
	tooltip,root,pin]).

% Edge options
dotopt(edge, [minlen,weight,label,fontsize,fontname,fontcolor,style,color,
		dir,tailclip,headclip,href,target,tooltip,arrowhead,arrowtail,
		headlabel,taillabel,labeldistance,port_label_distance,decorate,
		samehead,sametail,constraint,layer,w,len]).


% Node options values
dotopt(node, label, A) :- ground(A).
dotopt(node, fontsize, N) :- between(1,256,N). % arbitrary maximum!
dotopt(node, fontname, A) :- ground(A).
dotopt(node, shape,
	[	plaintext,ellipse,box,circle,egg,triangle,diamond,
		trapezium,parallelogram,house,hexagon,octagon]).
dotopt(node, style, [filled,solid,dashed,dotted,bold,invis]).


% Edge options values
dotopt(edge, fontsize, N) :- between(1,256,N). % arbitrary maximum!
dotopt(edge, label, A) :- ground(A).
dotopt(node, fontname, A) :- ground(A).
dotopt(node, style, [solid,dashed,dotted,bold,invis]).
