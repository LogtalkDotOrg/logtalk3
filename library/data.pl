/*
	graph(Directed, Type, Label, Metadata).
	node(Id, Type, Label, Metadata).
	edge(Source, Relation, Target, Directed, Label, Metadata).
*/

% single graph

graph(false, 'graph type', 'graph label', ['user-defined'-'values']).

node('0', 'node type', 'node label(0)', ['user-defined'-'values']).
node('1', 'node type', 'node label(1)', ['user-defined'-'values']).

edge('0', 'edge relationship', '1', false, 'edge label', ['user-defined'-'values']).

% multiple graphs

graph('0', false, 'graph type', 'graph label', ['user-defined'-'values']).
graph('1', false, 'graph type', 'graph label', ['user-defined'-'values']).

node('0', '0', 'node type', 'node label(0)', ['user-defined'-'values']).
node('0', '1', 'node type', 'node label(1)', ['user-defined'-'values']).
node('1', '0', 'node type', 'node label(0)', ['user-defined'-'values']).
node('1', '1', 'node type', 'node label(1)', ['user-defined'-'values']).

edge('0', '0', 'edge relationship', '1', false, 'edge label', ['user-defined'-'values']).
edge('1', '0', 'edge relationship', '1', false, 'edge label', ['user-defined'-'values']).

/*
{
    "graph": {
        "directed": false,
        "type": "graph type",
        "label": "graph label",
        "metadata": {
            "user-defined": "values"
        },
        "nodes": [
            {
                "id": "0",
                "type": "node type",
                "label": "node label(0)",
                "metadata": {
                    "user-defined": "values"
                }
            },
            {
                "id": "1",
                "type": "node type",
                "label": "node label(1)",
                "metadata": {
                    "user-defined": "values"
                }
            }
        ],
        "edges": [
            {
                "source": "0",
                "relation": "edge relationship",
                "target": "1",
                "directed": false,
                "label": "edge label",
                "metadata": {
                    "user-defined": "values"
                }
            }
        ]
    }
}
*/

