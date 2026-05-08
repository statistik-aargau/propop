# Create flowchart on subregional migration rate method

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


# Specify filepath
filepath <- paste0("./man/figures/rate_method.png")

# Create flowchart
graph <- DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = \"filled,rounded\", fillcolor = deepskyblue, fontname = Helvetica]

data1 [label = 'Aggregated\npast migration', shape = folder, fillcolor = gold]
# data2 [label = 'Dataset 2', shape = folder, fillcolor = gold]

calc_emi [label =  'calculate\n_emi_\nmean']

data2 [label = 'emi_rate\n= average\n emigration\n per group', shape = folder, fillcolor = gold]

calc_hist [label =  'calculate\n_hist_\nshares']

data3 [label = 'imm_sub\n= immigration\n share\nper group', shape = folder, fillcolor = gold]

propop [label =  'propop']


result [label= 'Population\nprojection\nresult', shape = folder, fillcolor = gold]

# edge definitions with the node IDs
# {data1 data2}  -> process -> statistical -> result

{data1}  -> calc_emi -> data2 -> propop
{data1}  ->calc_hist -> data3 -> propop -> result

}", height = 200, width = 800)


graph

# Export the graph as SVG, convert to PNG, and save it
svg_code <- DiagrammeRsvg::export_svg(graph)

# Convert SVG to PNG and save
rsvg::rsvg_png(charToRaw(svg_code), file = filepath)
