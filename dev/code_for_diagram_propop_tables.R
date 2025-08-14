
library(DiagrammeR) # create flowchart
library(DiagrammeRsvg) # export flowchart
library(rsvg)


graph <- grViz("
digraph flowchart {
  # Define graph attributes
  graph [layout = dot, rankdir = TB]

  # Define node styles
  node [shape = box, style = filled, fillcolor = 'transparent', fontname = Arial, color = 'transparent']

  # Define nodes
  A [label = < <b> propop_tables() </b>>, style = filled, color = 'transparent', fillcolor = 'transparent']
  B [label = < Uses <i>purrr::reduce() </i> to iterate across <BR/><b> years </b> and <b>spatial units   </b>  in parameters and population <BR/>and to bind results of year t and year t + 1>, color = 'transparent']
  C [label = < <b> year t </b><BR/>start year or projected year>, fontcolor = '#007AB8']
  D [label = < <b> project_population() </b><BR/>wrapper function>, fontcolor = '#ffa81f']
  E [label = < <b> 1. advance_population() </b><BR/>population ages by one year;<BR/> aggregates population 100+>, fillcolor = 'transparent']
  F [label = < <b> 2. calculate_projection() </b><BR/>cohort component method <BR/>for population aged 1-99+>, fillcolor = 'transparent']
  G [label = < <b> 3. calculate_newborns() </b><BR/>calculates births>, fillcolor = 'transparent']
  H [label = < <b> results for year t + 1 </b>>, fontcolor = '#007AB8']

  # Align side by side
{ rank=same; A }
{ rank=same; C; D}

  # Define edges (connections)
  A -> B
  B -> C
  C -> D
  D -> E
  E -> F
  F -> G
  G -> H
  H -> C
}
")

graph

# Export the graph as SVG, convert to PNG, and save it
svg_code <- DiagrammeRsvg::export_svg(graph) # Export to SVG

# Convert SVG to PNG and save
rsvg::rsvg_png(charToRaw(svg_code), file = filepath)

