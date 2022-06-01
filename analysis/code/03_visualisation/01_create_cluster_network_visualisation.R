### create cluster network visualisation ###

# We rely on graphviz being installed in the operating system. We use the sfdp
# filter (for drawing large undirected graphs) from this library to achieve the
# best possible placement in the visualization.
#
# For the purpose of visualization, the 'typical' vessels or their drawings are
# used as icons and for this purpose they are converted into pngs of max.
# 100x100 pixels using ImageMagick ( command convert).
#
# In the next step these images and the created directory structure serve to
# visualize the clustering (function make_graphviz).

system("
       #!/bin/bash

       for i in `find ./ -name 'node.png'` ; do

       convert \"$i\" -resize 100x100 \"${i}.out.png\"

       done
       ")
make_graphviz(output_directory)
