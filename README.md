# keychain_stl_program
A python program that generates an stl file of a keychain I designed to be 3d printed.

For my rapid prototyping technologies (15-294) final project, I coded a python program that would create an stl of a complex keychain. The keychain is composed of an dodecahedron skeleton with an inner icosahedron freely resting inside. There are also toruses that compose the chain part. The python code creates a mesh of counterclockwise rectangles and triangles for each part and conjoins them together. The torus were approximated using a parametrization, while the two polyhedra were exactly calculated and some artistic choices were made for the design.
