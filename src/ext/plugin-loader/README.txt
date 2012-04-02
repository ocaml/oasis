
This project helps to load plugins that has dependencies. It uses two methods
to detect plugins:
* using findlib, if the META file contains the right keyword
* in a directory a .cma/.cmo/.cmxs file + its .plugin counterpart.

Just using text files (META and .plugin)
