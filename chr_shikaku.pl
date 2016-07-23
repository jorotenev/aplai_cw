
:- use_module(library(chr)).
:- use_module(library(lists)).
:- chr_option(optimize,full).

:- chr_type pos ---> row-col.
:- chr_type row == int.
:- chr_type col == int.

:- chr_constraint 	maybe(+pos, +),
					bucket(+pos, +),
					temp(+pos,+int).



