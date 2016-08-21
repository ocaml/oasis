/********************************************************************************/
/*  OASIS: architecture for building OCaml libraries and applications           */
/*                                                                              */
/*  Copyright (C) 2011-2016, Sylvain Le Gall                                    */
/*  Copyright (C) 2008-2011, OCamlCore SARL                                     */
/*                                                                              */
/*  This library is free software; you can redistribute it and/or modify it     */
/*  under the terms of the GNU Lesser General Public License as published by    */
/*  the Free Software Foundation; either version 2.1 of the License, or (at     */
/*  your option) any later version, with the OCaml static compilation           */
/*  exception.                                                                  */
/*                                                                              */
/*  This library is distributed in the hope that it will be useful, but         */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  */
/*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          */
/*  details.                                                                    */
/*                                                                              */
/*  You should have received a copy of the GNU Lesser General Public License    */
/*  along with this library; if not, write to the Free Software Foundation,     */
/*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               */
/********************************************************************************/

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <int> main
%%
main:
expr EOL                { $1 }
;
expr:
INT                     { $1 }
| LPAREN expr RPAREN      { $2 }
| expr PLUS expr          { $1 + $3 }
| expr MINUS expr         { $1 - $3 }
| expr TIMES expr         { $1 * $3 }
| expr DIV expr           { $1 / $3 }
| MINUS expr %prec UMINUS { - $2 }
;
