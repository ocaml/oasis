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

%{

  open OASISVersion_types

%}

%token <string> VER
%token GT GE EQ LT LE OR AND
%token LPAREN RPAREN
%token EOF
%left OR   /* lowest precedence */
%left AND  /* highest precedence */

%start main
%type <OASISVersion_types.t> main
%%
main:
  cmp EOF { $1 }
;

cmp:
| LPAREN cmp RPAREN     { $2 }
| cmp AND cmp           { VCAnd ($1, $3) }
| cmp OR cmp            { VCOr ($1, $3) }
| GT VER                { VCGt $2 }
| GE VER                { VCGe $2 }
| EQ VER                { VCEq $2 }
| LT VER                { VCLt $2 }
| LE VER                { VCLe $2 }
;
