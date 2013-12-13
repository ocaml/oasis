/********************************************************************************/
/*  OASIS: architecture for building OCaml libraries and applications           */
/*                                                                              */
/*  Copyright (C) 2011-2013, Sylvain Le Gall                                    */
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
open OASISLicense_types
%}

%token <string> TOKEN
%token COMMA
%token AND
%token OR
%token OR
%token WITH
%token EXCEPTION
%token EOF
%left COMMA
%left OR
%left AND

%start main
%type <OASISLicense_types.t> main
%%

main:
  licenses EOF { $1 }
;

licenses:
| licenses AND licenses   { And ($1, $3) }
| licenses OR  licenses   { Or  ($1, $3) }
| licenses COMMA AND licenses { And ($1, $4) }
| licenses COMMA OR licenses  { Or($1, $4) }
| TOKEN WITH tokens EXCEPTION { let excpt = String.concat " " (List.rev $3) in License($1, Some excpt) }
| TOKEN { License($1, None) }
;

tokens:
| tokens TOKEN { $2 :: $1 }
| TOKEN        { [$1] }
;
