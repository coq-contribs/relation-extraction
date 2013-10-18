(****************************************************************************)
(*  RelationExtraction - Extraction of inductive relations for Coq          *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.   *)
(*                                                                          *)
(*  Copyright 2011 CNAM-ENSIIE                                              *)
(*                 Catherine Dubois <dubois@ensiie.fr>                      *)
(*                 David Delahaye <david.delahaye@cnam.fr>                  *)
(*                 Pierre-Nicolas Tollitte <tollitte@ensiie.fr>             *)
(****************************************************************************)


(*i camlp4deps: "parsing/grammar.cma" i*)

open Genarg
open Pp
open Relation_extraction

let pr_mode _ _ _ (id, mode) =
  str "" ++ spc () ++ str "[" ++
     (List.fold_left (fun b e -> b ++ spc () ++ int e) (mt ()) mode) ++
     spc () ++ str "]"

ARGUMENT EXTEND mode
  TYPED AS reference * (int list)
  PRINTED BY pr_mode
  | [ global(id) "[" integer_list(mde) "]" ] -> [ (id, mde) ]
END


VERNAC COMMAND EXTEND ExtractionRelation CLASSIFIED AS SIDEFF
| [ "Extraction" "Relation" mode(mde) ] ->
  [ msg_info (relation_extraction (fst mde) [ mde ]) ]
| [ "Extraction" "Relation" mode(mde) "with" mode_list(modes) ] ->
  [ msg_info (relation_extraction (fst mde) (mde :: modes)) ]
END

VERNAC COMMAND EXTEND ExtractionRelationRelaxed CLASSIFIED AS SIDEFF
| [ "Extraction" "Relation" "Relaxed" mode(mde) ] ->
  [ msg_info (relation_extraction_order (fst mde) [ mde ]) ]
| [ "Extraction" "Relation" "Relaxed" mode(mde) "with" mode_list(modes) ] ->
  [ msg_info (relation_extraction_order (fst mde) (mde :: modes)) ]
END

VERNAC COMMAND EXTEND ExtractionRelationSingle CLASSIFIED AS SIDEFF
| [ "Extraction" "Relation" "Single" mode(mde) ] ->
  [ msg_info (relation_extraction_single (fst mde) [ mde ]) ]
| [ "Extraction" "Relation" "Single" mode(mde) "with" mode_list(modes) ] ->
  [ msg_info (relation_extraction_single (fst mde) (mde :: modes)) ]
END

VERNAC COMMAND EXTEND ExtractionRelationSingleRelaxed CLASSIFIED AS SIDEFF
| [ "Extraction" "Relation" "Single" "Relaxed" mode(mde) ] ->
  [ msg_info (relation_extraction_single_order (fst mde) [ mde ]) ]
| [ "Extraction" "Relation" "Single" "Relaxed" mode(mde) "with" mode_list(modes) ] ->
  [ msg_info (relation_extraction_single_order (fst mde) (mde :: modes)) ]
END


VERNAC COMMAND EXTEND ExtractionRelationFixpoint CLASSIFIED AS SIDEFF
| [ "Extraction" "Relation" "Fixpoint" mode(mde) ] ->
  [ msg_info (relation_extraction_fixpoint (fst mde) [ mde ]) ]
| [ "Extraction" "Relation" "Fixpoint" mode(mde) "with" mode_list(modes) ] ->
  [ msg_info (relation_extraction_fixpoint (fst mde) (mde :: modes)) ]
END

VERNAC COMMAND EXTEND ExtractionRelationPrint CLASSIFIED AS SIDEFF
| [ "Extraction" "Relation" "Print" string(str) ] ->
  [ extraction_print str ]
END

