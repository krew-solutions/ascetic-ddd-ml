(** What the driver said when a statement or a scope boundary failed, as the port sees it:
    the text, and whether the failure is of the moment.

    A failure of the moment is one that trying again may get past: a lock cycle the server
    broke, a connection lost, a server going down or out of resources. Any other failure
    is a defect, of a statement, of the schema, of a value, and repeating it would repeat
    the defect. The backend draws the line, where the driver's error is at hand; a loop
    that meets the error later reads the verdict and waits on the first kind, stops on the
    second. The port names no driver type: the text is what the driver rendered. *)

type t = { text : string; transient : bool }

let transient text = { text; transient = true }
let defect text = { text; transient = false }
let pp ppf error = Format.pp_print_string ppf error.text
let to_string error = error.text
