(** Parser and printer for ini files. Ini files are stored in memory
    in nested string maps, to ensure that keys and sections are always
    sorted, and to easily handle duplication: if an ini file has
    duplicated keys or sections, the latest ones are used. *)

module SM : Map.S with type key = string

type t = string SM.t SM.t
(** Memory representation of an ini file: a nested string map. The
    outer string map index the sections, where the string "" maps to
    the empty section (no section), and the corresponding keys are
    string maps of pairs (keys). *)

type simple = (string * ((string * string) list)) list
(** A simpler type for easier manipulation, that can be converted to
    and from [t]. *)

val of_simple : simple -> t
(** [of_simple simple] is the ini file represented by the simple form
    [simple]. *)

val to_simple : t -> simple
(** [to_simple ini] is the simple form of [ini]. *)

val of_channel : in_channel -> t
(** [of_channel ic] is the memory representation of the ini file in
    [oc]. *)

val to_channel : out_channel -> t -> unit
(** [to_channel oc ini] writes [ini] in [oc]. *)
