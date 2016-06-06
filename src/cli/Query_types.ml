
type field = string
type field_name = string
type section_kind = string
type section_name = string

type t =
  | QueryListSections
  | QueryListFields
  | QueryField of field_name
  | QuerySectionField of section_kind * section_name * field_name
