package logql.parser.AST

sealed trait LabelFormat
case class RenameLabelFormat(oldName: String, newName: String) extends LabelFormat
case class TemplateLabelFormat(name: String, template: String) extends LabelFormat
