package xtract.jdbc

trait VendorSpecificFunctions {
  def escape(s: String): String
  def makeSetSchemaCommand(driver: String, schema: String): String
}

object PostgresSpecificFunctions extends VendorSpecificFunctions{
  def escape(s: String) = "\"" + s + "\""
  def makeSetSchemaCommand(driver: String, schema: String): String = s"set search_path to ${escape(schema)}"
}

object H2SpecificFunctions extends VendorSpecificFunctions{
  def escape(s: String) = "\"" + s + "\""
  def makeSetSchemaCommand(driver: String, schema: String) = s"set schema to ${escape(schema)}"

}
