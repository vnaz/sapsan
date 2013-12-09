package models

object Friut extends Table("fruit", "sapsan") {
    val id   = new Column(this, "id", true, "INT")
    val name = new Column(this, "name", false, "VARCHAR")
}

object Schema {
    val tables = collection.mutable.Map[String, Table]("fruit" -> Friut)
}