package models

import play.api.db._
import play.api.Play.current

import play.api.libs.json._

class Value (private val _column :Column,  private val _record :Record, private var _value:Any = null){    
    private var _oldValue:Any = _value
    
    def update(new_value:Any) {_value = new_value}
    
    def value = _value
    def value_=(x: Any) { if (x!=_value) {_record.isModified=true; _value = x} }
    def oldValue = _oldValue
    def resetValue(x:Any) = {_value = x; _oldValue = x}
    def rollbackValue() = {_value = _oldValue}
    
    def isModified = (_oldValue != _value)
    
    def getSQLValue :String = _column.getSQLValue(_value)
    def getSQLName = _column.getSQLName
    
    def getColumn = _column
    def getRecord = _record
}

class Record(private val _table:Table) extends collection.mutable.HashMap[String, Value] {
    var isNew=true
    var isModified=false
    
    def getTable = _table
    def columns = _table.columns
    
    def put(key: String, value: Any): Option[Any] = {
        if (this.contains(key)){
            this(key).value = value
            Some(value)
        }else{
            val tmp = new Value(_table.column(key), this, value)
            super.put(key, tmp)
        }
    }
    
//    def setValue(column :String, value :Any){
//        if (this.contains(column)){
//            this(column).update(value)
//        }
//    }
    
    def save(){
        DB.withConnection { conn => 
            val st = conn.createStatement()
            var SQL:String = ""
            if (this.isNew) { SQL = this.getInsertSQL } else if (this.isModified) { SQL = this.getUpdateSQL }
            println(SQL)
            st.execute( SQL )
        }
    }
    
    def getSQLUpdateColumnValuePair = for ( (n, v) <- this) yield ( if (v.isModified) s"${v.getSQLName} = ${v.getSQLValue}" )
    
    def getSQLInsertColumns = for ( (n, v) <- columns) yield ( s"${v.getSQLName}" )
    def getSQLInsertValues  = for ( (n, v) <- columns) yield ( this.getOrElse(n, "") )
    
    def getSQLWhere = { 
        val tmp = (for (x <- this.columns.idColumns) yield (x.getSQLName + " = " + this(x.name).getSQLValue)).mkString(" AND ") 
        if (tmp != "") s"WHERE $tmp" else "" 
    }
    
    def getUpdateSQL():String = s"UPDATE ${_table.getSQLTableName} SET ${this.getSQLUpdateColumnValuePair.mkString(", ")} ${this.getSQLWhere}"
    def getInsertSQL():String = s"INSERT INTO ${_table.getSQLTableName}(${this.getSQLInsertColumns.mkString(", ")}) VALUES (${this.getSQLInsertValues.mkString(", ")})"
    
}

class Records(private val _table:Table) extends collection.mutable.MutableList[Record] {
    
    val mapRecords = scala.collection.mutable.Map[Any, Record]()
    var curRecord :Record = null
    
    var offset = 0;
    var  limit = 0;
    
    def columns = _table.columns
    
    def reset() { mapRecords.clear; this.clear; curRecord = null; }
    
    def selectRecord(id:Any) :Record = {
        curRecord = mapRecords.getOrElse(id, new Record(_table) )
        curRecord
    }
    
    def save() {
        DB.withConnection { conn => 
            val st = conn.createStatement()
            var SQL:String = ""
            for (record <- this){
                
                if (curRecord.isNew) { SQL = record.getInsertSQL }
                else if (curRecord.isModified) { SQL = record.getUpdateSQL }
                
                println(SQL)
                st.execute( SQL )
            }
            
        }
    }
    
    def load() {
        DB.withConnection { conn => 
            val st = conn.createStatement()
            
            println(getSelectSQL)
            val rs = st.executeQuery( getSelectSQL )
            
            if (columns.size == 0 ) {    
                val md = rs.getMetaData()
                for (i <- 1 to md.getColumnCount()){
                    val isId :Boolean = md.isAutoIncrement(i)
                    
                    new Column(_table, md.getColumnName(i), isId, md.getColumnTypeName(i))
                }
            }
            
            var rowNum=0
            while (rs.next()) {
                curRecord = new Record(_table)
                for ( (i, c) <- columns.seqColumns) {
                    val value :Any = c.typeName match {
                        case "DECIMAL"  => rs.getBigDecimal(i)
                        case "INT"      => rs.getInt(i)
                        case "DOUBLE"   => rs.getDouble(i)
                        case "FLOAT"    => rs.getFloat(i)
                        case "DATETIME" => rs.getDate(i)
                        case "VARCHAR"  => rs.getString(i)
                        case _          => rs.getString(i)
                    }
                    
                    curRecord(c.name) = new Value(c, curRecord, value)
                }
                this += curRecord
                mapRecords(rowNum) = curRecord
                rowNum += 1
            }
        }
        
    }
    
    
    def getSQLLimit = if(limit>0) {s"LIMIT $limit" + (if(offset>0) {s" OFFSET $offset"} else "") } else {""} 
    def getSQLColumns = {
        val cols = (for (c <- columns.getSortedColumns) yield (c.getSQLName)).mkString(", ")
        if (cols != "") cols else "*"
    }
    def getSQLWhere = { 
        val tmp = (for ( (n,v) <- this.columns) yield (v.getSQLName + " = " + curRecord(v.name).getSQLValue)).mkString(" AND ") 
        if (tmp != "") s"WHERE $tmp" else "" 
    }
    def getSelectSQL():String = s"SELECT ${getSQLColumns} FROM ${_table.getSQLTableName} ${this.getSQLWhere} ${getSQLLimit}" 
    
    
}



class Column(private val _table:Table, val name:String, val isId:Boolean = false, val typeName:String="VARCHAR"){
    _table.columns.addColumn(name, this)
    
    def getSQLName = s"`$name`"
    
    def getSQLValue(arg:Any) :String = arg match { 
        case v:String => s"'$v'".replace("'", "\\'")
        case v:Int    => v.toString
        case _        => getSQLValue(arg.toString)
    }
    
    def getTable = _table
}

class Columns(private val _table:Table) extends collection.mutable.HashMap[String, Column] {
    def getTable = _table
    
    val seqColumns = new collection.mutable.HashMap[Int, Column]()
    val idColumns = new collection.mutable.MutableList[Column]()
    
    private var lastOrd = 0
    
    def addColumn(name :String, column :Column) {
        lastOrd += 1
        this(name) = column 
        seqColumns(lastOrd) = column
        if (column.isId) { idColumns :+ this }
    }
    
    def getByOrd(ord: Int) = seqColumns(ord) 
    
    def getSortedColumns = for (c <- seqColumns.keys.toList.sorted) yield seqColumns(c)
}



class Table(val tableName:String, val _schema:String = ""){
    
    val jqGrid = new jqGrid(this)
    
    def schema = _schema match { case "" => ""; case _ => _schema + "." }
    
    def columns = new Columns(this)
    def column(ord :Int) = columns.getByOrd(ord)
    def column(name :String) = columns(name)
        
    def records = new Records(this)
    def record: Record = records.curRecord
    
    def setValue(column :String, value :Any) { 
        if (columns.contains(column)){
            record(column) = new Value(columns(column), record, value)
        }
    }
    
    

    
    private var filters = collection.mutable.Seq[String]()
    
    def resetFilter { filters = collection.mutable.Seq() }
    def addFilter(colName:String, operator:String, value :Any) { 
        if ( columns.contains(colName) && Seq("=", "!=" , "<>", ">", "<", "IN", "LIKE").contains(operator) ){
            val col = columns(colName)
            filters :+ s"${col.getSQLName} ${operator} ${col.getSQLValue(value)}"
        }
    }
    
    def getSQLTableName = s"$schema$tableName"
}



class jqGrid(val table :Table) {
    def getXmlForGrid() = {
          <rows>
              <page></page>
              <total></total>
              {
                  for ( row <- table.records ) yield 
                    <row>{
                        for ( column <- table.columns.getSortedColumns) yield {
                        <cell>{
                          new scala.xml.Elem(null, column.name, scala.xml.Null, scala.xml.TopScope, false, scala.xml.Text( row(column.name).value.toString() )) 
                        }</cell>}
                    }</row>
              }
          </rows>
    }
    
    def getColumnNames() = {
        var result = JsArray()
    
        for ( column <- table.columns.getSortedColumns) {
            result = result.append( JsString(column.name) )
        }
        result.toString
    }
    def getColumnModel() = {
        var result = JsArray()
    
        for ( column <- table.columns.getSortedColumns) {
            result = result.append( Json.obj("name" -> column.name, "index" -> column.name, "editable" -> true) )
        }
        result.toString
    }
}