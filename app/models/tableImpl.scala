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



class Column(private val _table:Table, val name:String, val isId:Boolean = false, val typeName:String="VARCHAR"){
    _table.addColumn(name, this)
    
    def getSQLName = s"`$name`"
    
    def getSQLValue(arg:Any) :String = arg match { 
        case v:String => s"'$v'".replace("'", "\\'")
        case v:Int    => v.toString
        case _        => getSQLValue(arg.toString)
    }
    
    def getTable = _table
}

class Record(private val _table:Table) extends collection.mutable.HashMap[String, Value] {
    var isNew=true
    var isModified=false
    
    def getTable = _table
    
    def put(key: String, value: Any): Option[Any] = {
        if (this.contains(key)){
            this(key).value = value
            Some(value)
        }else{
            val tmp = new Value(_table.column(key), this, value)
            super.put(key, tmp)
        }
    }
    
    def setValue(column :String, value :Any){
        if (this.contains(column)){
            this(column).update(value)
        }
    }
}

class Table(val tableName:String, val _schema:String = ""){
    
    val jqGrid = new jqGrid(this)
    
    def schema = _schema match { case "" => ""; case _ => _schema + "." }
    
    val mapColumns = collection.mutable.Map[String, Column]()
    val seqColumns = collection.mutable.Map[Int, Column]()
    val idsColumns = collection.mutable.MutableList[Column]()
    
    def column(ord :Int) = seqColumns(ord)
    def column(name :String) = mapColumns(name)
    def columns = mapColumns
    
    private var lastOrd = 0
    def addColumn(name :String, column :Column) {
        lastOrd += 1
        mapColumns(name) = column 
        seqColumns(lastOrd) = column
        if (column.isId) { idsColumns :+ this }
    }
    def getSortedColumns = for (c <- seqColumns.keys.toList.sorted) yield seqColumns(c)
    
    
    val mapRecords = scala.collection.mutable.Map[Any, Record]()
    val seqRecords = collection.mutable.MutableList[Record]()
    var curRecord: Record = null
    
    def records = seqRecords
    def clearRecords() { mapRecords.clear; seqRecords.clear; curRecord = null; }
    
    def selectRecord(id:Any) :Record = {
        curRecord = mapRecords.getOrElse(id, new Record(this) )
        curRecord
    }
    
    def setValue(column :String, value :Any) { 
        if (columns.contains(column)){
            curRecord(column) = new Value(columns(column), curRecord, value)
        }
    }
    
    
    var offset = 0;
    var  limit = 0;
    
    private var filters = collection.mutable.Seq[String]()
    
    def resetFilter { filters = collection.mutable.Seq() }
    def addFilter(colName:String, operator:String, value :Any) { 
        if ( columns.contains(colName) && Seq("=", "!=" , "<>", ">", "<", "IN", "LIKE").contains(operator) ){
            val col = columns(colName)
            filters :+ s"${col.getSQLName} ${operator} ${col.getSQLValue(value)}"
        }
    }
    
    def getSQLTableName = s"$schema$tableName"
    def getSQLColumns = {
        val cols = (for (c <- seqColumns.keys.toList.sorted) yield ( seqColumns(c).getSQLName )).mkString(", ")
        if (cols != "") cols else "*"
    }
    
    def getSQLWhere = { 
        val tmp = ((for (x <- idsColumns) yield (x.getSQLName + " = " + curRecord(x.name).getSQLValue)) ++ filters).mkString(" AND ") 
        if (tmp != "") s"WHERE $tmp" else "" 
    }
    
    def getSQLLimit = if(limit>0) {s"LIMIT $limit" + (if(offset>0) {s" OFFSET $offset"} else "") } else {""} 
    
    def getSQLUpdateColumnValuePair = for ( (n, v) <- curRecord) yield ( if (v.isModified) s"${v.getSQLName} = ${v.getSQLValue}" )
    
    def getSQLInsertColumns = for ( (n, v) <- columns) yield ( s"${v.getSQLName}" )
    def getSQLInsertValues  = for ( (n, v) <- columns) yield ( curRecord.getOrElse(n, "") )
    
    def getSelectSQL():String = s"SELECT ${getSQLColumns} FROM ${getSQLTableName} ${getSQLWhere} ${getSQLLimit}" 
    def getUpdateSQL():String = s"UPDATE ${getSQLTableName} SET ${getSQLUpdateColumnValuePair.mkString(", ")} ${getSQLWhere}"
    def getInsertSQL():String = s"INSERT INTO ${getSQLTableName}(${getSQLInsertColumns.mkString(", ")}) VALUES (${getSQLInsertValues.mkString(", ")})"
    

    
    def saveCurrentRecord(){
        DB.withConnection { conn => 
            val st = conn.createStatement()
            var SQL:String = ""
            if (curRecord.isNew) { SQL = getInsertSQL }
            else if (curRecord.isModified) { SQL = getUpdateSQL }
            println(SQL)
            st.execute( SQL )
        }
    }
    
    def saveRecords(){
        val oldCurRecord = curRecord
        DB.withConnection { conn => 
            val st = conn.createStatement()
            var SQL:String = ""
            for (record <- records){
                
                curRecord = record
                if (curRecord.isNew) { SQL = getInsertSQL }
                else if (curRecord.isModified) { SQL = getUpdateSQL }
                
                println(SQL)
                st.execute( SQL )
            }
            
        }
        curRecord = oldCurRecord 
    }
    
    def loadValues() {
        DB.withConnection { conn => 
            val st = conn.createStatement()
            
            println(getSelectSQL)
            val rs = st.executeQuery( getSelectSQL )
            
            val columns = seqColumns
            if (lastOrd == 0 ) {    
                val md = rs.getMetaData()
                for (i <- 1 to md.getColumnCount()){
                    val isId :Boolean = md.isAutoIncrement(i)
                    
                    new Column(this, md.getColumnName(i), isId, md.getColumnTypeName(i))
                }
            }
            
            var rowNum=0
            while (rs.next()) {
                curRecord = new Record(this)
                for ( (i, c) <- seqColumns) {
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
                seqRecords += curRecord
                mapRecords(rowNum) = curRecord
                rowNum += 1
            }
        }
        
    }
}

class jqGrid(val table :Table) {
    def getXmlForGrid() = {
          <rows>
              <page></page>
              <total></total>
              {
                  for ( row <- table.records ) yield 
                    <row>{
                        for ( column <- table.getSortedColumns) yield {
                        <cell>{
                          new scala.xml.Elem(null, column.name, scala.xml.Null, scala.xml.TopScope, false, scala.xml.Text( row(column.name).value.toString() )) 
                        }</cell>}
                    }</row>
              }
          </rows>
    }
    
    def getColumnNames() = {
        var result = JsArray()
    
        for ( column <- table.getSortedColumns) {
            result = result.append( JsString(column.name) )
        }
        result.toString
    }
    def getColumnModel() = {
        var result = JsArray()
    
        for ( column <- table.getSortedColumns) {
            result = result.append( Json.obj("name" -> column.name, "index" -> column.name, "editable" -> true) )
        }
        result.toString
    }
}