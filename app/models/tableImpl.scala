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
    
    def isModified = (_oldValue.toString != _value.toString)
    
    def getSQLValue :String = _column.getSQLValue(_value)
    def getSQLOldValue :String = _column.getSQLValue(_oldValue)
    def getSQLName = _column.getSQLName
    
    def getColumn = _column
    def getRecord = _record
    
    override def toString() = _value.toString 
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
            if ( _table.columns.contains(key) ){
                val tmp = new Value(_table.column(key), this, value)
                super.put(key, tmp)
            }else{
                Some(value)
            }
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
            if (SQL != ""){
                println(SQL)
                st.execute( SQL )
            }
        }
    }
    
    def getSQLUpdateColumnValuePair = for ( (n, v) <- this if v.isModified) yield (s"${v.getSQLName} = ${v.getSQLValue}" )
    
    def getSQLInsertColumns = for ( (n, v) <- columns) yield ( s"${v.getSQLName}" )
    def getSQLInsertValues  = for ( (n, v) <- columns) yield ( v.getSQLValue( this.getOrElse(n, "") ) )
    
    def getSQLWhere = {
        val tmp = ( for (x <- this.columns.getIdColumns ) yield (x.getSQLName + " = " + this(x.name).getSQLOldValue) ).mkString(" AND ") 
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
    
    def selectRecord(rec :Record)  = { curRecord = rec; curRecord }
    
    def getRecord(id:Any) :Record = {
        mapRecords.getOrElse(id, new Record(_table) )
    }
    
    
    def getRecordKey(rec :Map[String,Any]) :String = {
        val ids = columns.getIdColumns
        if ( ids.forall( c => rec.contains(c.name) ) ){
            (for (c <- ids) yield rec( c.name )).mkString(";")
        }else{
            ""
        }
    }
    
    def getOrCreateRecord(data :Map[String, Any]) :Record = {
        val rec = getRecord( getRecordKey(data) )
        for (col <- data.keys){
            rec.put(col, data(col))
        }
        rec
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
            var tmpRecord :Record = null
            while (rs.next()) {
                tmpRecord = new Record(_table)
                tmpRecord.isNew = false
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
                    tmpRecord(c.name) = new Value(c, tmpRecord, value)
                }
                this += tmpRecord
                mapRecords( getRecordKey( tmpRecord.toMap[String,Value] ) ) = tmpRecord
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
        val tmp = _table.filters.getSQL() 
        if (tmp != "") s"WHERE $tmp" else ""
    }
    def getSelectSQL():String = s"SELECT ${getSQLColumns} FROM ${_table.getSQLTableName} ${this.getSQLWhere} ${getSQLLimit}" 
    
    
}


class Filter(private val _table:Table, val value:Any, val column:Column = null, val operator:String = null){
    def getSQL:String = {
        if (column!=null && operator !=null){
            column.getSQLName + operator + column.getSQLValue(value)
        }else{
            value.toString
        }
    }
}

class Filters(private val _table:Table) extends collection.mutable.MutableList[Filter] {
    
    def columns = _table.columns

    def resetFilters = this.clear
    
    def addFilter(column:String, value:Any, operator:String=null) { 
        if ( columns.contains(column) ){ addFilter( columns(column), value, operator ) }
    }
    
    def addFilter(column:Column, value:Any, operator:String) {
        if (value != null && value.toString != ""){
                this += new Filter(_table, value, column, operator )
        }
    }
    
    def getSQL():String = (for (f <- this) yield (f.getSQL)).mkString(" AND ")
}


class Column(private val _table:Table, val name:String, val isId:Boolean = false, val typeName:String="VARCHAR"){
    _table.columns.addColumn(name, this)
    
    def getSQLName = s"`$name`"
    
    def getSQLValue(arg:Any) :String = arg match { 
        case v:String => "'" + v.replace("'", "\\'") + "'"
        case v:Int    => v.toString
        case _        => getSQLValue(arg.toString)
    }
    
    def getTable = _table
}

class Columns(private val _table:Table) extends collection.mutable.HashMap[String, Column] {
    def getTable = _table
    
    val seqColumns = new collection.mutable.HashMap[Int, Column]()
    val idColumns = new collection.mutable.MutableList[Column]()
    
    def getIdColumns :List[Column] = if (idColumns.length > 0) idColumns.toList else getSortedColumns
    
    private var lastOrd = 0
    
    def addColumn(name :String, column :Column) {
        lastOrd += 1
        this(name) = column 
        seqColumns(lastOrd) = column
        if (column.isId) { idColumns += column }
    }
    
    def getByOrd(ord: Int) = seqColumns(ord) 
    
    def getSortedColumns = for (c <- seqColumns.keys.toList.sorted) yield seqColumns(c)
}



class Table(val tableName:String, val _schema:String = ""){
    
    val jqGrid = new jqGrid(this)
    
    def schema = _schema match { case "" => ""; case _ => _schema + "." }
    
    val columns = new Columns(this)
    def column(ord :Int) = columns.getByOrd(ord)
    def column(name :String) = columns(name)
    
    val records = new Records(this)
    def record: Record = records.curRecord
    
    val filters = new Filters(this)
    
    def setValue(column :String, value :Any) { 
        if (columns.contains(column)){
            record(column) = new Value(columns(column), record, value)
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