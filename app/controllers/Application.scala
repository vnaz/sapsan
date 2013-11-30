package controllers

import play.api.Play.current

import play.api._
import play.api.mvc._

import play.api.libs.json._

import models._

object Application extends Controller {
    
  def grid(tableName :String) = Action {
      Ok( views.html.grid( Schema.tables(tableName).jqGrid ) )
  }
  
  def gridData(tableName :String) = Action {
      val table = Schema.tables(tableName)
      table.loadValues
      Ok( table.jqGrid.getXmlForGrid )
  }
   
  def gridEdit(tableName :String) = Action { request =>
      val table = Schema.tables(tableName)
      val data = request.body.asFormUrlEncoded.getOrElse(Map()).map { case (k, v) => (k, if (v.length>0) { v(0) } ) }
      
      table.selectRecord( data.getOrElse("id", null) )
      println( data.getOrElse("id", null) )
      println( table.curRecord )
      for (col <- data.keys){
          table.setValue(col, data(col))
          table.saveCurrentRecord()
      }
      
      //data.get("oper")
      
      Ok("")
  }
  
}