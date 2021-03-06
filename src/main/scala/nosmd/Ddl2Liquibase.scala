package nosmd

import scala.io.Source
import scala.util.parsing.combinator.syntactical.StandardTokenParsers


object Ddl2Liquibase extends StandardTokenParsers {
  lexical.reserved += ("create", "table", "CREATE",  "TABLE", "varchar", "VARCHAR", "varchar2", "VARCHAR2",
    "number", "NUMBER", "date", "DATE", "blob", "BLOB", "PRIMARY", "KEY", "FOREIGN", "REFERENCES")
  lexical.delimiters += ("(", ");", ")", ",")
  val s = """
CREATE TABLE PARTY (
	id VARCHAR2(36) PRIMARY KEY,
	crmguid blob,
	name date,
	label VARCHAR2(256),
	status number(32,0),
	countryIsoCode VARCHAR2(2),
	orgid VARCHAR2(24),
	user_fk VARCHAR2(24),
	FOREIGN KEY(user_fk) REFERENCES USER(id),
  PRIMARY KEY(role_fk,opset_fk)
,);
    """

  def main(args: Array[String]) {
    val input = Source.fromFile(args(0)).getLines.reduceLeft[String](_ + '\n' + _)
    val tokens = new lexical.Scanner(input)

    val result = phrase(program)(tokens)
    result match {
      case Success(tree, _) => new Interpreter(tree).run()

      case e: NoSuccess => {
        Console.err.println(e)
        exit(100)
      }
    }
  }

  def program = table+

  def table = ( "CREATE" ~ "TABLE" ~ ident ~ "(" ~ (line+) ~ ");" ^^ { case _ ~ _ ~ n ~ _ ~ l ~_ => Table(n, l) } )

  def line = ( ident ~ stmtWVar ~ (primKey*) ~ "," ^^ { case n ~ t ~ pks ~ _ => Line(n, t, pks) }
             | ident ~ stmtNVar ~ (primKey*) ~ "," ^^ { case n ~ t ~ pks ~ _ => Line(n, t, pks) }
             | "FOREIGN" ~ "KEY" ~ "(" ~ ident ~ ")" ~ "REFERENCES" ~ ident ~ "(" ~ ident ~ ")" ~ "," ^^ {
                  case _ ~ _ ~ _ ~ lc ~ _ ~ _ ~ rt ~ _ ~ rc ~ _ ~ _ => FkLine(lc, rt, rc) }
             | "PRIMARY" ~ "KEY" ~ "(" ~ ident ~ "," ~ ident ~ ")" ~ "," ^^ {
                case _ ~ _ ~ _ ~ pk1 ~ _ ~ pk2 ~ _ ~ _ => PkLine(pk1, pk2) }
    )


  def stmtWVar = ( "VARCHAR2" ~ "(" ~ numericLit ~ ")" ^^ { case _ ~ _ ~ l ~ _ => Varchar(l) }
                 | "number" ~ "(" ~ numericLit ~ "," ~ numericLit ~ ")" ^^ { case _ ~ _ ~ l ~ _ ~ c ~ _ => Number(l, c) } )
  def stmtNVar = ( "blob" ^^^ Blob()
                 | "date" ^^^ Date() )
  def primKey = ( "PRIMARY" ~ "KEY" ^^^ PrimaryKey())
}

class Interpreter(tree: List[Statement]) {
  def run() {
    walkTree("", tree)
  }

  private def walkTree(tableName: String, tree: List[Statement]) {
    tree match {
      case Table(tableName, lines) :: rest => {
        val lp = lines.partition(_.isInstanceOf[Line])
        println("<createTable tableName=\"" + tableName + "\">")
        walkTree(tableName, lp._1)
        println("</createTable>")
        walkTree(tableName, lp._2)
        walkTree(tableName, rest)
      }

      case Line(colName, colType, pks) :: rest => {
        print("<column name=\"" + colName + "\" type=\"" + colType.text + "\"")

        if (pks.size == 0) {
          println("/>")
        } else {
          println(">")
          for (pk <- pks) println("<constraints primaryKey=\"true\" />")
          println("</column>")
        }

        walkTree(tableName, rest)
      }

      case FkLine(localCol, refTable, refCol) :: rest => {
        println("<addForeignKeyConstraint baseTableName=\"" + tableName + "\" baseColumnNames=\"" + localCol +
          "\" referencedTableName=\"" + refTable + "\" referencedColumnNames=\"" + refCol + "\"/>")
        walkTree(tableName, rest)
      }

      case PkLine(pk1, pk2) :: rest => {
        println("<addPrimaryKey tableName=\"" + tableName + "\" columnNames=\"" + pk1 + "," + pk2 + "\"/>")
        walkTree(tableName, rest)
      }

      case Nil => ()
    }
  }
}

sealed abstract class Statement

case class Table(tableName: String, lines: List[Statement]) extends Statement
case class Line(colName: String, colType: Type, pks: List[PrimaryKey]) extends Statement
case class FkLine(localCol: String, refTable: String, refCol: String) extends Statement
case class PkLine(pk1: String, pk2: String) extends Statement
case class PrimaryKey() extends Statement

sealed abstract class Type {
  val text: String
}

case class Blob() extends Type {
  override val text = "BLOB"
}
case class Date() extends Type {
  override val text = "DATE"
}
case class Varchar(len: String) extends Type {
  override val text = "VARCHAR(" + len + ")"
}
case class Number(len: String, cm: String) extends Type {
  override val text = "NUMBER(" + len + "," + cm + ")"
}